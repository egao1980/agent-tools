(in-package #:agent-tools)

;;; Runs and jobs (H2c). Every bash call is a %JOB: a SHELL-JOB whose id is
;;; NIL until it is registered (run_in_background, or the RUN-IN-BACKGROUND
;;; restart of a foreground call). Per job:
;;;
;;; - a pump thread, the only thread that touches the job's pty or process
;;;   handle (I-owner). Other threads post (:WRITE octets box) / (:KILL) to
;;;   the job's MAILBOX and wait on its condition variable;
;;; - on the :PROCESS executor also a reader thread that blocks on the
;;;   merged stdout pipe. It is never joined: a grandchild may hold the pipe
;;;   open (process-protocol#9), and the job finishes without it;
;;; - TEXT[START, end): output decoded as it arrives (UTF-8, a ≤3-byte CARRY
;;;   for a split sequence) and not read yet; cleaned when taken. More than
;;;   CAP characters unread drops the oldest, cut forward to a line start, and
;;;   counts them in DROPPED.
;;;
;;; The call returns when the shell has exited and its output reached EOF or
;;; stayed quiet for +DRAIN-QUIET+ seconds, so a background child keeping the
;;; terminal or pipe open does not hold it up.

(defparameter +drain-quiet+ 0.2
  "Seconds of silence after the shell exited that end its output.")

(defparameter +pty-read-timeout+ 0.05
  "Seconds one pump read waits; bounds the latency of job_input and job_kill.")

(defparameter +kill-wait+ 10
  "Seconds job_kill waits for a killed job to finish.")

(defun %now ()
  (/ (float (get-internal-real-time) 1d0) internal-time-units-per-second))

(defclass %job (shell-job)
  ((tools :initarg :tools :reader %job-tools)
   (executor :initarg :executor :reader %job-executor)
   (handle :initform nil :accessor %job-handle
           :documentation "The PTY or PROCESS-HANDLE; the pump owns it.")
   (lock :initform (bt2:make-lock :name "shell-job") :reader %job-lock)
   (cv :initform (bt2:make-condition-variable :name "shell-job") :reader %job-cv)
   (text :initform (make-array 256 :element-type 'character :adjustable t :fill-pointer 0)
         :reader %job-text)
   (start :initform 0 :accessor %job-start)
   (dropped :initform 0 :accessor %job-dropped)
   (cap :initarg :cap :reader %job-cap)
   (carry :initform nil :accessor %job-carry)
   (mailbox :initform '() :accessor %job-mailbox)
   (exited :initform nil :accessor %job-exited
           :documentation "The shell exited (before any kill took effect).")
   (killed :initform nil :accessor %job-killed
           :documentation "A kill reached the job while it still ran.")
   (eof :initform nil :accessor %job-eof)
   (last-data :initform (%now) :accessor %job-last-data)
   (finished :initform nil :accessor %job-finished))
  (:documentation "A command run by bash: foreground until registered."))

;;; --- buffer (call with the job lock held) -----------------------------------

(defun %job-push-text (job string)
  (let ((text (%job-text job)))
    (loop for c across string do (vector-push-extend c text (max 256 (length string))))
    (let ((cap (%job-cap job))
          (end (fill-pointer text)))
      (when (> (- end (%job-start job)) cap)
        (let* ((cut (- end cap))
               (lf (position #\Newline text :start cut :end end)))
          (when (and lf (< (1+ lf) end))
            (setf cut (1+ lf)))
          (incf (%job-dropped job) (- cut (%job-start job)))
          (setf (%job-start job) cut)))
      ;; Compact once most of the vector is dropped or read.
      (let ((start (%job-start job)))
        (when (and (> start 4096) (> start (floor (fill-pointer text) 2)))
          (replace text text :start2 start)
          (setf (fill-pointer text) (- (fill-pointer text) start)
                (%job-start job) 0))))))

(defun %job-append (job octets)
  "Decode OCTETS (output of the job) into its buffer."
  (bt2:with-lock-held ((%job-lock job))
    (unless (%job-finished job)
      (let ((bytes (if (%job-carry job)
                       (concatenate '(vector (unsigned-byte 8)) (%job-carry job) octets)
                       octets)))
        (multiple-value-bind (string used) (%utf8-decode-lenient bytes :partial t)
          (setf (%job-carry job) (and (< used (length bytes)) (subseq bytes used)))
          (%job-push-text job string)))
      (setf (%job-last-data job) (%now))
      (bt2:condition-broadcast (%job-cv job)))))

(defun %status-line (job)
  (let ((id (shell-job-id job)))
    (ecase (shell-job-status job)
      (:running (format nil "[job ~a running]" id))
      (:killed (format nil "[job ~a killed]" id))
      (:exited (if (shell-job-signal job)
                   (format nil "[job ~a terminated by signal ~a]" id (%signal-name (shell-job-signal job)))
                   (format nil "[job ~a exited with code ~a]" id (shell-job-exit-code job)))))))

(defun %job-take (job &key peek)
  "→ (values unread-text dropped status-line), consuming the text unless
PEEK. While the job runs, an unfinished escape sequence or a final CR stays
for the next take."
  (bt2:with-lock-held ((%job-lock job))
    (let* ((text (%job-text job))
           (start (%job-start job))
           (end (if (%job-finished job)
                    (fill-pointer text)
                    (%take-end text start (fill-pointer text))))
           (chunk (subseq text start end))
           (dropped (%job-dropped job)))
      (unless peek
        (setf (%job-start job) end
              (%job-dropped job) 0))
      (values chunk dropped (%status-line job)))))

(defun %output-text (tools chunk dropped)
  "CHUNK cleaned, after a dropped-output notice when DROPPED > 0, capped at
the tools' max-output, final newlines removed."
  (let* ((clean (clean-shell-output chunk))
         (text (if (plusp dropped)
                   (format nil "[truncated: ~d characters dropped; read job_output more often or redirect the output to a file]~%~a"
                           dropped clean)
                   clean)))
    (string-right-trim '(#\Newline) (truncate-shell-output text (shell-tools-max-output tools)))))

(defun %with-status (output status)
  (if (zerop (length output))
      status
      (format nil "~a~%~a" output status)))

(defun %signal-name (signal)
  (if (symbolp signal) (symbol-name signal) (princ-to-string signal)))

;;; --- waiting and requests ------------------------------------------------------

(defun %wait-for (job predicate seconds)
  "Wait up to SECONDS for PREDICATE (called with the job lock held) to be
true. → its value, or NIL on timeout."
  (let ((deadline (+ (%now) seconds)))
    (bt2:with-lock-held ((%job-lock job))
      (loop
        (let ((v (funcall predicate)))
          (when v (return v)))
        (let ((left (- deadline (%now))))
          (when (<= left 0) (return nil))
          (bt2:condition-wait (%job-cv job) (%job-lock job) :timeout (min left 0.5d0)))))))

(defun %wait-finished (job seconds)
  (%wait-for job (lambda () (%job-finished job)) seconds))

(defun %post (job request)
  "Queue REQUEST for the pump. → NIL when the job already finished."
  (bt2:with-lock-held ((%job-lock job))
    (unless (%job-finished job)
      (setf (%job-mailbox job) (append (%job-mailbox job) (list request)))
      (bt2:condition-broadcast (%job-cv job))
      t)))

(defun %take-requests (job)
  (bt2:with-lock-held ((%job-lock job))
    (shiftf (%job-mailbox job) '())))

(defun %kill-job (job seconds)
  "Kill JOB (its process tree on the pty) and wait up to SECONDS for it to
finish. → true when it has finished."
  (%post job (list :kill))
  (%wait-finished job seconds))

(defun %job-write (job octets)
  "Write OCTETS to the job's stdin. → NIL when the job finished first."
  (let ((box (list nil)))
    (and (%post job (list :write octets box))
         (%wait-for job (lambda () (or (car box) (%job-finished job))) 10)
         (car box))))

(defun %request-done (job box)
  (bt2:with-lock-held ((%job-lock job))
    (setf (car box) t)
    (bt2:condition-broadcast (%job-cv job))))

;;; --- finishing ---------------------------------------------------------------

(defun %record-exit (job code signal)
  (bt2:with-lock-held ((%job-lock job))
    (unless (%job-killed job)
      (setf (%job-exited job) t
            (slot-value job 'exit-code) (and (null signal) code)
            (slot-value job 'signal) signal))))

(defun %mark-killed (job)
  (bt2:with-lock-held ((%job-lock job))
    (unless (%job-exited job)
      (setf (%job-killed job) t))))

(defun %close-handle (job)
  "Free the job's handle (pump thread only). On the pty this hangs up the
terminal: HUP, then KILL, then the master is closed."
  (let ((h (%job-handle job)))
    (when h
      (ecase (%job-executor job)
        (:pty (pty:pty-close h))
        (:process
         (when (proc:alive-p h)
           (proc:kill h :force t))
         (let ((in (proc:stdin h)))
           (when (and in (open-stream-p in))
             (handler-case (close in :abort t)
               (stream-error () nil)))))))))

(defun %finish (job)
  "Mark JOB finished (idempotent), free its handle and take it off the
tools' live set. Called by the pump thread."
  (let ((first nil))
    (bt2:with-lock-held ((%job-lock job))
      (unless (%job-finished job)
        (setf first t)
        (let ((carry (shiftf (%job-carry job) nil)))
          (when carry
            (%job-push-text job (make-string (length carry) :initial-element +replacement-char+))))
        (setf (slot-value job 'status) (if (%job-killed job) :killed :exited)
              (%job-finished job) t
              (%job-mailbox job) '())
        (when (%job-killed job)
          (setf (slot-value job 'exit-code) nil
                (slot-value job 'signal) nil))))
    (when first
      (unwind-protect (%close-handle job)
        (let ((tools (%job-tools job)))
          (bt2:with-lock-held ((%shell-lock tools))
            (setf (%shell-live tools) (remove job (%shell-live tools)))))
        (bt2:with-lock-held ((%job-lock job))
          (bt2:condition-broadcast (%job-cv job)))))))

;;; --- pumps ---------------------------------------------------------------------

(defun %quiet-since-exit-p (job exit-time)
  (> (- (%now) (max exit-time (bt2:with-lock-held ((%job-lock job)) (%job-last-data job))))
     +drain-quiet+))

(defun %pump-pty (job)
  (let ((pty (%job-handle job)) (eof nil) (exit-time nil))
    (loop
      (dolist (r (%take-requests job))
        (ecase (first r)
          (:write
           (handler-bind ((pty:pty-io-error (lambda (c) (continue c))))
             (pty:pty-write pty (second r)))
           (%request-done job (third r)))
          (:kill
           (%mark-killed job)
           (return-from %pump-pty))))
      (unless eof
        (let ((data (handler-bind ((pty:pty-io-error (lambda (c) (setf eof t) (continue c))))
                      (pty:pty-read pty :timeout +pty-read-timeout+))))
          (cond ((null data) (setf eof t))
                ((plusp (length data)) (%job-append job data)))))
      (unless exit-time
        (multiple-value-bind (code signal)
            (handler-bind ((pty:pty-io-error (lambda (c) (continue c))))
              (pty:pty-wait pty :timeout (if eof +pty-read-timeout+ 0)))
          (when (or code signal)
            (setf exit-time (%now))
            (%record-exit job code signal))))
      (when (and exit-time (or eof (%quiet-since-exit-p job exit-time)))
        (return)))))

(defun %read-process-output (job stream)
  "Reader thread body: append STREAM's octets to JOB until EOF."
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
    (unwind-protect
         (handler-case
             (loop
               (let ((b (read-byte stream nil nil)))
                 (unless b (return))
                 (setf (aref buffer 0) b)
                 (let ((n 1))
                   (loop while (and (< n (length buffer)) (listen stream))
                         do (let ((b (read-byte stream nil nil)))
                              (unless b (loop-finish))
                              (setf (aref buffer n) b)
                              (incf n)))
                   (%job-append job (subseq buffer 0 n)))))
           (stream-error () nil))
      (bt2:with-lock-held ((%job-lock job))
        (setf (%job-eof job) t)
        (bt2:condition-broadcast (%job-cv job)))
      (handler-case (close stream :abort t)
        (stream-error () nil)))))

(defun %pump-process (job)
  (let* ((h (%job-handle job))
         (exit-time nil))
    (bt2:make-thread (lambda () (%read-process-output job (proc:stdout h)))
                     :name "agent-tools shell output")
    (loop
      (dolist (r (%take-requests job))
        (ecase (first r)
          (:write
           (let ((in (proc:stdin h)))
             (handler-case (progn (write-sequence (second r) in)
                                  (finish-output in))
               (stream-error () nil)))
           (%request-done job (third r)))
          (:kill
           (%mark-killed job)
           (when (proc:alive-p h)
             (proc:kill h :force t)))))
      (unless exit-time
        (let ((code (proc:exit-code h)))
          (when code
            (setf exit-time (%now))
            (%record-exit job code nil))))
      (when (and exit-time
                 (or (bt2:with-lock-held ((%job-lock job)) (%job-eof job))
                     (%quiet-since-exit-p job exit-time)))
        (return))
      (bt2:with-lock-held ((%job-lock job))
        (unless (%job-mailbox job)
          (bt2:condition-wait (%job-cv job) (%job-lock job) :timeout 0.02d0))))))

(defun %run-pump (job)
  "Pump thread body. Whatever happens, the job ends finished."
  (unwind-protect
       (ecase (%job-executor job)
         (:pty (%pump-pty job))
         (:process (%pump-process job)))
    (%finish job)))

;;; --- spawning ------------------------------------------------------------------

(defun %shell-overrides (tools)
  "+SHELL-ENVIRONMENT+ then the tools' :ENV, one entry per name (later wins);
a NIL value unsets."
  (let ((out '()))
    (dolist (pair (append +shell-environment+ (shell-tools-env tools)) (nreverse out))
      (setf out (cons (cons (car pair) (cdr pair))
                      (remove (car pair) out :key #'car :test #'string=))))))

(defun %cannot-start (tools reason)
  (error 'agent:tool-unavailable
         :message (format nil "the shell ~a cannot start: ~a"
                          (first (shell-tools-shell tools)) reason)))

(defun %spawn-handle (tools argv directory)
  (let ((overrides (%shell-overrides tools)))
    (handler-case
        (ecase (shell-tools-executor tools)
          (:pty
           (let ((env (remove-if (lambda (e)
                                   (let ((o (assoc (car e) overrides :test #'string=)))
                                     (and o (null (cdr o)))))
                                 (pty:merge-environment (remove nil overrides :key #'cdr)
                                                        :inherit t))))
             (pty:spawn-pty argv :directory directory :environment env
                                 :inherit-environment nil :cols 512 :rows 50)))
          (:process
           (proc:launch argv :input nil :output nil :error-output :output
                             :element-type :octets :directory directory
                             :env overrides :env-mode :merge)))
      (pty:pty-spawn-error (c) (%cannot-start tools c))
      (proc:process-error (c) (%cannot-start tools c)))))

(defun %spawn-job (tools command directory &key foreground)
  "Start COMMAND in DIRECTORY (a native directory string). → a live %JOB
with its pump running. The shell not starting, or TOOLS closed, signals
TOOL-UNAVAILABLE."
  (let* ((program (first (shell-tools-shell tools)))
         (executor (shell-tools-executor tools))
         (posix-eof (and foreground (eq executor :pty)
                         (eq (shell-tools-dialect tools) :posix)))
         (argv (append (shell-tools-shell tools)
                       (list (if posix-eof
                                 ;; Same line: $LINENO and messages keep their numbers.
                                 (concatenate 'string "exec </dev/null; " command)
                                 command))))
         (job (make-instance '%job :tools tools :executor executor :id nil
                                   :command command :cap (shell-tools-max-job-buffer tools))))
    (when (and (find-if (lambda (c) (find c "/\\")) program)
               (null (uiop:probe-file* (uiop:parse-native-namestring program))))
      (%cannot-start tools "no such file"))
    (setf (%job-handle job) (%spawn-handle tools argv directory))
    (when (bt2:with-lock-held ((%shell-lock tools))
            (if (shell-tools-closed-p tools)
                t
                (progn (push job (%shell-live tools)) nil)))
      ;; Closed while it started: no pump runs yet, so this thread owns it.
      (%close-handle job)
      (%closed-error "bash"))
    (when (and foreground (eq executor :process))
      (handler-case (close (proc:stdin (%job-handle job)))
        (stream-error () nil)))
    (bt2:make-thread (lambda () (%run-pump job)) :name "agent-tools shell job")
    (when (and foreground (eq executor :pty) (not posix-eof))
      ;; ^D at the start of a line: end of file for a reader on the terminal.
      (%post job (list :write (make-array 1 :element-type '(unsigned-byte 8)
                                            :initial-element 4)
                       (list nil))))
    job))

(defun %register-job (tools job &key check-limit reservation)
  "Give JOB the next id and list it. CHECK-LIMIT refuses at max-jobs.
RESERVATION: a cell whose car is true while this start holds a max-jobs
reservation; it is released (car set to NIL) first."
  (bt2:with-lock-held ((%shell-lock tools))
    (when (and reservation (car reservation))
      (decf (%shell-reserved tools))
      (setf (car reservation) nil))
    (when (shell-tools-closed-p tools)
      (%closed-error "bash"))
    (when check-limit
      (%check-job-limit tools))
    (let ((id (format nil "job-~d" (incf (%shell-counter tools)))))
      (setf (slot-value job 'id) id)
      (push job (%shell-jobs tools))
      job)))

(defun %check-job-limit (tools)
  "With the tools lock held."
  (let ((running (+ (%shell-reserved tools)
                    (count :running (%shell-jobs tools) :key #'shell-job-status))))
    (when (>= running (shell-tools-max-jobs tools))
      (%input-error "~d background job~:p already running (max-jobs ~d); stop one with job_kill or wait for it to exit"
                    running (shell-tools-max-jobs tools)))))

(defun %started-text (job)
  (format nil "Started job ~a: ~a~%Read its output with job_output (job_id \"~a\"); stop it with job_kill."
          (shell-job-id job) (shell-job-command job) (shell-job-id job)))

;;; --- arguments ---------------------------------------------------------------

(defun %ranged-arg (args key low high default)
  "Integer argument KEY in LOW…HIGH; absent or null → DEFAULT."
  (multiple-value-bind (v present) (gethash key args)
    (cond ((or (not present) (eq v :null)) default)
          ((and (integerp v) (<= low v high)) v)
          (t (%input-error "~a must be an integer from ~d to ~d, got ~a"
                           key low high (%describe-value v))))))

(defun %native-directory (path)
  "PATH (a local pathlib path) as the OS names it: C:\\dir\\ on Windows, where
the POSIX form /C:/dir/ is not a usable directory, and /dir/ elsewhere."
  (uiop:native-namestring (uiop:ensure-directory-pathname (fs:path-pathname path))))

(defun %workdir-directory (tools workdir)
  "The native directory a command with WORKDIR (NIL: the root) starts in."
  (if (null workdir)
      (%native-directory (shell-tools-root tools))
      (let ((path (resolve-tool-path (%shell-files tools) workdir)))
        (unless (fs:exists-p path)
          (%input-error "workdir ~a not found; give an existing directory relative to the project root"
                        workdir))
        (unless (fs:directory-p path)
          (%input-error "workdir ~a is not a directory; give a directory relative to the project root"
                        workdir))
        (%native-directory path))))

(defun %find-job (tools id)
  (or (find-shell-job tools id)
      (error 'job-not-found
             :id id
             :message (format nil "there is no job ~a on this source; job_list shows the jobs" id))))

(defun %utf8-encode (string)
  (let ((out (make-array (length string) :element-type '(unsigned-byte 8)
                                         :adjustable t :fill-pointer 0)))
    (flet ((put (b) (vector-push-extend b out)))
      (loop for c across string
            for k = (char-code c)
            do (cond ((< k #x80) (put k))
                     ((< k #x800)
                      (put (logior #xC0 (ash k -6)))
                      (put (logior #x80 (logand k #x3F))))
                     ((< k #x10000)
                      (put (logior #xE0 (ash k -12)))
                      (put (logior #x80 (logand (ash k -6) #x3F)))
                      (put (logior #x80 (logand k #x3F))))
                     (t
                      (put (logior #xF0 (ash k -18)))
                      (put (logior #x80 (logand (ash k -12) #x3F)))
                      (put (logior #x80 (logand (ash k -6) #x3F)))
                      (put (logior #x80 (logand k #x3F)))))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

;;; --- tools -------------------------------------------------------------------

(defun %exit-line (job)
  (cond ((shell-job-signal job)
         (format nil "[killed by signal ~a]" (%signal-name (shell-job-signal job))))
        (t (format nil "[exit code ~a]" (shell-job-exit-code job)))))

(defun %timeout-message (timeout-ms output)
  (format nil "Command timed out after ~d ms and was killed; output so far:~%~a~%Run long commands with run_in_background: true and read them with job_output."
          timeout-ms (if (zerop (length output)) "(no output)" output)))

(defun %bash-foreground (tools command directory timeout-ms)
  (let ((job (%spawn-job tools command directory :foreground t))
        (kept nil))
    (unwind-protect
         (loop
           (when (%wait-finished job (/ timeout-ms 1000d0))
             (when (eq (shell-job-status job) :killed)
               ;; Only CLOSE-SHELL-TOOLS kills a foreground run.
               (%closed-error "bash"))
             (return (multiple-value-bind (chunk dropped) (%job-take job)
                       (%with-status (%output-text tools chunk dropped) (%exit-line job)))))
           ;; I-restart: signalled here, in the caller's thread, with the
           ;; command still running; the restarts recover in this frame.
           (restart-case
               (let ((so-far (multiple-value-bind (chunk dropped) (%job-take job :peek t)
                               (%output-text tools chunk dropped))))
                 (error 'shell-timeout :command command :timeout timeout-ms :output so-far
                                       :message (%timeout-message timeout-ms so-far)))
             (continue ()
               :report (lambda (s) (format s "Wait another ~d ms for the command." timeout-ms))
               nil)
             (run-in-background ()
               :report "Keep the command running as a background job; the call returns its job id."
               (%register-job tools job :check-limit t)
               (setf kept t)
               (return (%started-text job)))))
      ;; I-kill.
      (unless kept
        (%kill-job job 5)))))

(defun %bash-background (tools command directory)
  (bt2:with-lock-held ((%shell-lock tools))
    (%check-job-limit tools)
    (incf (%shell-reserved tools)))
  (let ((job nil) (reservation (list t)) (registered nil))
    (unwind-protect
         (progn
           (setf job (%spawn-job tools command directory))
           (%register-job tools job :reservation reservation)
           (setf registered t)
           (%started-text job))
      (when (car reservation)
        (bt2:with-lock-held ((%shell-lock tools))
          (decf (%shell-reserved tools))))
      (when (and job (not registered))
        (%kill-job job 5)))))

(defun %tool-bash (tools args)
  (let* ((command (%arg args "command" :string :required t))
         (timeout (%ranged-arg args "timeout_ms" 1 (shell-tools-max-timeout tools)
                               (shell-tools-default-timeout tools)))
         (workdir (%arg args "workdir" :string))
         (background (%arg args "run_in_background" :boolean)))
    (%arg args "description" :string)       ; accepted, unused (UI text)
    (when (%blank-p command)
      (%input-error "command is empty; give the command line to run"))
    (let ((directory (%workdir-directory tools workdir)))
      (if background
          (%bash-background tools command directory)
          (%bash-foreground tools command directory timeout)))))

(defun %job-report (tools job wait-ms)
  "What job_output returns for JOB after waiting up to WAIT-MS for it."
  (when (plusp wait-ms)
    (%wait-finished job (/ wait-ms 1000d0)))
  (multiple-value-bind (chunk dropped status) (%job-take job)
    (%with-status (%output-text tools chunk dropped) status)))

(defun %tool-job-output (tools args)
  (let* ((id (%arg args "job_id" :string :required t))
         (wait (%ranged-arg args "wait_ms" 0 (shell-tools-max-timeout tools) 0)))
    (%job-report tools (%find-job tools id) wait)))

(defun %not-running (job)
  (error 'job-not-running
         :id (shell-job-id job)
         :status (shell-job-status job)
         :message (format nil "job ~a is not running (~(~a~)); start a new command with bash"
                          (shell-job-id job) (shell-job-status job))))

(defun %tool-job-input (tools args)
  (let* ((id (%arg args "job_id" :string :required t))
         (input (%arg args "input" :string :required t))
         (wait (%ranged-arg args "wait_ms" 0 (shell-tools-max-timeout tools) 0))
         (job (%find-job tools id)))
    (unless (eq (shell-job-status job) :running)
      (%not-running job))
    (unless (%job-write job (%utf8-encode input))
      (%not-running job))
    (%job-report tools job wait)))

(defun %tool-job-kill (tools args)
  (let* ((id (%arg args "job_id" :string :required t))
         (job (%find-job tools id)))
    (when (eq (shell-job-status job) :running)
      (%kill-job job +kill-wait+))
    (%job-report tools job 0)))

(defun %job-list-status (job)
  (ecase (shell-job-status job)
    (:running "running")
    (:killed "killed")
    (:exited (if (shell-job-signal job)
                 (format nil "signal ~a" (%signal-name (shell-job-signal job)))
                 (format nil "exited ~a" (shell-job-exit-code job))))))

(defun %tool-job-list (tools args)
  (declare (ignore args))
  (let ((jobs (shell-tools-jobs tools)))
    (if (null jobs)
        "[no jobs]"
        (format nil "~{~a~^~%~}"
                (mapcar (lambda (j)
                          (format nil "~a~c~a~c~a" (shell-job-id j) #\Tab (%job-list-status j)
                                  #\Tab (shell-job-command j)))
                        jobs)))))
