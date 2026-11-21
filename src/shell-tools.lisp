(in-package #:agent-tools)

;;; H2c contract: SHELL-TOOLS runs shell commands for the coding agent:
;;; bash (foreground with a timeout, or a background job), job_output,
;;; job_input, job_kill and job_list. Unix runs commands on a pty
;;; (pty-protocol, pty-backend-native); Windows, and :EXECUTOR :PROCESS
;;; anywhere, through process-protocol pipes. SHELL-COMMAND-RESOURCES turns a
;;; command line into permission resources.
;;;
;;; This file: the classes, the constructor and the facade (dispatch,
;;; close, the job list). Runs, jobs and the tool handlers are in
;;; src/shell-run.lisp; cleaning and truncation in src/shell-output.lisp;
;;; the scanner in src/shell-scan.lisp.
;;;
;;; Invariants:
;;; - I-owner: a job's pty or process handle is touched only by that job's
;;;   pump thread; other threads post requests to the job's mailbox.
;;; - I-ids: a job id exists iff a command spawned and was registered; ids
;;;   count from 1 with no gaps (a refused start takes none).
;;; - I-kill: when control leaves a foreground bash call other than through
;;;   RUN-IN-BACKGROUND, its command is killed before the call returns.
;;; - I-restart: SHELL-TIMEOUT is signalled inside RESTART-CASE in the
;;;   calling thread and nothing between the signal and the caller unwinds.

(defparameter +shell-tool-names+
  '("bash" "job_output" "job_input" "job_kill" "job_list")
  "Every tool a SHELL-TOOLS source can expose, in LIST-AGENT-TOOLS order.")

(defparameter +shell-environment+
  '(("TERM" . "dumb") ("PAGER" . "cat") ("GIT_PAGER" . "cat")
    ("NO_COLOR" . "1") ("GIT_TERMINAL_PROMPT" . "0"))
  "Variables every command gets on top of the parent's environment, before
the source's own :ENV. They keep pagers, colours and credential prompts out
of non-interactive runs.")

(defun default-shell ()
  "→ the shell argv prefix used when MAKE-SHELL-TOOLS gets no :SHELL: the
command line is appended as one last argument. (\"/bin/sh\" \"-c\") on Unix,
(\"cmd.exe\" \"/d\" \"/s\" \"/c\") on Windows."
  #+(or windows win32) (list "cmd.exe" "/d" "/s" "/c")
  #-(or windows win32) (list "/bin/sh" "-c"))

(defun shell-dialect (shell)
  "→ the scanner dialect of SHELL (an argv prefix, a list of strings): the
basename of its first element, case-insensitively and without a .exe suffix,
is sh, bash, dash, zsh, ksh, mksh or ash → :POSIX; cmd → :CMD; anything else
→ :OPAQUE."
  (let* ((program (if (consp shell) (first shell) shell))
         (name (if (stringp program) program ""))
         (slash (position-if (lambda (c) (find c "/\\")) name :from-end t))
         (base (string-downcase (subseq name (if slash (1+ slash) 0)))))
    (when (and (> (length base) 4) (string= ".exe" base :start2 (- (length base) 4)))
      (setf base (subseq base 0 (- (length base) 4))))
    (cond ((member base '("sh" "bash" "dash" "zsh" "ksh" "mksh" "ash") :test #'string=)
           :posix)
          ((string= base "cmd") :cmd)
          (t :opaque))))

;;; --- jobs ------------------------------------------------------------------

(defclass shell-job ()
  ((id :initarg :id :reader shell-job-id
       :documentation "\"job-<n>\", n counting from 1 per SHELL-TOOLS source.")
   (command :initarg :command :reader shell-job-command
            :documentation "The command line as the model gave it.")
   (status :initarg :status :initform :running :reader shell-job-status
           :documentation ":RUNNING, :EXITED (it ended on its own) or :KILLED
(job_kill, CLOSE-SHELL-TOOLS).")
   (exit-code :initarg :exit-code :initform nil :reader shell-job-exit-code
              :documentation "Exit code once :EXITED normally, else NIL.")
   (signal :initarg :signal :initform nil :reader shell-job-signal
           :documentation "Terminating signal (a keyword such as :TERM, or a
number) when the job :EXITED by a signal on the pty executor, else NIL.")
   (unit :initarg :unit :initform nil :reader shell-job-unit
         :documentation "H8c: the SUPERVISOR-PROTOCOL:UNIT running the job when the
source has a supervisor (the job id is then the unit id, \"shell-job-<n>\"), else NIL."))
  (:documentation "A background command started by bash run_in_background
(or the RUN-IN-BACKGROUND restart). The implementation may subclass it or add
slots."))

;;; --- the source ------------------------------------------------------------

(defclass shell-tools ()
  ((root :initarg :root :reader shell-tools-root
         :documentation "Absolute directory CL-STACK-PATHLIB:PATH on the local
filesystem (its truename). Commands start here unless workdir says otherwise.")
   (executor :initarg :executor :reader shell-tools-executor
             :documentation ":PTY or :PROCESS (never :AUTO).")
   (shell :initarg :shell :reader shell-tools-shell
          :documentation "Argv prefix; the command line is its last argument.")
   (dialect :initarg :dialect :reader shell-tools-dialect
            :documentation ":POSIX, :CMD or :OPAQUE: how SHELL-COMMAND-RESOURCES
reads the command line.")
   (env :initarg :env :initform nil :reader shell-tools-env
        :documentation "Alist (name . value) over the parent's environment and
+SHELL-ENVIRONMENT+; a NIL value unsets.")
   (needs-approval :initarg :needs-approval :initform t
                   :reader shell-tools-needs-approval
                   :documentation "When true, TOOL-NEEDS-APPROVAL-P is T for
bash and job_input, so with no deciding permission rule they ask.")
   (default-timeout :initarg :default-timeout :initform 120000
                    :reader shell-tools-default-timeout
                    :documentation "Milliseconds; bash timeout_ms default.")
   (max-timeout :initarg :max-timeout :initform 600000
                :reader shell-tools-max-timeout
                :documentation "Milliseconds; cap on timeout_ms and wait_ms.")
   (max-output :initarg :max-output :initform 30000
               :reader shell-tools-max-output
               :documentation "Characters of output in one result (the
status line is not counted); see TRUNCATE-SHELL-OUTPUT.")
   (max-jobs :initarg :max-jobs :initform 8 :reader shell-tools-max-jobs
             :documentation "Running background jobs at a time.")
   (max-job-buffer :initarg :max-job-buffer :initform 1000000
                   :reader shell-tools-max-job-buffer
                   :documentation "Characters of unread output a job keeps;
older unread output is dropped.")
   (names :initarg :names :reader shell-tools-names
          :documentation "Enabled tool names, a subset of +SHELL-TOOL-NAMES+
in that order.")
   (closed-p :initform nil :reader shell-tools-closed-p
             :documentation "True after CLOSE-SHELL-TOOLS.")
   (supervisor :initarg :supervisor :initform nil :reader shell-tools-supervisor
               :documentation "H8c: a SUPERVISOR-PROTOCOL:SUPERVISOR, or NIL. With one,
every bash command runs as a :SHELL-JOB unit owned by SCOPE, and the job tools
read the unit logs (see the agent-tools-units brief).")
   (scope :initarg :scope :initform nil :reader shell-tools-scope
          :documentation "H8c: the :TOOL-SOURCE scope named \"shell\" this source
made under MAKE-SHELL-TOOLS :SCOPE (default the supervisor's root); NIL without
a supervisor. CLOSE-SHELL-TOOLS closes it.")
   ;; Implementation slots.
   (files :initarg :files :reader %shell-files
          :documentation "A FILE-TOOLS over the same root with no tools, so
workdir resolves through RESOLVE-TOOL-PATH exactly like H2b paths.")
   (lock :initform (bt2:make-lock :name "shell-tools") :reader %shell-lock
         :documentation "Guards JOBS, COUNTER, RESERVED, LIVE and CLOSED-P.")
   (jobs :initform '() :accessor %shell-jobs
         :documentation "Registered jobs, newest first.")
   (counter :initform 0 :accessor %shell-counter
            :documentation "The last job number given out.")
   (reserved :initform 0 :accessor %shell-reserved
             :documentation "Background starts between the max-jobs check and
their registration.")
   (live :initform '() :accessor %shell-live
         :documentation "Every job whose command has not finished, foreground
runs included; CLOSE-SHELL-TOOLS kills them."))
  (:documentation "Tool source running shell commands in one local directory
tree. The implementation may add slots (jobs, a lock, the id counter)."))

(defun shell-tools-p (x)
  (typep x 'shell-tools))


;;; --- constructor -----------------------------------------------------------

(defun %local-filesystem ()
  "The local filesystem pathlib's PATHs share (PATH-EQUAL compares the
filesystem by identity)."
  (if (typep fs:*filesystem* 'fs:local-filesystem)
      fs:*filesystem*
      fs::*local-filesystem*))

(defun %check-env (env)
  (unless (and (listp env)
               (every (lambda (e)
                        (and (consp e) (stringp (car e)) (plusp (length (car e)))
                             (typep (cdr e) '(or null string))))
                      env))
    (error 'type-error :datum env
                       :expected-type '(or null (cons (cons string (or null string)) list)))))

(defun make-shell-tools (&key root (executor :auto) shell dialect env
                           (needs-approval t) (default-timeout 120000)
                           (max-timeout 600000) (max-output 30000) (max-jobs 8)
                           (max-job-buffer 1000000) (tools +shell-tool-names+)
                           supervisor scope)
  "→ a SHELL-TOOLS source rooted at ROOT (a string, pathname or local
CL-STACK-PATHLIB:PATH naming an existing directory).
EXECUTOR: :AUTO (:PTY on Unix, :PROCESS on Windows), :PTY or :PROCESS; :PTY on
Windows signals an ERROR. SHELL defaults to (DEFAULT-SHELL); DIALECT to
(SHELL-DIALECT SHELL). TOOLS selects the exposed tool names.
A missing ROOT signals CL-STACK-PATHLIB:PATH-NOT-FOUND; a ROOT that is not a
directory signals CL-STACK-PATHLIB:PATH-ERROR; a PATH on another filesystem
than the local one, an unknown tool name, an unknown executor or dialect, or
DEFAULT-TIMEOUT above MAX-TIMEOUT signal an ERROR.
H8c: SUPERVISOR (a SUPERVISOR-PROTOCOL:SUPERVISOR) runs every command as a unit
(see SHELL-TOOLS-SUPERVISOR); :EXECUTOR :AUTO then means :PROCESS, and :PTY is an
ERROR until the supervisor has a pty executor (H8a-1). SCOPE (a scope of
SUPERVISOR, default its root) is the parent of the source's own \"shell\"
scope. SCOPE without SUPERVISOR, or a scope of another supervisor → ERROR."
  (check-type executor (member :auto :pty :process))
  (when (and supervisor (not (typep supervisor 'sup:supervisor)))
    (error 'type-error :datum supervisor :expected-type 'sup:supervisor))
  (when scope
    (unless supervisor
      (error "make-shell-tools: :scope needs :supervisor"))
    (unless (and (typep scope 'sup:scope) (eq supervisor (sup:scope-supervisor scope)))
      (error "make-shell-tools: :scope ~s is not a scope of the supervisor" scope)))
  (when (and supervisor (eq executor :pty))
    (error "make-shell-tools: :executor :pty with a supervisor arrives with the supervisor's pty executor (H8a-1); use :process"))
  (check-type dialect (or null (member :posix :cmd :opaque)))
  (check-type default-timeout (integer 1))
  (check-type max-timeout (integer 1))
  (check-type max-output (integer 1))
  (check-type max-jobs (integer 1))
  (check-type max-job-buffer (integer 1))
  (check-type tools list)
  (%check-env env)
  (when (> default-timeout max-timeout)
    (error "make-shell-tools: :default-timeout ~d is above :max-timeout ~d"
           default-timeout max-timeout))
  (let ((shell (or shell (default-shell))))
    (unless (and (consp shell) (every #'stringp shell) (plusp (length (first shell))))
      (error 'type-error :datum shell :expected-type '(cons string list)))
    (let ((unknown (remove-if (lambda (n) (member n +shell-tool-names+ :test #'equal)) tools)))
      (when unknown
        (error "make-shell-tools: unknown tool name~p ~{~s~^, ~}; known: ~{~a~^, ~}"
               (length unknown) unknown +shell-tool-names+)))
    (let ((executor (cond ((not (eq executor :auto)) executor)
                          (supervisor :process)
                          (t #+(or windows win32) :process #-(or windows win32) :pty))))
      #+(or windows win32)
      (when (eq executor :pty)
        (error "make-shell-tools: :executor :pty is not supported on Windows in 0.2.0; use :process"))
      (let* ((path (cond ((fs:path-p root)
                          (unless (typep (fs:path-filesystem root) 'fs:local-filesystem)
                            (error "make-shell-tools: the root ~a is not on the local filesystem; commands run in OS directories"
                                   (fs:as-posix root)))
                          root)
                         ((or (stringp root) (pathnamep root))
                          (fs:ensure-path root :filesystem (%local-filesystem)))
                         (t (error 'type-error :datum root
                                               :expected-type '(or string pathname fs:path)))))
             ;; Signals PATH-NOT-FOUND / PATH-ERROR, and gives the truename.
             (files (make-file-tools :root path :tools '())))
        (make-instance 'shell-tools
                       :root (file-tools-root files)
                       :files files
                       :executor executor
                       :shell (copy-list shell)
                       :dialect (or dialect (shell-dialect shell))
                       :env (copy-alist env)
                       :needs-approval needs-approval
                       :default-timeout default-timeout
                       :max-timeout max-timeout
                       :max-output max-output
                       :max-jobs max-jobs
                       :max-job-buffer max-job-buffer
                       :names (remove-if-not (lambda (n) (member n tools :test #'equal))
                                             +shell-tool-names+)
                       :supervisor supervisor
                       :scope (and supervisor
                                   (sup:make-scope supervisor :tool-source "shell"
                                                   :parent (or scope (sup:supervisor-root-scope supervisor)))))))))

;;; --- facade ----------------------------------------------------------------

(defparameter +shell-tool-handlers+
  '(("bash" . %tool-bash) ("job_output" . %tool-job-output) ("job_input" . %tool-job-input)
    ("job_kill" . %tool-job-kill) ("job_list" . %tool-job-list)))

(defun %shell-enabled-p (tools name)
  (and (stringp name) (member name (shell-tools-names tools) :test #'string=) t))

(defun %closed-error (name)
  (error 'agent:tool-unavailable
         :message (format nil "~a cannot run: this shell-tools source is closed" name)))

(defun call-shell-tool (tools name arguments)
  "Run tool NAME of TOOLS synchronously on ARGUMENTS (a JSON object string or
a string-keyed hash table). → the result string. Failures signal
AI-AGENT-PROTOCOL:TOOL-FAILURE subtypes; a foreground timeout signals
SHELL-TIMEOUT with its restarts; a name that is not enabled signals
AI-AGENT-PROTOCOL:AGENT-UNKNOWN-TOOL; any call after CLOSE-SHELL-TOOLS
signals AI-AGENT-PROTOCOL:TOOL-UNAVAILABLE."
  (unless (%shell-enabled-p tools name)
    ;; An AGENT-CONDITION, not an ERROR; ERROR still makes it impossible to
    ;; ignore.
    (error 'agent:agent-unknown-tool
           :name name
           :message (format nil "~a is not enabled on this shell-tools source (enabled: ~{~a~^, ~})"
                            name (shell-tools-names tools))))
  (when (shell-tools-closed-p tools)
    (%closed-error name))
  (if (shell-tools-supervisor tools)
      (%call-supervised-shell-tool tools name (%decode-arguments arguments))
      (funcall (cdr (assoc name +shell-tool-handlers+ :test #'string=))
               tools (%decode-arguments arguments))))

(defun close-shell-tools (tools)
  "Kill every running job of TOOLS (status :KILLED) and refuse later calls.
Idempotent. → NIL. H8c: with a supervisor, close the source's scope instead
(its units stop; their logs stay)."
  (when (shell-tools-supervisor tools)
    (return-from close-shell-tools (%close-supervised-shell-tools tools)))
  (let ((live (bt2:with-lock-held ((%shell-lock tools))
                (unless (shell-tools-closed-p tools)
                  (setf (slot-value tools 'closed-p) t)
                  (copy-list (%shell-live tools))))))
    (dolist (job live)
      (%kill-job job 5)))
  nil)

(defun shell-tools-jobs (tools)
  "→ the SHELL-JOBs of TOOLS, oldest first, finished ones included."
  (bt2:with-lock-held ((%shell-lock tools))
    (reverse (%shell-jobs tools))))

(defun find-shell-job (tools id)
  "→ the SHELL-JOB of TOOLS named ID, or NIL."
  (and (stringp id)
       (find id (shell-tools-jobs tools) :key #'shell-job-id :test #'equal)))
