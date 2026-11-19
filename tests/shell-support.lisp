(in-package #:agent-tools/tests)

;;; Helpers for the shell tests (H2c). Every SHELL-TOOLS runs over a fresh
;;; local temp directory (its truename) and is closed afterwards, so no job
;;; outlives its test. Executor tests run once per executor this OS has:
;;; :PTY and :PROCESS on Unix, :PROCESS on Windows. Commands that differ
;;; between /bin/sh and cmd.exe go through PICK.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +executors+
    #+(or windows win32) '(:process)
    #-(or windows win32) '(:pty :process)
    "Executors exercised on this OS."))

(defmacro deftest-executors (name &body body)
  "A test run once per executor in +EXECUTORS+, with EXECUTOR bound."
  `(deftest-parametrize ,name
       ((executor) :ids ,(mapcar #'string-downcase +executors+)
        ,@(mapcar #'list +executors+))
     ,@body))

(defmacro with-shell ((tools root executor &rest keys) entries &body body)
  "ROOT: a fresh local directory holding ENTRIES (see MAKE-TREE). TOOLS: a
SHELL-TOOLS over it made with :EXECUTOR EXECUTOR and KEYS, closed after BODY."
  `(with-root (,root :local)
     (make-tree ,root ,entries)
     (let ((,tools nil))
       (unwind-protect
            (progn
              (setf ,tools (make-shell-tools :root ,root :executor ,executor ,@keys))
              ,@body)
         (when ,tools
           (ignore-errors (close-shell-tools ,tools)))))))

(defun pick (posix cmd)
  "POSIX on Unix (the default shell is /bin/sh), CMD on Windows (cmd.exe)."
  #+(or windows win32) (progn posix cmd)
  #-(or windows win32) (progn cmd posix))

(defun sleep-command (seconds)
  "A command that takes about SECONDS seconds."
  (pick (format nil "sleep ~d" seconds)
        (format nil "ping -n ~d 127.0.0.1 >nul" (1+ seconds))))

(defun shell (tools name &rest kv)
  "CALL-SHELL-TOOL with arguments given as a JSON object string."
  (call-shell-tool tools name (apply #'json-args kv)))

(defun bash (tools command &rest kv)
  (apply #'shell tools "bash" "command" command kv))

(defun job-output (tools id &rest kv)
  (apply #'shell tools "job_output" "job_id" id kv))

(defun started-text (id command)
  "The bash result for a job started in the background."
  (format nil "Started job ~a: ~a~%Read its output with job_output (job_id \"~a\"); stop it with job_kill."
          id command id))

(defun exit-text (output code)
  "A foreground bash result: OUTPUT (without its final LF) then the status."
  (if (zerop (length output))
      (format nil "[exit code ~d]" code)
      (format nil "~a~%[exit code ~d]" output code)))

(defun job-text (output status-line)
  (if (zerop (length output))
      status-line
      (format nil "~a~%~a" output status-line)))

(defun numbered-lines (n)
  "\"line1\" … \"lineN\", each followed by LF."
  (format nil "~{line~d~%~}" (loop for i from 1 to n collect i)))

(defun lines-command (n)
  "A command printing NUMBERED-LINES N."
  (pick (format nil "i=1; while [ $i -le ~d ]; do echo line$i; i=$((i+1)); done" n)
        (format nil "for /L %i in (1,1,~d) do @echo line%i" n)))

(defun trim-final-newlines (string)
  (string-right-trim '(#\Newline) string))

(defun now ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defmacro timed (form)
  "→ (values seconds-elapsed value-of-FORM)."
  (let ((start (gensym)) (v (gensym)))
    `(let* ((,start (now))
            (,v ,form))
       (values (- (now) ,start) ,v))))

(defun real-failure (condition)
  "CONDITION when it is not the contract stub's not-implemented error."
  (and condition
       (not (has "is not implemented" (message condition)))
       condition))

(defun scan (command &rest keys)
  (apply #'shell-command-resources command keys))

(defun check-resources (cases &rest keys)
  "CASES: (command expected-resources). One OK per case."
  (dolist (c cases)
    (destructuring-bind (command expected) c
      (ok (equal expected
                 (handler-case (apply #'shell-command-resources command keys)
                   (error (e) (list :error (princ-to-string e)))))
          (format nil "~s ~s" command keys)))))

(defun check-dynamic (commands &rest keys)
  "Each of COMMANDS scans with \"*\" as its last resource."
  (dolist (command commands)
    (let ((r (handler-case (apply #'shell-command-resources command keys)
               (error () nil))))
      (ok (equal "*" (car (last r))) (format nil "~s is dynamic" command)))))
