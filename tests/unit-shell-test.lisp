(in-package #:agent-tools/tests)

;;; H8c: SHELL-TOOLS with a supervisor. Every bash command is a :SHELL-JOB unit;
;;; bash and the job tools keep their H2c text, with unit ids as job ids.

(defmacro with-supervised-shell ((tools sup root &rest keys) &body body)
  "TOOLS: a SHELL-TOOLS over a fresh ROOT with a fresh supervisor SUP (its own
temp state directory) and KEYS; UNITS: a UNIT-TOOLS over SUP."
  `(call-with-state-dir
    (lambda (state)
      (let ((,sup (sup:make-supervisor :exit-hook nil :state-directory state)))
        (unwind-protect
             (with-shell (,tools ,root :auto :supervisor ,sup ,@keys) '()
               ,@body)
          (quiet-shutdown ,sup))))))

(defun shell-units (sup) (sup:find-units sup :kind :shell-job))

(deftest supervised-shell-construction
  (with-supervised-shell (tools sup root)
    (ok (eq sup (shell-tools-supervisor tools)))
    (ok (eq :process (shell-tools-executor tools)) ":auto means :process under a supervisor")
    (let ((scope (shell-tools-scope tools)))
      (ok (typep scope 'sup:scope))
      (ok (equal '("shell") (sup:scope-path scope)))
      (ok (eq :tool-source (sup:scope-kind scope)))))
  (call-with-state-dir
   (lambda (state)
     (let ((sup (sup:make-supervisor :exit-hook nil :state-directory state)))
       (unwind-protect
            (with-root (root :local)
              (ok (failure (make-shell-tools :root root :supervisor sup :executor :pty))
                  ":pty needs the supervisor's pty executor (H8a-1)")
              (ok (failure (make-shell-tools :root root :scope (sup:supervisor-root-scope sup)))
                  ":scope without :supervisor")
              (let ((s (sup:make-scope sup :session "s")))
                (let ((tools (make-shell-tools :root root :supervisor sup :scope s)))
                  (ok (equal '("s" "shell") (sup:scope-path (shell-tools-scope tools))))
                  (close-shell-tools tools))))
         (quiet-shutdown sup))))))

(deftest supervised-bash-foreground
  (with-supervised-shell (tools sup root)
    (ok (equal (exit-text "hi" 0) (bash tools "echo hi")) "the H2c text")
    (let ((u (first (shell-units sup))))
      (ok u "the command ran as a unit")
      (ok (equal '(:tool "bash") (sup:unit-labels u)))
      (ok (eq (shell-tools-scope tools) (sup:unit-owner u)))
      (ok (eq :exited (sup:unit-state u)))
      ;; cmd.exe's echo ends its line in CR LF and the log keeps the exact
      ;; output octets (supervisor.md, Binary / non-UTF-8), so the record is "hi" CR.
      (ok (equal (list (pick "hi" (format nil "hi~c" #\Return)))
                 (mapcar #'sup:log-record-text (sup:log-read u)))
          "its output is in its log"))
    (ok (equal (exit-text "" 3) (bash tools (pick "exit 3" "exit /b 3"))))
    (ok (null (shell-tools-jobs tools)) "foreground commands are not jobs")))

(deftest supervised-bash-stdin-is-closed
  (with-supervised-shell (tools sup root)
    (multiple-value-bind (seconds result) (timed (bash tools (pick "cat" "sort") "timeout_ms" 20000))
      (ok (< seconds 10) "a stdin reader sees end of file at once")
      (ok (equal (exit-text "" 0) result)))))

(deftest supervised-jobs-keep-their-text
  (with-supervised-shell (tools sup root)
    (bash tools "echo warm-up")
    (let* ((started (bash tools "echo bg" "run_in_background" t))
           (job (first (shell-tools-jobs tools)))
           (id (shell-job-id job)))
      (ok (typep (shell-job-unit job) 'sup:unit))
      (ok (equal id (sup:unit-id (shell-job-unit job))) "the job id is the unit id")
      (ok (equal "shell-job-2" id) "the foreground command took shell-job-1")
      (ok (equal (started-text id "echo bg") started))
      (ok (equal (job-text "bg" (format nil "[job ~a exited with code 0]" id))
                 (job-output tools id "wait_ms" 20000)))
      (ok (equal (format nil "[job ~a exited with code 0]" id) (job-output tools id))
          "the next read has nothing new")
      (ok (equal (tsv id "exited 0" "echo bg") (shell tools "job_list")))
      (ok (eq :exited (shell-job-status job)))
      (ok (eql 0 (shell-job-exit-code job))))))

(deftest supervised-job-kill-and-errors
  (with-supervised-shell (tools sup root)
    (bash tools (sleep-command 60) "run_in_background" t)
    (let* ((job (first (shell-tools-jobs tools)))
           (id (shell-job-id job)))
      (ok (equal (format nil "[job ~a killed]" id) (shell tools "job_kill" "job_id" id)))
      (ok (eq :killed (sup:unit-state (shell-job-unit job))))
      (ok (equal (tsv id "killed" (sleep-command 60)) (shell tools "job_list")))
      (ok (typep (failure (job-output tools "job-1")) 'job-not-found) "H2c ids are gone under a supervisor")
      (ok (typep (failure (shell tools "job_input" "job_id" id "input" "x")) 'job-not-running)))))

(defun read-until (tools id needle &key (timeout 20) (initial ""))
  "job_output (200 ms waits) until INITIAL plus the text read so far has NEEDLE.
→ that text."
  (let ((seen initial))
    (wait-for (lambda ()
                (setf seen (concatenate 'string seen (job-output tools id "wait_ms" 200)))
                (has needle seen))
              :timeout timeout)
    seen))

(deftest supervised-job-input-and-incremental-output
  #+(or windows win32) (skip "a line-by-line stdin echo needs /bin/sh")
  #-(or windows win32)
  (with-supervised-shell (tools sup root)
    (bash tools "while read l; do echo got:$l; done" "run_in_background" t)
    (let ((id (shell-job-id (first (shell-tools-jobs tools)))))
      (ok (has "got:one" (read-until tools id "got:one"
                                     :initial (shell tools "job_input" "job_id" id "input" (format nil "one~%")))))
      (let ((next (read-until tools id "got:two"
                              :initial (shell tools "job_input" "job_id" id "input" (format nil "two~%")))))
        (ok (has "got:two" next))
        (ok (not (has "got:one" next)) "job_output returns only what is new"))
      (shell tools "job_kill" "job_id" id))))

(deftest supervised-timeout-to-background
  (with-supervised-shell (tools sup root)
    (let ((result (handler-bind ((shell-timeout (lambda (c) (declare (ignore c))
                                                  (invoke-restart 'run-in-background))))
                    (bash tools (sleep-command 30) "timeout_ms" 300))))
      (let ((job (first (shell-tools-jobs tools))))
        (ok job "the timed-out command became a job")
        (ok (equal (started-text (shell-job-id job) (sleep-command 30)) result))
        (ok (eq :running (sup:unit-state (shell-job-unit job))))
        (shell tools "job_kill" "job_id" (shell-job-id job))))))

(deftest supervised-truncation-names-the-unit
  (with-supervised-shell (tools sup root :max-output 2000)
    (let* ((result (bash tools (lines-command 3000)))
           (u (car (last (shell-units sup)))))
      (ok (<= (length result) (+ 2000 (length "[exit code 0]") 1)))
      (ok (has (format nil "characters omitted; the full output is in unit ~a: use unit_read or unit_search]"
                       (sup:unit-id u))
               result)))))

(deftest supervised-close-stops-the-scope
  (with-supervised-shell (tools sup root)
    (bash tools (sleep-command 60) "run_in_background" t)
    (let ((u (shell-job-unit (first (shell-tools-jobs tools)))))
      (ok (null (close-shell-tools tools)))
      (ok (eq :killed (sup:unit-state u)))
      (ok (sup:scope-closed-p (shell-tools-scope tools)))
      (ok (sup:log-read u :types t) "the log stays")
      (ok (null (close-shell-tools tools)) "idempotent")
      (ok (typep (failure (bash tools "echo x")) 'a:tool-unavailable)))))

(deftest supervised-jobs-in-unit-tools
  (with-supervised-shell (tools sup root)
    (bash tools "echo visible-to-units")
    (let ((units (make-unit-tools :supervisor sup)))
      (unwind-protect
           (let ((u (first (shell-units sup))))
             (ok (has (tsv (sup:unit-id u) "shell-job" "exited 0" "shell") (ucall units "unit_list")))
             (ok (has "tool=bash" (ucall units "unit_list")))
             (ok (has "visible-to-units" (ucall units "unit_search" "query" "visible-to-units"))))
        (ignore-errors (close-unit-tools units))))))
