(in-package #:agent-tools/tests)

;;; R4d-4a: confined bash and jobs without a supervisor, once per executor this
;;; OS has. The recording backend shows what CONFINE-COMMAND was asked for and
;;; what ran; the launch's environment (SHELL-JOB-LAUNCH) is what the child sees.

;;; --- what runs -------------------------------------------------------------------

(deftest-executors confined-runs-the-launch-argv
  (let ((b (make-recording-backend :prefix (pick "echo wrapped; " "echo wrapped& "))))
    (with-confined-shell (tools root executor b) '()
      (let ((r (bash tools "echo hi")))
        (ok (uiop:string-prefix-p "wrapped" r) "the backend's argv is what ran")
        (ok (has "hi" r))
        (ok (uiop:string-suffix-p r "[exit code 0]")))
      (let ((rec (only-launch b)))
        (ok rec "one launch per command line")
        (when rec
          (ok (equal (shell-tools-shell tools) (butlast (recorded-launch-argv rec)))
              "the backend got the shell prefix")
          (ok (has "echo hi" (car (last (recorded-launch-argv rec))))
              "… and the command line as its last argument"))))))

(deftest-executors confined-env-is-the-launch-env
  (let ((b (make-recording-backend))
        (old (uiop:getenv "AGENT_TOOLS_CANARY")))
    (setf (uiop:getenv "AGENT_TOOLS_CANARY") "leaked")
    (unwind-protect
         (with-confined-shell (tools root executor b :env '(("AGENT_TOOLS_SOURCE" . "yes"))) '()
           (bash tools (env-command) "run_in_background" t)
           (let* ((out (wait-job-finished tools "job-1"))
                  (job (find-shell-job tools "job-1"))
                  (launch (shell-job-launch job))
                  (names (env-names out)))
             (ok (shell-job-confined-p job))
             (ok (typep launch 'sc:confined-launch))
             (ok (has "STACK_COMPUTE_CONFINEMENT=model" out) "the backend's env")
             (ok (has "AGENT_TOOLS_BACKEND=recording" out))
             (ok (has "AGENT_TOOLS_SOURCE=yes" out) "the source's :env, after the scrub")
             (ok (has "TERM=dumb" out) "+shell-environment+, after the scrub")
             (ok (not (member "AGENT_TOOLS_CANARY" names :test #'string-equal))
                 "nothing of the parent's environment leaks")
             (when launch
               (let ((launch-names (mapcar #'car (sc:confined-launch-env launch))))
                 (ok (member "AGENT_TOOLS_SOURCE" launch-names :test #'string=))
                 (ok (not (member "AGENT_TOOLS_CANARY" launch-names :test #'string=)))
                 #-(or windows win32)
                 (ok (every (lambda (n) (or (member n launch-names :test #'string=)
                                            (member n +shell-added-variables+ :test #'string=)))
                            names)
                     (format nil "exactly the launch env (+ the shell's own): ~s"
                             (set-difference names (append launch-names +shell-added-variables+)
                                             :test #'string=)))))))
      (setf (uiop:getenv "AGENT_TOOLS_CANARY") (or old "")))))

(deftest-executors confined-workdir-is-the-directory
  (let ((b (make-recording-backend)))
    (with-confined-shell (tools root executor b) '(("sub/keep.txt" "k"))
      (let ((r (bash tools (pick "pwd" "cd") "workdir" "sub")))
        (ok (has "sub" r)))
      (let ((rec (only-launch b)))
        (ok rec)
        (when rec
          (ok (same-directory-p (recorded-launch-directory rec)
                                (merge-pathnames "sub/" (p:path-pathname root)))
              "workdir → :directory")))
      (bash tools "echo root")
      (let ((rec (second (launches b))))
        (ok (and rec (same-directory-p (recorded-launch-directory rec) (p:path-pathname root)))
            "no workdir → the root")))))

(deftest-executors confined-private-tmp
  (let ((b (make-recording-backend)))
    (with-confined-shell (tools root executor b) '()
      (let ((r (bash tools (pick "echo \"[$TMPDIR]\"" "echo [%TMPDIR%]"))))
        (let ((rec (only-launch b)))
          (ok (and rec (recorded-launch-tmp rec)) "a private temp directory per launch")
          (when (and rec (recorded-launch-tmp rec))
            (ok (has (string-right-trim "/\\" (uiop:native-namestring (recorded-launch-tmp rec))) r)
                "TMPDIR is the launch's")))))))

;;; --- release ---------------------------------------------------------------------

(deftest-executors confined-release-on-exit
  (let ((b (make-recording-backend)))
    (with-confined-shell (tools root executor b) '()
      (ok (equal (exit-text "" 3) (bash tools (pick "exit 3" "exit /b 3"))))
      (let ((rec (only-launch b)))
        (ok rec)
        (when rec
          (ok (released-eventually-p rec) "released once after a normal exit")
          (ok (tmp-gone-p rec) "the private tmp is gone"))))))

(deftest-executors confined-release-on-timeout
  (let ((b (make-recording-backend)))
    (with-confined-shell (tools root executor b) '()
      (ok (typep (failure (bash tools (sleep-command 30) "timeout_ms" 300)) 'shell-timeout))
      (let ((rec (only-launch b)))
        (ok rec)
        (when rec
          (ok (released-eventually-p rec) "released once after the timeout killed it")
          (ok (tmp-gone-p rec)))))))

(deftest-executors confined-timeout-to-background-keeps-the-launch
  (let ((b (make-recording-backend)))
    (with-confined-shell (tools root executor b) '()
      (let ((r (handler-bind ((shell-timeout (lambda (c)
                                               (declare (ignore c))
                                               (invoke-restart 'run-in-background))))
                 (bash tools (sleep-command 30) "timeout_ms" 300))))
        (ok (has "Started job job-1" r)))
      (let ((rec (only-launch b))
            (job (find-shell-job tools "job-1")))
        (ok (and job (shell-job-confined-p job)))
        (when rec
          (sleep 0.3)
          (ok (zerop (cleanups rec)) "not released while the job runs")
          (shell tools "job_kill" "job_id" "job-1")
          (ok (released-eventually-p rec) "released once after job_kill"))))))

(deftest-executors confined-release-on-kill
  (let ((b (make-recording-backend)))
    (with-confined-shell (tools root executor b) '()
      (bash tools (sleep-command 30) "run_in_background" t)
      (let ((rec (only-launch b)))
        (ok rec)
        (when rec
          (sleep 0.3)
          (ok (zerop (cleanups rec)) "not released while running")
          (ok (or (null (recorded-launch-tmp rec))
                  (uiop:directory-exists-p (recorded-launch-tmp rec)))
              "the private tmp exists while running")
          (ok (equal "[job job-1 killed]" (shell tools "job_kill" "job_id" "job-1")))
          (ok (released-eventually-p rec) "released once after job_kill")
          (ok (tmp-gone-p rec)))))))

(deftest-executors confined-release-on-close
  (let ((b (make-recording-backend)))
    (with-confined-shell (tools root executor b) '()
      (bash tools (sleep-command 30) "run_in_background" t)
      (close-shell-tools tools)
      (let ((rec (only-launch b)))
        (ok (and rec (released-eventually-p rec)) "CLOSE-SHELL-TOOLS releases")))))

(deftest-executors confined-release-on-spawn-failure
  (let ((b (make-recording-backend :program "/nonexistent/agent-tools-no-such-shell")))
    (with-confined-shell (tools root executor b) '()
      ;; Either the launcher refuses (TOOL-UNAVAILABLE) or the child fails to
      ;; exec (a non-zero exit): the launch is released either way.
      (let ((c (failure (bash tools "echo hi"))))
        (ok (or (null c) (typep c 'a:tool-failure)) (format nil "~a" c)))
      (let ((rec (only-launch b)))
        (ok (and rec (released-eventually-p rec)) "released after a failed spawn"))
      (ok (null (shell-tools-jobs tools))))))

;;; --- refusals --------------------------------------------------------------------

(deftest-executors confined-refusal-unsupported-runs-nothing
  (let ((b (make-recording-backend :features '(:filesystem :deny-read))))
    (with-confined-shell (tools root executor b) '()
      (dolist (background '(nil t))
        (let ((c (refused (lambda () (bash tools (ran-marker-command root "ran.txt")
                                           "run_in_background" background)))))
          (ok (typep c 'shell-confinement-refused) (format nil "background ~a: ~a" background c))
          (when (typep c 'shell-confinement-refused)
            (ok (typep c 'a:tool-unavailable))
            (ok (typep (shell-confinement-refused-cause c) 'sc:confinement-unsupported))
            (ok (equal '(:network-none)
                       (sc:confinement-unsupported-missing (shell-confinement-refused-cause c))))
            (ok (equal (ran-marker-command root "ran.txt") (shell-confinement-refused-command c)))
            (ok (has "was not run" (message c)))
            (ok (has "refused" (message c))))))
      (sleep 0.3)
      (ok (not (exists-at root "ran.txt")) "nothing ran")
      (ok (null (launches b)) "the backend was never asked for an argv")
      (ok (null (shell-tools-jobs tools)) "no job registered")
      (ok (equal "[no jobs]" (shell tools "job_list"))))))

(deftest-executors confined-refusal-unavailable-runs-nothing
  (with-confined-shell (tools root executor (make-instance 'unavailable-backend)) '()
    (let ((c (refused (lambda () (bash tools (ran-marker-command root "ran.txt"))))))
      (ok (typep c 'shell-confinement-refused) (format nil "~a" c))
      (when (typep c 'shell-confinement-refused)
        (ok (typep (shell-confinement-refused-cause c) 'sc:confinement-unavailable))
        (ok (has "unavailable on purpose" (message c)) "the reason is in the message")))
    (sleep 0.3)
    (ok (not (exists-at root "ran.txt")))))

(deftest-executors confined-refusal-use-value
  (let ((b (make-recording-backend :features '(:filesystem :deny-read :network-allow))))
    (with-confined-shell (tools root executor b) '()
      (let ((r (handler-bind ((shell-confinement-refused
                                (lambda (c)
                                  (use-value (area-confinement root :network :allow)
                                             (shell-confinement-refused-cause c)))))
                 (bash tools (ran-marker-command root "ran.txt")))))
        (ok (equal (exit-text "" 0) r) "the restart's confinement ran it")
        (ok (exists-at root "ran.txt"))
        (let ((rec (only-launch b)))
          (ok (and rec (released-eventually-p rec))))))))

;;; --- metadata --------------------------------------------------------------------

(deftest-executors confined-job-list-and-metadata
  (with-confined-shell (tools root executor (sc:make-model-confinement-backend)) '()
    (bash tools "echo one" "run_in_background" t)
    (wait-job-finished tools "job-1")
    (let ((job (find-shell-job tools "job-1")))
      (ok (shell-job-confined-p job))
      (ok (typep (shell-job-launch job) 'sc:confined-launch))
      (ok (and (shell-job-launch job) (sc:confined-launch-released-p (shell-job-launch job)))
          "released once it ended (kept on the job)"))
    (ok (equal (format nil "job-1~cexited 0~cconfined~cecho one" #\Tab #\Tab #\Tab)
               (shell tools "job_list")))))

(deftest-executors unconfined-jobs-unchanged
  (with-shell (tools root executor) '()
    (bash tools "echo one" "run_in_background" t)
    (job-output tools "job-1" "wait_ms" 20000)
    (let ((job (find-shell-job tools "job-1")))
      (ok (null (shell-job-launch job)))
      (ok (not (shell-job-confined-p job))))
    (ok (equal (format nil "job-1~cexited 0~cecho one" #\Tab #\Tab) (shell tools "job_list")))))

;;; --- denial notes ----------------------------------------------------------------

(defun denial-command (line code)
  "A command printing LINE to stderr and exiting with CODE."
  (pick (format nil "echo '~a' >&2; exit ~d" line code)
        (format nil "echo ~a 1>&2 & exit /b ~d" line code)))

(deftest-executors confined-denial-note-foreground
  ;; The recording backend replaces the command line: a simulated denial.
  (let ((b (make-recording-backend
            :replace-command (denial-command "touch: /etc/agent-tools-x: Operation not permitted" 1))))
    (with-confined-shell (tools root executor b) '()
      (let ((r (bash tools "touch /etc/agent-tools-x")))
        (ok (has "[sandbox: " r) "a note on a simulated denial")
        (ok (has "writes are allowed only under" r))
        (ok (uiop:string-suffix-p r "[exit code 1]") "the status line stays last")
        (let ((ls (lines r)))
          (ok (and (>= (length ls) 2)
                   (uiop:string-prefix-p "[sandbox: " (nth (- (length ls) 2) ls)))
              "the note is right before the status line")))))
  (let ((b (make-recording-backend
            :replace-command (denial-command "curl: (6) Could not resolve host: example.com" 6))))
    (with-confined-shell (tools root executor b) '()
      (ok (has "network access is off" (bash tools "curl https://example.com"))))))

(deftest-executors confined-no-note-on-normal-failure
  (let ((b (make-recording-backend
            :replace-command (denial-command "ls: nope: No such file or directory" 1))))
    (with-confined-shell (tools root executor b) '()
      (let ((r (bash tools "ls nope")))
        (ok (has "No such file" r))
        (ok (not (has "[sandbox:" r)) "an ordinary failure gets no note"))))
  (with-confined-shell (tools root executor (sc:make-model-confinement-backend)) '()
    (ok (not (has "[sandbox:" (bash tools "echo fine"))) "nor a success")))

(deftest-executors unconfined-no-note
  (with-shell (tools root executor) '()
    (let ((r (bash tools (denial-command "touch: /etc/x: Operation not permitted" 1))))
      (ok (has "Operation not permitted" r))
      (ok (not (has "[sandbox:" r)) "only confined sources add notes"))))

(deftest-executors confined-denial-note-job-output
  (let ((b (make-recording-backend
            :replace-command (denial-command "touch: /etc/agent-tools-x: Operation not permitted" 1))))
    (with-confined-shell (tools root executor b) '()
      (bash tools "touch /etc/agent-tools-x" "run_in_background" t)
      (let ((r (job-output tools "job-1" "wait_ms" 20000)))
        (ok (has "[sandbox: " r) "the read with the final status carries the note")
        (ok (uiop:string-suffix-p r "[job job-1 exited with code 1]"))))))
