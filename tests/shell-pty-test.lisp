(in-package #:agent-tools/tests)

;;; The :pty executor only (Unix; this file is
;;; :if-feature (:not (:or :windows :win32))). The command's shell leads its
;;; own session, so killing it reaches its whole process tree: a grandchild
;;; that would create a marker file later never does.

(defun marker-command (name)
  (format nil "(sleep 2; touch ~a) & wait" name))

(defun wait-past-markers ()
  (sleep 4))

(deftest pty-job-kill-kills-the-tree
  (with-shell (tools root :pty) '()
    (bash tools (marker-command "m1") "run_in_background" t)
    (job-output tools "job-1" "wait_ms" 500)
    (ok (equal "[job job-1 killed]" (shell tools "job_kill" "job_id" "job-1")))
    (wait-past-markers)
    (ok (not (exists-at root "m1")))))

(deftest pty-timeout-kills-the-tree
  (with-shell (tools root :pty) '()
    (ok (typep (failure (bash tools (marker-command "m2") "timeout_ms" 500)) 'shell-timeout))
    (wait-past-markers)
    (ok (not (exists-at root "m2")))))

(deftest pty-close-kills-the-tree
  (with-shell (tools root :pty) '()
    (bash tools (marker-command "m3") "run_in_background" t)
    (job-output tools "job-1" "wait_ms" 500)
    (close-shell-tools tools)
    (wait-past-markers)
    (ok (not (exists-at root "m3")))))

(deftest pty-marker-control
  ;; Control for the three tests above: left alone, the grandchild does
  ;; create its marker.
  (with-shell (tools root :pty) '()
    (bash tools (marker-command "m0") "run_in_background" t)
    (ok (equal "[job job-1 exited with code 0]" (job-output tools "job-1" "wait_ms" 20000)))
    (ok (exists-at root "m0"))))

(deftest pty-signal-status
  (with-shell (tools root :pty) '()
    (ok (equal "[killed by signal TERM]" (bash tools "kill -TERM $$")))
    (bash tools "kill -TERM $$" "run_in_background" t)
    (ok (equal "[job job-1 terminated by signal TERM]"
               (job-output tools "job-1" "wait_ms" 20000)))
    (let ((job (find-shell-job tools "job-1")))
      (ok (eq :exited (shell-job-status job)))
      (ok (null (shell-job-exit-code job)))
      (ok (eq :term (shell-job-signal job))))
    (ok (equal (format nil "job-1~csignal TERM~ckill -TERM $$" #\Tab #\Tab)
               (shell tools "job_list")))))
