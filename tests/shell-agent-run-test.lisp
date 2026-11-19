(in-package #:agent-tools/tests)

;;; SHELL-TOOLS inside a real agent run (mock LLM backend, libuv loop),
;;; next to a FILE-TOOLS over the same root.

(defun shell-agent (sources steps box &rest keys)
  (apply #'a:make-ai-agent :name "coder" :backend (scripted-backend steps box)
                           :tools sources keys))

(defparameter +allow-shell+ (list (a:make-permission-rule "*" "*" :allow)))

(deftest shell-agent-sees-both-sources
  (with-shell (shell root :auto) '()
    (let* ((files (make-file-tools :root root))
           (agent (shell-agent (list files shell) '("done") (list nil))))
      (ok (equal (append +file-tool-names+ +shell-tool-names+)
                 (mapcar #'llm:llm-tool-name (a:collect-run-tools agent))))
      (ok (eq shell (a:find-agent-tool agent "bash")))
      (ok (eq shell (a:find-agent-tool agent "job_list")))
      (ok (eq files (a:find-agent-tool agent "read"))))))

(deftest shell-agent-run-bash-and-jobs
  (with-agent-loop
    (with-shell (shell root :auto) '()
      (let* ((run (a:run-ai-agent
                   (shell-agent (list shell)
                                (list (cons "bash" (json-args "command" "echo hi"))
                                      (cons "bash" (json-args "command" "echo bg"
                                                              "run_in_background" t))
                                      (cons "job_output" (json-args "job_id" "job-1"
                                                                    "wait_ms" 20000))
                                      "done")
                                (list nil)
                                :permissions +allow-shell+)
                   "go"))
             (results (tool-results run)))
        (ok (eq :stop (a:agent-run-finish-reason run)))
        (ok (= 3 (length results)))
        (ok (notany #'result-error-p results))
        (ok (equal (exit-text "hi" 0) (result-content (first results))))
        (ok (equal (started-text "job-1" "echo bg") (result-content (second results))))
        (ok (equal (job-text "bg" "[job job-1 exited with code 0]")
                   (result-content (third results))))))))

(deftest shell-agent-run-deny-rule
  (with-agent-loop
    (with-shell (shell root :auto) '(("keep.txt" "k"))
      (let* ((run (a:run-ai-agent
                   (shell-agent (list shell)
                                (list (cons "bash" (json-args "command" "rm keep.txt"))
                                      "done")
                                (list nil)
                                :permissions (list (a:make-permission-rule "*" "*" :allow)
                                                   (a:make-permission-rule
                                                    "bash" "program:rm" :deny
                                                    :feedback "do not delete files")))
                   "go"))
             (results (tool-results run)))
        (ok (= 1 (length results)))
        (ok (has "was not run" (result-content (first results))))
        (ok (has "do not delete files" (result-content (first results))))
        (ok (exists-at root "keep.txt"))))))

(deftest shell-agent-run-timeout-is-a-tool-result
  (with-agent-loop
    (with-shell (shell root :auto :needs-approval nil) '()
      (multiple-value-bind (seconds run)
          (timed (a:run-ai-agent
                  (shell-agent (list shell)
                               (list (cons "bash" (json-args "command" (sleep-command 30)
                                                             "timeout_ms" 500))
                                     (cons "job_list" (json-args))
                                     "done")
                               (list nil))
                  "go"))
        (let ((results (tool-results run)))
          (ok (eq :stop (a:agent-run-finish-reason run)) "a timeout does not abort the run")
          (ok (< seconds 15))
          (ok (= 2 (length results)))
          (ok (result-error-p (first results)))
          (ok (has "Tool bash timed out" (result-content (first results))))
          (ok (has "run_in_background" (result-content (first results))))
          (ok (equal "[no jobs]" (result-content (second results)))
              "the timed-out command did not become a job"))))))

(deftest shell-agent-run-failures-are-tool-results
  (with-agent-loop
    (with-shell (shell root :auto :needs-approval nil) '()
      (let* ((run (a:run-ai-agent
                   (shell-agent (list shell)
                                (list (cons "bash" (json-args "command" "echo x"
                                                              "workdir" "../.."))
                                      (cons "job_kill" (json-args "job_id" "job-7"))
                                      "done")
                                (list nil))
                   "go"))
             (results (tool-results run)))
        (ok (eq :stop (a:agent-run-finish-reason run)))
        (ok (= 2 (length results)))
        (ok (every #'result-error-p results))
        (ok (has "Tool bash rejected its arguments" (result-content (first results))))
        (ok (has "outside" (result-content (first results))))
        (ok (has "job-7" (result-content (second results))))))))
