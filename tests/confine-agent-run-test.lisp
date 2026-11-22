(in-package #:agent-tools/tests)

;;; R4d-4a in a real agent run (mock LLM backend, libuv loop): a refused
;;; confinement is a tool result, and the run goes on; a confined command runs.

(deftest confined-agent-run-refusal-is-a-result
  (with-agent-loop
    (with-confined-shell (shell root :auto (make-recording-backend :features '(:filesystem)))
        '()
      (let* ((run (a:run-ai-agent
                   (shell-agent (list shell)
                                (list (cons "bash" (json-args "command" (ran-marker-command root "ran.txt")))
                                      "done")
                                (list nil)
                                :permissions +allow-shell+)
                   "go"))
             (results (tool-results run)))
        (ok (eq :stop (a:agent-run-finish-reason run)) "the run goes on")
        (ok (= 1 (length results)))
        (when results
          (ok (result-error-p (first results)))
          (ok (has "Tool bash was not run" (result-content (first results))))
          (ok (has "refused" (result-content (first results))))
          (ok (has "never run unconfined" (result-content (first results)))))
        (sleep 0.3)
        (ok (not (exists-at root "ran.txt")) "nothing ran")))))

(deftest confined-agent-run-bash
  (with-agent-loop
    (with-confined-shell (shell root :auto (sc:make-model-confinement-backend)) '()
      (let* ((run (a:run-ai-agent
                   (shell-agent (list shell)
                                (list (cons "bash" (json-args "command" "echo hi")) "done")
                                (list nil)
                                :permissions +allow-shell+)
                   "go"))
             (results (tool-results run)))
        (ok (= 1 (length results)))
        (when results
          (ok (not (result-error-p (first results))))
          (ok (equal (exit-text "hi" 0) (result-content (first results)))))))))
