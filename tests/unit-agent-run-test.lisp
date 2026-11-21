(in-package #:agent-tools/tests)

;;; H8c: UNIT-TOOLS inside a real agent run (mock LLM backend, libuv loop).

(deftest unit-agent-run-reads-and-searches
  (with-agent-loop
    (with-units (tools sup)
      (let* ((u (thread-lines sup +build-lines+))
             (run (a:run-ai-agent
                   (a:make-ai-agent :name "coder"
                                    :backend (scripted-backend
                                              (list (cons "unit_list" (json-args))
                                                    (cons "unit_search" (json-args "query" "undeclared identifier"))
                                                    (cons "unit_tail" (json-args "unit" (sup:unit-id u) "lines" 2))
                                                    "done")
                                              (list nil))
                                    :tools (list tools))
                   "go"))
             (results (tool-results run)))
        (ok (eq :stop (a:agent-run-finish-reason run)))
        (ok (= 3 (length results)))
        (ok (notany #'result-error-p results) "reading needs no approval")
        (ok (has (sup:unit-id u) (result-content (first results))))
        (ok (has "[1] " (result-content (second results))))
        (ok (has "end of output" (result-content (third results))))))))

(deftest unit-agent-run-control-rules
  (with-agent-loop
    (with-units (tools sup)
      (let* ((job (sup:start-unit sup :shell-job :command (sleeper-argv 60)))
             (sub (sup:start-unit sup :subagent :function (lambda (u) (declare (ignore u)) (sleep 60) :done)))
             (run (a:run-ai-agent
                   (a:make-ai-agent :name "coder"
                                    :backend (scripted-backend
                                              (list (cons "unit_control" (json-args "unit" (sup:unit-id sub) "action" "stop"))
                                                    (cons "unit_control" (json-args "unit" (sup:unit-id job) "action" "stop"))
                                                    "done")
                                              (list nil))
                                    :tools (list tools)
                                    :permissions (list (a:make-permission-rule "unit_control" "stop:shell-job-*" :allow)
                                                       (a:make-permission-rule "unit_control" "*:subagent-*" :deny
                                                                               :feedback "leave subagents alone")))
                   "go"))
             (results (tool-results run)))
        (ok (= 2 (length results)))
        (ok (has "leave subagents alone" (result-content (first results))) "denied by rule")
        (ok (eq :running (sup:unit-state sub)))
        (ok (equal (format nil "Stopped ~a: killed" (sup:unit-id job)) (result-content (second results)))
            "allowed by rule, so no approval was asked")
        (ok (eq :killed (sup:unit-state job)))
        (sup:stop-unit sub :grace 1)))))
