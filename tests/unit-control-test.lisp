(in-package #:agent-tools/tests)

;;; H8c unit_control (stop / restart / input) and the permission resources of
;;; every unit tool.

(deftest unit-permission-resources
  (with-units (tools sup)
    (flet ((res (name &rest kv) (a:tool-permission-resources tools name (apply #'json-args kv))))
      (ok (equal '("stop:shell-job-3") (res "unit_control" "unit" "shell-job-3" "action" "stop")))
      (ok (equal '("restart:rpc-1") (res "unit_control" "unit" "rpc-1" "action" "restart")))
      (ok (equal '("input:shell-job-2") (res "unit_control" "unit" "shell-job-2" "action" "input" "input" "y")))
      (ok (equal '("unit:shell-job-3") (res "unit_read" "ref" "shell-job-3:2:1045")))
      (ok (equal '("unit:shell-job-3") (res "unit_read" "unit" "shell-job-3")))
      (ok (equal '("unit:rpc-1") (res "unit_tail" "unit" "rpc-1")))
      (ok (equal '("unit:*") (res "unit_search" "query" "x")))
      (ok (equal '("unit:shell-job-*" "unit:rpc-1")
                 (res "unit_search" "query" "x" "units" #("shell-job-*" "rpc-1"))))
      (ok (equal '("*") (res "unit_list")))
      (ok (equal '("*") (res "unit_control" "action" "stop")) "no unit")
      (ok (equal '("*") (a:tool-permission-resources tools "unit_control" "{not json")) "never signals")
      (ok (equal '("*") (res "unit_read" "ref" 42))))))

(deftest unit-control-permission-rules
  (with-units (tools sup)
    (let ((rules (list (a:make-permission-rule "unit_control" "stop:shell-job-*" :allow)
                       (a:make-permission-rule "unit_control" "*:subagent-*" :deny))))
      (flet ((effect (&rest kv)
               (a:permission-rules-effect rules "unit_control"
                                          (a:tool-permission-resources tools "unit_control"
                                                                       (apply #'json-args kv)))))
        (ok (eq :allow (effect "unit" "shell-job-4" "action" "stop")) "stopping shell jobs is allowed")
        (ok (null (effect "unit" "shell-job-4" "action" "restart")) "restart: no rule, so the tool asks")
        (ok (null (effect "unit" "rpc-1" "action" "stop")))
        (ok (eq :deny (effect "unit" "subagent-1" "action" "input")))))))

(deftest unit-control-stop
  (with-units (tools sup)
    (let* ((u (sup:start-unit sup :shell-job :command (sleeper-argv 60)))
           (id (sup:unit-id u)))
      (ok (equal (format nil "Stopped ~a: killed" id)
                 (ucall tools "unit_control" "unit" id "action" "stop")))
      (ok (eq :killed (sup:unit-state u)))
      (ok (equal (format nil "~a had already finished: killed" id)
                 (ucall tools "unit_control" "unit" id "action" "stop"))
          "not an error"))))

(deftest unit-control-restart
  (with-units (tools sup)
    (let* ((u (thread-lines sup '("first run")))
           (id (sup:unit-id u))
           (before (sup:log-last-line u)))
      (ok (equal (format nil "Restarted ~a: running; its output continues after line ~d (unit_tail after_line ~d)"
                         id before before)
                 (ucall tools "unit_control" "unit" id "action" "restart")))
      (settle u)
      (ok (equal '("first run" "first run") (mapcar #'sup:log-record-text (sup:log-read u)))
          "the same unit ran again; one log"))))

(deftest unit-control-input
  (with-units (tools sup)
    (let* ((u (sup:start-unit sup :rpc :command (echo-argv) :restart :never))
           (id (sup:unit-id u))
           (before (sup:log-last-line u)))
      (ok (equal (format nil "Sent 3 characters to ~a, then closed its stdin; read the reply with unit_tail after_line ~d"
                         id before)
                 (ucall tools "unit_control" "unit" id "action" "input" "input" (format nil "hi~%") "eof" t)))
      (ok (has "hi" (ucall tools "unit_tail" "unit" id "after_line" before "wait_ms" 20000)))
      (ok (equal '(:exited 0) (multiple-value-list (sup:wait-unit u :timeout 20)))
          "end of file reached the program"))))

(deftest unit-control-errors
  (with-units (tools sup)
    (let ((done (thread-lines sup '("x")))
          (thread (sup:start-unit sup :subagent :function (lambda (u) (declare (ignore u)) (sleep 30) :done))))
      (flet ((err (&rest kv) (failure (apply #'ucall tools "unit_control" kv))))
        (ok (typep (err "unit" "shell-job-99" "action" "stop") 'unit-not-found))
        (ok (typep (err "unit" (sup:unit-id done) "action" "jump") 'a:tool-input-error))
        (ok (typep (err "unit" (sup:unit-id done) "action" "input") 'a:tool-input-error) "input needs input")
        (ok (has "not running" (message (err "unit" (sup:unit-id done) "action" "input" "input" "y"))))
        (ok (has "does not take input" (message (err "unit" (sup:unit-id thread) "action" "input" "input" "y"))))
        (sup:stop-unit thread :grace 1)))))

(deftest unit-control-scope-and-previous
  (call-with-state-dir
   (lambda (dir)
     (let (old-key)
       (let ((old (sup:make-supervisor :exit-hook nil :state-directory dir)))
         (setf old-key (format nil "~a--~a" (sup:supervisor-id old) (sup:unit-id (thread-lines old '("old")))))
         (quiet-shutdown old))
       (let* ((sup (sup:make-supervisor :exit-hook nil :state-directory dir))
              (mine (sup:make-scope sup :agent "mine"))
              (other (sup:make-scope sup :agent "other"))
              (theirs (sup:start-unit sup :shell-job :command (sleeper-argv 60) :owner other))
              (tools (make-unit-tools :supervisor sup :scope mine)))
         (unwind-protect
              (progn
                (ok (typep (failure (ucall tools "unit_control" "unit" (sup:unit-id theirs) "action" "stop"))
                           'unit-not-found)
                    "a unit outside the scope is not visible")
                (ok (eq :running (sup:unit-state theirs)))
                (ok (has "previous session"
                         (message (failure (ucall tools "unit_control" "unit" old-key "action" "stop")))))
                (ok (has "old" (ucall tools "unit_tail" "unit" old-key)) "but its log can be read"))
           (ignore-errors (close-unit-tools tools))
           (quiet-shutdown sup)))))))
