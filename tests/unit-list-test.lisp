(in-package #:agent-tools/tests)

;;; H8c unit_list: rows, filters, previous sessions, truncation, scope.

(defun list-fixture (sup)
  "session s › agent a. A finished thread unit owned by a (labels tool=demo), a
running sleeper owned by s (labels tool=bash), a process that failed with 3.
→ (values thread sleeper failed agent-scope)."
  (let* ((s (sup:make-scope sup :session "s"))
         (a (sup:make-scope sup :agent "a" :parent s))
         (thread (thread-lines sup '("one" "two" "three") :kind :subagent :owner a
                                                         :labels '(:tool "demo")))
         (sleeper (sup:start-unit sup :shell-job :command (sleeper-argv 60) :owner s
                                                 :labels '(:tool "bash")))
         (failed (settle (sup:start-unit sup :shell-job :command (exit-argv 3) :owner s))))
    (values thread sleeper failed a)))

(deftest unit-list-rows
  (with-units (tools sup)
    (multiple-value-bind (thread sleeper failed) (list-fixture sup)
      (ok (equal (format nil "~a~%~a~%~a~%~a" +unit-list-header+
                         (expected-row thread) (expected-row sleeper) (expected-row failed))
                 (ucall tools "unit_list"))
          "header, then every unit in start order")
      (ok (equal "subagent-1" (sup:unit-id thread)))
      (ok (has (tsv "subagent-1" "subagent" "exited" "s/a") (ucall tools "unit_list")))
      (ok (has (tsv "failed 3" "s") (ucall tools "unit_list"))))))

(deftest unit-list-filters
  (with-units (tools sup)
    (multiple-value-bind (thread sleeper failed) (list-fixture sup)
      (flet ((rows (&rest kv) (rest (result-lines (apply #'ucall tools "unit_list" kv)))))
        (ok (equal (list (expected-row sleeper) (expected-row failed)) (rows "kind" "shell-job")))
        (ok (equal (list (expected-row sleeper)) (rows "state" "live")))
        (ok (equal (list (expected-row thread) (expected-row failed)) (rows "state" "finished")))
        (ok (equal (list (expected-row failed)) (rows "state" "failed")))
        (ok (equal (list (expected-row thread)) (rows "owner" "s/*")))
        (ok (equal (list (expected-row sleeper) (expected-row failed)) (rows "owner" "s")))
        (ok (equal (list (expected-row sleeper)) (rows "label" (args "tool" "bash"))))
        (ok (equal (list (expected-row sleeper)) (rows "kind" "shell-job" "state" "running")))
        (ok (equal "[no units]" (ucall tools "unit_list" "kind" "lsp")))))))

(deftest unit-list-argument-errors
  (with-units (tools sup)
    (ok (typep (failure (ucall tools "unit_list" "state" "sleeping")) 'a:tool-input-error))
    (ok (typep (failure (ucall tools "unit_list" "kind" 3)) 'a:tool-input-error))
    (ok (typep (failure (ucall tools "unit_list" "label" "tool=bash")) 'a:tool-input-error))
    (ok (has "state" (message (failure (ucall tools "unit_list" "state" "sleeping")))))))

(deftest unit-list-previous-sessions
  (call-with-state-dir
   (lambda (dir)
     (let (old-key old-last old-start)
       (let ((old (sup:make-supervisor :exit-hook nil :state-directory dir)))
         (let ((u (thread-lines old '("from before"))))
           (setf old-key (format nil "~a--~a" (sup:supervisor-id old) (sup:unit-id u))
                 old-last (sup:log-last-line u)
                 old-start (sup:log-record-wall-time (first (sup:log-read u :types t)))))
         (quiet-shutdown old))
       (let* ((sup (sup:make-supervisor :exit-hook nil :state-directory dir))
              (tools (make-unit-tools :supervisor sup)))
         (unwind-protect
              (let ((now (thread-lines sup '("now"))))
                (ok (equal (format nil "~a~%~a" +unit-list-header+ (expected-row now))
                           (ucall tools "unit_list"))
                    "previous sessions only on request")
                (ok (equal (format nil "~a~%~a~%~a" +unit-list-header+ (expected-row now)
                                   (tsv old-key "shell-job" "previous" "-"
                                        (rfc3339 (+ (floor old-start 1000) (encode-universal-time 0 0 0 1 1 1970 0)))
                                        old-last "-" "-"))
                           (ucall tools "unit_list" "include_previous" t)))
                (ok (equal (format nil "~a~%~a" +unit-list-header+ (expected-row now))
                           (ucall tools "unit_list" "include_previous" t "state" "finished"))
                    "a state filter leaves previous logs out"))
           (ignore-errors (close-unit-tools tools))
           (quiet-shutdown sup)))))))

(deftest unit-list-truncation
  (with-units (tools sup :tool-keys (:max-results 2))
    (let ((us (loop for i below 3 collect (thread-lines sup (list (format nil "u~d" i))))))
      (ok (equal (format nil "~a~%~a~%~a~%[truncated: showing 2 of 3 units; filter by kind, state, owner or label]"
                         +unit-list-header+ (expected-row (first us)) (expected-row (second us)))
                 (ucall tools "unit_list")))))
  (with-units (tools sup :tool-keys (:max-output 200))
    (dotimes (i 6) (thread-lines sup (list (format nil "u~d" i))))
    (let ((r (ucall tools "unit_list")))
      (ok (<= (length r) 200) "never over max-output")
      (ok (has "[truncated: showing " r))
      (ok (has " of 6 units; filter by kind, state, owner or label]" r)))))

(deftest unit-list-scope
  (with-units (tools sup)
    (multiple-value-bind (thread sleeper failed agent) (list-fixture sup)
      (declare (ignore sleeper failed))
      (let ((scoped (make-unit-tools :supervisor sup :scope agent)))
        (unwind-protect
             (ok (equal (format nil "~a~%~a" +unit-list-header+ (expected-row thread))
                        (ucall scoped "unit_list"))
                 "only units the scope owns, directly or below")
          (ignore-errors (close-unit-tools scoped)))))))
