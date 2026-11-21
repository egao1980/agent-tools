(in-package #:agent-tools/tests)

;;; H8c unit_tail: the last lines, following with after_line + wait_ms, the
;;; status footer.

(deftest unit-tail-last-lines
  (with-units (tools sup)
    (let* ((u (thread-lines sup (numbered-texts 30)))
           (id (sup:unit-id u)))
      (ok (equal (format nil "~a~%~a~%~a~%[unit ~a exited; end of output]"
                         (log-line " " 29 "text line 028") (log-line " " 30 "text line 029")
                         (log-line " " 31 "text line 030") id)
                 (ucall tools "unit_tail" "unit" id "lines" 3)))
      (ok (equal (format nil "[unit ~a exited; end of output]" id)
                 (ucall tools "unit_tail" "unit" id "after_line" 31))
          "nothing after the last line of a closed log"))))

(deftest unit-tail-follows-a-running-unit
  (with-units (tools sup)
    (let* ((gate (list nil))
           (u (gated-lines sup '("before") gate '("after the gate")))
           (id (sup:unit-id u)))
      (ok (wait-for (lambda () (eql 2 (ignore-errors (sup:log-last-line u))))))
      (ok (equal (format nil "~a~%[unit ~a running; continue with after_line 2]" (log-line " " 2 "before") id)
                 (ucall tools "unit_tail" "unit" id)))
      (bt2:make-thread (lambda () (sleep 0.3) (setf (car gate) t)) :name "open the gate")
      (multiple-value-bind (seconds result)
          (timed (ucall tools "unit_tail" "unit" id "after_line" 2 "wait_ms" 20000))
        (ok (< seconds 10) "returns when the line arrives, not at the end of the wait")
        (ok (equal (log-line " " 3 "after the gate") (first (result-lines result))))
        (ok (has (format nil "[unit ~a " id) (car (last (result-lines result))))))
      (settle u)
      (ok (equal (format nil "[unit ~a exited; end of output]" id)
                 (ucall tools "unit_tail" "unit" id "after_line" 3 "wait_ms" 5000))))))

(deftest unit-tail-wait-expires
  (with-units (tools sup)
    (let* ((u (sup:start-unit sup :shell-job :command (sleeper-argv 60)))
           (id (sup:unit-id u)))
      (multiple-value-bind (seconds result)
          (timed (ucall tools "unit_tail" "unit" id "after_line" 1 "wait_ms" 500))
        (ok (>= seconds 0.4) "waited")
        (ok (< seconds 5))
        (ok (equal (format nil "[unit ~a running; continue with after_line 1]" id) result))))))

(deftest unit-tail-statuses
  (with-units (tools sup)
    (let ((failed (settle (sup:start-unit sup :shell-job :command (exit-argv 3))))
          (killed (sup:start-unit sup :shell-job :command (sleeper-argv 60))))
      (sup:stop-unit killed :grace 1)
      (settle killed)
      (ok (equal (format nil "[unit ~a failed with code 3; end of output]" (sup:unit-id failed))
                 (ucall tools "unit_tail" "unit" (sup:unit-id failed))))
      (ok (equal (format nil "[unit ~a killed; end of output]" (sup:unit-id killed))
                 (ucall tools "unit_tail" "unit" (sup:unit-id killed)))))))

(deftest unit-tail-errors
  (with-units (tools sup)
    (let ((u (thread-lines sup '("x"))))
      (flet ((err (&rest kv) (failure (apply #'ucall tools "unit_tail" kv))))
        (ok (typep (err) 'a:tool-input-error) "unit is required")
        (ok (typep (err "unit" "nope-1") 'unit-not-found))
        (ok (typep (err "unit" (sup:unit-id u) "wait_ms" 600001) 'a:tool-input-error))
        (ok (typep (err "unit" (sup:unit-id u) "lines" 0) 'a:tool-input-error))
        (ok (typep (err "unit" (sup:unit-id u) "after_line" -1) 'a:tool-input-error))))))
