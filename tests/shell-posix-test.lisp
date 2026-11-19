(in-package #:agent-tools/tests)

;;; /bin/sh behaviour on both executors. Unix only: this file is
;;; :if-feature (:not (:or :windows :win32)).

(deftest-executors bash-quoting-reaches-the-shell
  (with-shell (tools root executor) '()
    (ok (equal (exit-text "a b|c" 0) (bash tools "printf '%s|%s\\n' 'a b' \"c\"")))
    (ok (equal (exit-text "it's" 0) (bash tools "echo \"it's\"")))))

(deftest-executors bash-output-is-cleaned
  (with-shell (tools root executor) '()
    (ok (equal (exit-text "red plain" 0)
               (bash tools "printf '\\033[31mred\\033[0m plain\\n'"))
        "ANSI escapes removed")
    (ok (equal (exit-text "100%" 0) (bash tools "printf '10%%\\r50%%\\r100%%\\n'"))
        "carriage-return progress keeps the last state")
    (ok (equal (exit-text (format nil "caf~c" (code-char #xE9)) 0)
               (bash tools "printf 'caf\\303\\251\\n'")))
    (ok (equal (exit-text (format nil "a~cb" (code-char #xFFFD)) 0)
               (bash tools "printf 'a\\377b\\n'")))
    (ok (equal (exit-text (format nil "a~%~%b") 0) (bash tools "printf 'a\\n\\nb\\n\\n\\n'"))
        "final newlines are dropped, inner blank lines kept")))

(deftest-executors bash-terminal-or-pipe
  (with-shell (tools root executor) '()
    (ok (equal (exit-text (if (eq executor :pty) "tty" "pipe") 0)
               (bash tools "if [ -t 1 ]; then echo tty; else echo pipe; fi")))
    (ok (equal (exit-text "eof" 0) (bash tools "read x || echo eof"))
        "foreground stdin is at end of file on both executors")))

(deftest-executors bash-background-child-does-not-block
  (with-shell (tools root executor) '()
    (multiple-value-bind (seconds r) (timed (bash tools "sleep 30 & echo done"))
      (ok (equal (exit-text "done" 0) r))
      (ok (< seconds 10) "the call returns when the shell exits"))))

(deftest-executors bash-script-in-workdir
  (with-shell (tools root executor) '(("sub/run.sh" "echo from-script"))
    (ok (equal (exit-text "from-script" 0) (bash tools "sh run.sh" "workdir" "sub")))
    (ok (equal (exit-text (p:as-posix (p:join root "sub")) 0)
               (bash tools "pwd -P" "workdir" "sub")))))

(deftest-executors job-input-reaches-the-job
  (with-shell (tools root executor) '()
    (bash tools "read x; echo \"got $x\"" "run_in_background" t)
    (let ((r (shell tools "job_input" "job_id" "job-1" "input" (nl "abc") "wait_ms" 20000)))
      (ok (has "got abc" r) "job_input returns output like job_output")
      (ok (equal "[job job-1 exited with code 0]" (last-line r))))))

(deftest-executors job-input-then-output
  (with-shell (tools root executor) '()
    (bash tools "read a; read b; echo \"$a+$b\"" "run_in_background" t)
    (let ((r (shell tools "job_input" "job_id" "job-1" "input" (nl "1"))))
      (ok (equal "[job job-1 running]" (last-line r))))
    (shell tools "job_input" "job_id" "job-1" "input" (nl "2"))
    (let ((r (job-output tools "job-1" "wait_ms" 20000)))
      (ok (has "1+2" r))
      (ok (equal "[job job-1 exited with code 0]" (last-line r))))))

(deftest-executors job-output-is-incremental
  (with-shell (tools root executor) '()
    (bash tools "echo one; sleep 3; echo two" "run_in_background" t)
    (ok (equal (job-text "one" "[job job-1 running]")
               (job-output tools "job-1" "wait_ms" 1500))
        "wait_ms elapses while the job runs")
    (ok (equal (job-text "two" "[job job-1 exited with code 0]")
               (job-output tools "job-1" "wait_ms" 20000))
        "only the new output, returned as soon as the job exits")))
