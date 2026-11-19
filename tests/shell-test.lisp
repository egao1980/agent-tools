(in-package #:agent-tools/tests)

;;; bash and the job tools on every OS, once per executor (:pty and :process
;;; on Unix, :process on Windows). Commands come from PICK: /bin/sh syntax
;;; on Unix, cmd.exe syntax on Windows.

;;; --- foreground ------------------------------------------------------------

(deftest-executors bash-echo
  (with-shell (tools root executor) '()
    (ok (equal (exit-text "hello" 0) (bash tools "echo hello")))))

(deftest-executors bash-exit-code
  (with-shell (tools root executor) '()
    (ok (equal (exit-text "" 3) (bash tools (pick "exit 3" "exit /b 3"))))
    (ok (equal (exit-text "" 0) (bash tools (pick "true" "rem nothing"))) "no output")))

(deftest-executors bash-merges-stderr
  (with-shell (tools root executor) '()
    (let ((r (bash tools (pick "echo out; echo err 1>&2; exit 1"
                               "(echo out)& (echo err)1>&2& exit /b 1"))))
      #-(or windows win32)
      (ok (equal (exit-text (format nil "out~%err") 1) r))
      #+(or windows win32)
      (ok (and (has "out" r) (has "err" r) (equal "[exit code 1]" (last-line r)))))))

(deftest-executors bash-workdir
  (with-shell (tools root executor) '(("sub/marker.txt" "m"))
    (ok (equal (exit-text "marker.txt" 0)
               (bash tools (pick "ls" "dir /b") "workdir" "sub")))
    (ok (equal (exit-text "marker.txt" 0)
               (bash tools (pick "ls" "dir /b") "workdir" "./sub/")))
    (ok (equal (exit-text "sub" 0) (bash tools (pick "ls" "dir /b"))) "default: the root")))

(deftest-executors bash-workdir-errors
  (with-shell (tools root executor) '(("sub/marker.txt" "m"))
    (let ((c (failure (bash tools "echo x" "workdir" "../"))))
      (ok (typep c 'path-outside-root))
      (ok (has "outside" (message c))))
    (let ((c (failure (bash tools "echo x" "workdir" "missing"))))
      (ok (typep c 'a:tool-input-error))
      (ok (has "not found" (message c))))
    (let ((c (failure (bash tools "echo x" "workdir" "sub/marker.txt"))))
      (ok (typep c 'a:tool-input-error))
      (ok (has "not a directory" (message c))))))

(deftest-executors bash-environment
  (with-shell (tools root executor :env '(("AGENT_TOOLS_X" . "42"))) '()
    (ok (equal (exit-text "cat cat dumb 0" 0)
               (bash tools (pick "echo $PAGER $GIT_PAGER $TERM $GIT_TERMINAL_PROMPT"
                                 "echo %PAGER% %GIT_PAGER% %TERM% %GIT_TERMINAL_PROMPT%"))))
    (ok (equal (exit-text "42" 0)
               (bash tools (pick "echo $AGENT_TOOLS_X" "echo %AGENT_TOOLS_X%"))))))

(deftest-executors bash-stdin-is-empty
  (with-shell (tools root executor) '()
    (multiple-value-bind (seconds r)
        (timed (bash tools (pick "cat" "sort") "timeout_ms" 20000))
      (ok (equal (exit-text "" 0) r) "a command reading stdin sees end of file")
      (ok (< seconds 10)))))

(deftest-executors bash-argument-errors
  (with-shell (tools root executor) '()
    (flet ((input-error (arguments &optional needle)
             (let ((c (failure (call-shell-tool tools "bash" arguments))))
               (and (typep c 'a:tool-input-error)
                    (or (null needle) (has needle (message c)))))))
      (ok (input-error (json-args) "command"))
      (ok (input-error (json-args "command" "") "command"))
      (ok (input-error (json-args "command" "   ") "command"))
      (ok (input-error (json-args "command" 42) "command"))
      (ok (input-error (json-args "command" "echo x" "timeout_ms" 0) "timeout_ms"))
      (ok (input-error (json-args "command" "echo x" "timeout_ms" 600001) "timeout_ms"))
      (ok (input-error (json-args "command" "echo x" "timeout_ms" "10") "timeout_ms"))
      (ok (input-error (json-args "command" "echo x" "run_in_background" "yes")
                       "run_in_background"))
      (ok (input-error (json-args "command" "echo x" "workdir" 5) "workdir"))
      (ok (input-error "{\"command\": " ))
      (ok (input-error "[1, 2]")))
    (ok (equal (exit-text "x" 0)
               (call-shell-tool tools "bash" (args "command" "echo x" "extra" 1)))
        "hash-table arguments; unknown keys are ignored")))

;;; --- timeouts --------------------------------------------------------------

(deftest-executors bash-timeout-kills-and-signals
  (with-shell (tools root executor) '()
    (let ((command (pick "echo start; sleep 30" "echo start& ping -n 30 127.0.0.1 >nul")))
      (multiple-value-bind (seconds c)
          (timed (failure (bash tools command "timeout_ms" 1000)))
        (ok (typep c 'shell-timeout))
        (ok (typep c 'a:tool-timeout))
        (ok (< seconds 15) "returns soon after the timeout")
        (when (typep c 'shell-timeout)
          (ok (eql 1000 (shell-timeout-timeout c)))
          (ok (equal command (shell-timeout-command c)))
          (ok (has "start" (shell-timeout-output c)))
          (ok (has "timed out after 1000 ms" (message c)))
          (ok (has "start" (message c)) "the message carries the output so far")
          (ok (has "run_in_background" (message c))))))))

(deftest-executors bash-timeout-continue-restart
  (with-shell (tools root executor) '()
    (let ((signals 0) (restarts nil))
      (ok (equal (exit-text "done" 0)
                 (handler-bind ((shell-timeout
                                  (lambda (c)
                                    (declare (ignore c))
                                    (incf signals)
                                    (setf restarts (list (and (find-restart 'continue) t)
                                                         (and (find-restart 'run-in-background) t)))
                                    (invoke-restart 'continue))))
                   (bash tools (pick "sleep 2; echo done"
                                     "ping -n 3 127.0.0.1 >nul& echo done")
                         "timeout_ms" 500)))
          "CONTINUE waits another timeout_ms")
      (ok (>= signals 1))
      (ok (equal '(t t) restarts) "CONTINUE and RUN-IN-BACKGROUND are offered"))))

(deftest-executors bash-timeout-run-in-background-restart
  (with-shell (tools root executor) '()
    (let ((command (pick "echo start; sleep 2; echo done"
                         "echo start& ping -n 3 127.0.0.1 >nul& echo done")))
      (ok (equal (started-text "job-1" command)
                 (handler-bind ((shell-timeout
                                  (lambda (c)
                                    (declare (ignore c))
                                    (invoke-restart 'run-in-background))))
                   (bash tools command "timeout_ms" 500))))
      (let ((job (find-shell-job tools "job-1")))
        (ok (and job (equal command (shell-job-command job)))))
      (ok (equal (job-text (format nil "start~%done") "[job job-1 exited with code 0]")
                 (job-output tools "job-1" "wait_ms" 20000))
          "the output from before the timeout is kept")
      (let ((job (find-shell-job tools "job-1")))
        (ok (and job (eq :exited (shell-job-status job))))
        (ok (and job (eql 0 (shell-job-exit-code job))))))))

;;; --- output caps -----------------------------------------------------------

(deftest-executors bash-output-truncated
  (with-shell (tools root executor :max-output 200) '()
    (ok (equal (exit-text (trim-final-newlines
                           (truncate-shell-output (numbered-lines 100) 200))
                          0)
               (bash tools (lines-command 100))))))

;;; --- background jobs -------------------------------------------------------

(deftest-executors job-start-and-read
  (with-shell (tools root executor) '()
    (ok (equal (started-text "job-1" "echo bg")
               (bash tools "echo bg" "run_in_background" t)))
    (ok (equal (job-text "bg" "[job job-1 exited with code 0]")
               (job-output tools "job-1" "wait_ms" 20000)))
    (ok (equal "[job job-1 exited with code 0]" (job-output tools "job-1"))
        "output is returned once")
    (let ((job (first (shell-tools-jobs tools))))
      (ok (typep job 'shell-job))
      (ok (and job (equal "job-1" (shell-job-id job))))
      (ok (and job (eq :exited (shell-job-status job))))
      (ok (and job (eql 0 (shell-job-exit-code job))))
      (ok (and job (null (shell-job-signal job)))))))

(deftest-executors job-exit-code
  (with-shell (tools root executor) '()
    (bash tools (pick "exit 4" "exit /b 4") "run_in_background" t)
    (ok (equal "[job job-1 exited with code 4]" (job-output tools "job-1" "wait_ms" 20000)))
    (ok (eql 4 (shell-job-exit-code (find-shell-job tools "job-1"))))))

(deftest-executors job-running-then-killed
  (with-shell (tools root executor) '()
    (let ((command (sleep-command 30)))
      (bash tools command "run_in_background" t)
      (ok (equal "[job job-1 running]" (job-output tools "job-1")))
      (ok (eq :running (shell-job-status (find-shell-job tools "job-1"))))
      (ok (equal (format nil "job-1~crunning~c~a" #\Tab #\Tab command)
                 (shell tools "job_list")))
      (multiple-value-bind (seconds r) (timed (shell tools "job_kill" "job_id" "job-1"))
        (ok (equal "[job job-1 killed]" r))
        (ok (< seconds 10)))
      (ok (eq :killed (shell-job-status (find-shell-job tools "job-1"))))
      (ok (equal "[job job-1 killed]" (job-output tools "job-1")))
      (ok (equal "[job job-1 killed]" (shell tools "job_kill" "job_id" "job-1"))
          "killing a finished job is not an error")
      (ok (equal (format nil "job-1~ckilled~c~a" #\Tab #\Tab command)
                 (shell tools "job_list"))))))

(deftest-executors job-ids-and-list
  (with-shell (tools root executor) '()
    (ok (equal "[no jobs]" (shell tools "job_list")))
    (bash tools "echo one" "run_in_background" t)
    (bash tools (sleep-command 30) "run_in_background" t)
    (job-output tools "job-1" "wait_ms" 20000)
    (ok (equal '("job-1" "job-2") (mapcar #'shell-job-id (shell-tools-jobs tools))))
    (ok (equal (sleep-command 30) (shell-job-command (find-shell-job tools "job-2"))))
    (ok (null (find-shell-job tools "job-3")))
    (ok (equal (format nil "job-1~cexited 0~cecho one~%job-2~crunning~c~a"
                       #\Tab #\Tab #\Tab #\Tab (sleep-command 30))
               (shell tools "job_list")))))

(deftest-executors job-errors
  (with-shell (tools root executor) '()
    (dolist (name '("job_output" "job_kill"))
      (let ((c (failure (shell tools name "job_id" "job-9"))))
        (ok (typep c 'job-not-found) name)
        (ok (and (typep c 'job-not-found) (equal "job-9" (job-not-found-id c))))
        (ok (has "job-9" (message c)))
        (ok (has "job_list" (message c)))))
    (ok (typep (failure (shell tools "job_input" "job_id" "job-9" "input" "x"))
               'job-not-found))
    (ok (typep (failure (shell tools "job_output")) 'a:tool-input-error))
    (ok (typep (failure (shell tools "job_input" "job_id" "job-9")) 'a:tool-input-error))
    (bash tools "echo x" "run_in_background" t)
    (dolist (wait '(-1 600001 "5"))
      (let ((c (failure (job-output tools "job-1" "wait_ms" wait))))
        (ok (and (typep c 'a:tool-input-error) (has "wait_ms" (message c)))
            (format nil "wait_ms ~s" wait))))))

(deftest-executors job-input-after-exit
  (with-shell (tools root executor) '()
    (bash tools "echo x" "run_in_background" t)
    (job-output tools "job-1" "wait_ms" 20000)
    (let ((c (failure (shell tools "job_input" "job_id" "job-1" "input" "y"))))
      (ok (typep c 'job-not-running))
      (ok (and (typep c 'job-not-running) (equal "job-1" (job-not-running-id c))))
      (ok (and (typep c 'job-not-running) (eq :exited (job-not-running-status c))))
      (ok (has "job-1" (message c))))))

(deftest-executors job-limit
  (with-shell (tools root executor :max-jobs 1) '()
    (bash tools (sleep-command 30) "run_in_background" t)
    (let ((c (failure (bash tools "echo two" "run_in_background" t))))
      (ok (typep c 'a:tool-input-error))
      (ok (has "job_kill" (message c))))
    (shell tools "job_kill" "job_id" "job-1")
    (ok (equal (started-text "job-2" "echo two") (bash tools "echo two" "run_in_background" t))
        "a refused start takes no id; a finished job frees its slot")))

(deftest-executors job-output-truncated
  (with-shell (tools root executor :max-output 200) '()
    (bash tools (lines-command 100) "run_in_background" t)
    (ok (equal (job-text (trim-final-newlines (truncate-shell-output (numbered-lines 100) 200))
                         "[job job-1 exited with code 0]")
               (job-output tools "job-1" "wait_ms" 20000)))))

(deftest-executors job-buffer-drops-old-output
  (with-shell (tools root executor :max-job-buffer 100) '()
    (bash tools (lines-command 100) "run_in_background" t)
    (let ((r (job-output tools "job-1" "wait_ms" 20000)))
      (ok (has "[truncated: " r))
      (ok (has "characters dropped" r))
      (ok (has "line100" r) "the newest output is kept")
      (ok (not (has (format nil "line1~%") r)) "the oldest is dropped")
      (ok (< (length r) 400))
      (ok (equal "[job job-1 exited with code 0]" (last-line r))))))

(deftest-executors close-kills-running-jobs
  (with-shell (tools root executor) '()
    (bash tools (sleep-command 30) "run_in_background" t)
    (multiple-value-bind (seconds v) (timed (close-shell-tools tools))
      (ok (null v))
      (ok (< seconds 10)))
    (ok (eq :killed (shell-job-status (find-shell-job tools "job-1"))))
    (ok (typep (failure (shell tools "job_list")) 'a:tool-unavailable))))

(deftest-executors shell-spawn-failure
  (with-shell (tools root executor :shell (pick '("/nonexistent/agent-tools-sh" "-c")
                                                '("C:\\nonexistent\\agent-tools-sh.exe" "/c")))
      '()
    (dolist (background '(nil t))
      (let ((c (failure (bash tools "echo x" "run_in_background" background))))
        (ok (typep c 'a:tool-unavailable) (format nil "background ~a" background))
        (ok (has "agent-tools-sh" (message c)))))
    (ok (null (shell-tools-jobs tools)) "a failed start is not a job")))
