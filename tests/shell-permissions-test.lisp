(in-package #:agent-tools/tests)

;;; TOOL-PERMISSION-RESOURCES, TOOL-NEEDS-APPROVAL-P, TOOL-EXECUTABLE-P and
;;; TOOL-INSTRUCTION-PATHS of a SHELL-TOOLS source, and rules over them.

(defun shell-resources (tools name arguments)
  (a:tool-permission-resources tools name arguments))

(defun rules-effect (rules tools command &rest kv)
  (values (a:permission-rules-effect
           (a:coerce-permission-rules rules) "bash"
           (shell-resources tools "bash" (apply #'json-args "command" command kv)))))

(deftest shell-resources-for-bash
  (with-shell (tools root :auto) '("src/")
    (ok (equal '("bash:git status" "program:git")
               (shell-resources tools "bash" (json-args "command" "git status"))))
    (ok (equal '("bash:git status" "program:git")
               (shell-resources tools "bash" (args "command" "git status")))
        "hash-table arguments")
    (ok (equal '("cd:src" "bash:git status" "program:git")
               (shell-resources tools "bash" (json-args "command" "git status" "workdir" "src"))))
    (ok (equal '("cd:src" "bash:git status" "program:git")
               (shell-resources tools "bash" (json-args "command" "git status"
                                                        "workdir" "./src/"))))
    (ok (equal '("bash:git status" "program:git")
               (shell-resources tools "bash" (json-args "command" "git status"
                                                        "run_in_background" t))))
    #-(or windows win32)
    (ok (equal '("bash:echo x" "program:echo" "write:src/a.txt")
               (shell-resources tools "bash"
                                (json-args "command" (format nil "echo x > ~a/src/a.txt"
                                                             (string-right-trim
                                                              "/" (p:as-posix root))))))
        "absolute paths under the root are root-relative")))

(deftest shell-resources-never-signal
  (with-shell (tools root :auto) '()
    (dolist (arguments (list (json-args) (json-args "command" "") (json-args "command" 42)
                             (json-args "command" "ls" "workdir" "../elsewhere")
                             "{\"command\": " "[1]" 42 nil))
      (ok (equal '("*") (handler-case (shell-resources tools "bash" arguments)
                          (error () :error)))
          (format nil "~s" arguments)))))

(deftest shell-resources-for-jobs
  (with-shell (tools root :auto) '()
    (dolist (name '("job_output" "job_kill"))
      (ok (equal '("job:job-3") (shell-resources tools name (json-args "job_id" "job-3"))) name))
    (ok (equal '("job:job-3")
               (shell-resources tools "job_input" (json-args "job_id" "job-3" "input" "y"))))
    (ok (equal '("*") (shell-resources tools "job_output" (json-args))))
    (ok (equal '("*") (shell-resources tools "job_list" (json-args))))))

(deftest shell-approval-and-executable
  (with-shell (tools root :auto) '()
    (ok (a:tool-needs-approval-p tools "bash" (json-args "command" "ls")))
    (ok (a:tool-needs-approval-p tools "job_input" (json-args "job_id" "job-1" "input" "x")))
    (dolist (name '("job_output" "job_kill" "job_list"))
      (ok (not (a:tool-needs-approval-p tools name (json-args "job_id" "job-1"))) name))
    (dolist (name +shell-tool-names+)
      (ok (a:tool-executable-p tools name) name))
    (ok (not (a:tool-executable-p tools "read"))))
  (with-shell (tools root :auto :needs-approval nil :tools '("bash")) '()
    (ok (not (a:tool-needs-approval-p tools "bash" (json-args "command" "ls"))))
    (ok (not (a:tool-executable-p tools "job_output")))))

(deftest shell-instruction-paths
  (with-shell (tools root :auto) '("src/")
    (ok (equal '(".") (a:tool-instruction-paths tools "bash" (json-args "command" "ls"))))
    (ok (equal '("src") (a:tool-instruction-paths tools "bash"
                                                  (json-args "command" "ls" "workdir" "src/"))))
    (ok (null (a:tool-instruction-paths tools "bash"
                                        (json-args "command" "ls" "workdir" "../x"))))
    (ok (null (a:tool-instruction-paths tools "job_output" (json-args "job_id" "job-1"))))))

(deftest shell-rules-over-scanner-output
  (with-shell (tools root :auto) '()
    (let ((git '(("bash" "bash:git *" :allow) ("bash" "program:git" :allow))))
      (ok (eq :allow (rules-effect git tools "git status")))
      (ok (null (rules-effect git tools "git status && rm -rf x"))
          "an undecided command leaves the call to the tool default (ask)")
      (ok (null (rules-effect git tools "git log $(rm -rf x)"))
          "a dynamic construct is never allowed by command rules")
      (ok (eq :deny (rules-effect (append git '(("bash" "program:rm" :deny)))
                                  tools "rm x")))
      #-(or windows win32)
      (progn
        (ok (eq :allow (rules-effect git tools "git status && git diff")))
        (ok (eq :deny (rules-effect (append git '(("bash" "program:rm" :deny)))
                                    tools "git status; rm x")))))
    (let ((all '(("bash" "*" :allow) ("bash" "bash:rm *" :deny))))
      (ok (eq :allow (rules-effect all tools "echo hi")))
      (ok (eq :deny (rules-effect all tools "rm -f x")))
      #-(or windows win32)
      (ok (eq :deny (rules-effect all tools "echo a && rm -f x"))))
    #-(or windows win32)
    (let ((secret '(("bash" "*" :allow) ("bash" "write:secret/*" :deny))))
      (ok (eq :deny (rules-effect secret tools "echo k > secret/k.txt")))
      (ok (eq :deny (rules-effect secret tools "cd secret && echo k > k.txt")))
      (ok (eq :deny (rules-effect secret tools "touch k.txt" "workdir" "secret")))
      (ok (eq :deny (rules-effect secret tools "rm -f secret/k.txt")))
      (ok (eq :allow (rules-effect secret tools "echo k > ok.txt"))))))
