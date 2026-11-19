(defsystem "agent-tools"
  :version "0.2.0"
  :description "Coding agent tools as ai-agent-protocol tool sources: files (read, write, edit, multi_edit, glob, grep, ls) over any cl-stack-pathlib filesystem, and shell (bash, background jobs, command scanner for permissions)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "ai-agent-protocol" "0.10.0")
               (:version "cl-stack-pathlib" "0.4.0")
               (:version "diff-protocol" "0.2.0")
               "json-protocol"
               "json-backend-jzon"
               (:version "process-protocol" "0.3.0")
               (:version "process-backend-uiop" "0.2.0")
               (:version "pty-protocol" "0.1.0")
               (:version "pty-backend-native" "0.2.1"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "file-tools")
               (:file "source")
               (:file "shell-output")
               (:file "shell-scan")
               (:file "shell-tools")
               (:file "shell-run")
               (:file "shell-source"))
  :in-order-to ((test-op (test-op "agent-tools/tests"))))

(defsystem "agent-tools/tests"
  :depends-on ("agent-tools"
               "ai-agent-protocol"
               "llm-protocol"
               "cl-stack-pathlib"
               "diff-protocol"
               "json-protocol"
               "event-backend-libuv"
               "process-protocol"
               "pty-protocol"
               "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "support")
               (:module "fixtures"
                :components ((:static-file "crlf.txt")))
               (:file "construct-test")
               (:file "schema-test")
               (:file "paths-test")
               (:file "symlink-test" :if-feature (:not (:or :windows :win32)))
               (:file "read-test")
               (:file "write-test")
               (:file "edit-test")
               (:file "multi-edit-test")
               (:file "glob-test")
               (:file "grep-test")
               (:file "ls-test")
               (:file "permissions-test")
               (:file "agent-run-test")
               (:file "shell-support")
               (:file "shell-construct-test")
               (:file "shell-scan-test")
               (:file "shell-output-test")
               (:file "shell-test")
               (:file "shell-posix-test" :if-feature (:not (:or :windows :win32)))
               (:file "shell-pty-test" :if-feature (:not (:or :windows :win32)))
               (:file "shell-permissions-test")
               (:file "shell-agent-run-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
