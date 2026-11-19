(defsystem "agent-tools"
  :version "0.1.1"
  :description "Coding agent file tools (read, write, edit, multi_edit, glob, grep, ls) as an ai-agent-protocol tool source over any cl-stack-pathlib filesystem"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "ai-agent-protocol" "0.10.0")
               (:version "cl-stack-pathlib" "0.4.0")
               (:version "diff-protocol" "0.2.0")
               "json-protocol"
               "json-backend-jzon")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "file-tools")
               (:file "source"))
  :in-order-to ((test-op (test-op "agent-tools/tests"))))

(defsystem "agent-tools/tests"
  :depends-on ("agent-tools"
               "ai-agent-protocol"
               "llm-protocol"
               "cl-stack-pathlib"
               "diff-protocol"
               "json-protocol"
               "event-backend-libuv"
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
               (:file "agent-run-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
