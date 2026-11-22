(defsystem "agent-tools"
  :version "0.4.0"
  :description "Coding agent tools as ai-agent-protocol tool sources: files (read, write, edit, multi_edit, glob, grep, ls) over any cl-stack-pathlib filesystem, shell (bash, background jobs, command scanner for permissions), and supervisor units (list, search, read, tail, control their logs); shell commands optionally confined through a compute-protocol backend"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "ai-agent-protocol" "0.10.0")
               (:version "cl-stack-pathlib" "0.4.0")
               (:version "diff-protocol" "0.2.0")
               "json-protocol"
               "json-backend-jzon"
               (:version "process-protocol" "0.4.0")
               (:version "process-backend-uiop" "0.3.0")
               (:version "pty-protocol" "0.1.0")
               (:version "pty-backend-native" "0.2.1")
               (:version "supervisor-protocol" "0.4.0")
               (:version "supervisor-protocol/search" "0.4.0")
               (:version "compute-protocol" "0.3.2"))
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
               (:file "shell-source")
               (:file "unit-tools")       ; H8c: the source, constants, constructor
               (:file "unit-source")      ; H8c: descriptors, schemas, protocol methods
               (:file "unit-run")         ; H8c: the five tools, shell jobs under a supervisor
               (:file "shell-confine"))   ; R4d-4a: confined bash / jobs (compute-protocol launches)
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
               "supervisor-protocol"
               "supervisor-protocol/search"
               "compute-protocol"
               (:feature :darwin "compute-backend-seatbelt")
               (:feature :linux "compute-backend-landlock")
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
               (:file "shell-agent-run-test")
               (:file "unit-support")
               (:file "unit-construct-test")
               (:file "unit-time-test")
               (:file "unit-list-test")
               (:file "unit-search-test")
               (:file "unit-read-test")
               (:file "unit-tail-test")
               (:file "unit-control-test")
               (:file "unit-shell-test")
               (:file "unit-agent-run-test")
               (:file "confine-support")          ; R4d-4a: recording backend, helpers
               (:file "confine-construct-test")
               (:file "confine-test")             ; per executor, model / recording backends
               (:file "confine-unit-test")        ; under a supervisor (compute executor)
               (:file "confine-agent-run-test")
               (:file "confine-real-support" :if-feature (:and :sbcl (:or :darwin :linux)))
               (:file "confine-seatbelt-test" :if-feature (:and :sbcl :darwin))
               (:file "confine-landlock-test" :if-feature (:and :sbcl :linux)))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
