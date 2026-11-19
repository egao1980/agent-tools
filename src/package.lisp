(defpackage #:agent-tools
  (:use #:cl)
  (:nicknames #:stack-agent-tools)
  (:local-nicknames (#:agent #:ai-agent-protocol)
                    (#:llm #:llm-protocol)
                    (#:fs #:cl-stack-pathlib)
                    (#:diff #:diff-protocol)
                    (#:json #:json-protocol)
                    (#:pty #:pty-protocol)
                    (#:proc #:process-protocol))
  (:export
   ;; tool source
   #:file-tools
   #:file-tools-p
   #:make-file-tools
   #:file-tools-root
   #:file-tools-filesystem
   #:file-tools-ignore
   #:file-tools-names
   #:file-tools-max-read-lines
   #:file-tools-max-output
   #:file-tools-max-results
   #:+file-tool-names+
   #:call-file-tool
   #:resolve-tool-path
   ;; conditions (all subtypes of ai-agent-protocol:tool-input-error)
   #:path-outside-root
   #:path-outside-root-path
   #:edit-match-error
   #:edit-match-error-path
   #:edit-match-error-count
   #:edit-match-error-index
   #:stale-file-error
   #:stale-file-error-path
   #:stale-file-error-expected
   #:stale-file-error-actual
   ;; shell tools (H2c)
   #:shell-tools
   #:shell-tools-p
   #:make-shell-tools
   #:shell-tools-root
   #:shell-tools-executor
   #:shell-tools-shell
   #:shell-tools-dialect
   #:shell-tools-env
   #:shell-tools-needs-approval
   #:shell-tools-default-timeout
   #:shell-tools-max-timeout
   #:shell-tools-max-output
   #:shell-tools-max-jobs
   #:shell-tools-max-job-buffer
   #:shell-tools-names
   #:+shell-tool-names+
   #:+shell-environment+
   #:default-shell
   #:shell-dialect
   #:call-shell-tool
   #:close-shell-tools
   #:shell-tools-closed-p
   ;; background jobs
   #:shell-job
   #:shell-job-id
   #:shell-job-command
   #:shell-job-status
   #:shell-job-exit-code
   #:shell-job-signal
   #:shell-tools-jobs
   #:find-shell-job
   ;; scanner and output (pure)
   #:shell-command-resources
   #:clean-shell-output
   #:truncate-shell-output
   ;; conditions and restarts
   #:shell-timeout
   #:shell-timeout-command
   #:shell-timeout-timeout
   #:shell-timeout-output
   #:run-in-background
   #:job-not-found
   #:job-not-found-id
   #:job-not-running
   #:job-not-running-id
   #:job-not-running-status))

(in-package #:agent-tools)
