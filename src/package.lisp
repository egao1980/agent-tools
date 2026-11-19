(defpackage #:agent-tools
  (:use #:cl)
  (:nicknames #:stack-agent-tools)
  (:local-nicknames (#:agent #:ai-agent-protocol)
                    (#:llm #:llm-protocol)
                    (#:fs #:cl-stack-pathlib)
                    (#:diff #:diff-protocol)
                    (#:json #:json-protocol))
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
   #:stale-file-error-actual))

(in-package #:agent-tools)
