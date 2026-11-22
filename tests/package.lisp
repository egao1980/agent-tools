(defpackage #:agent-tools/tests
  (:use #:cl #:rove #:agent-tools)
  (:local-nicknames (#:a #:ai-agent-protocol)
                    (#:llm #:llm-protocol)
                    (#:p #:cl-stack-pathlib)
                    (#:d #:diff-protocol)
                    (#:json #:json-protocol)
                    (#:sup #:supervisor-protocol)
                    (#:ss #:supervisor-protocol/search)
                    (#:sc #:compute-protocol)))

(in-package #:agent-tools/tests)
