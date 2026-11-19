(defpackage #:agent-tools/tests
  (:use #:cl #:rove #:agent-tools)
  (:local-nicknames (#:a #:ai-agent-protocol)
                    (#:llm #:llm-protocol)
                    (#:p #:cl-stack-pathlib)
                    (#:d #:diff-protocol)
                    (#:json #:json-protocol)))

(in-package #:agent-tools/tests)
