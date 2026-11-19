(in-package #:agent-tools)

;;; SHELL-TOOLS as an ai-agent-protocol tool source. An agent lists the
;;; SHELL-TOOLS object itself in :TOOLS (next to a FILE-TOOLS over the same
;;; root); FIND-AGENT-TOOL then returns it as the invocation source.

(defmethod agent:list-agent-tools ((source shell-tools) &key context)
  "→ fresh LLM-PROTOCOL:LLM-TOOL descriptors for the enabled names, in
+SHELL-TOOL-NAMES+ order, with draft-07 JSON Schema parameters as
string-keyed hash tables."
  (declare (ignore context))
  (%not-implemented 'list-agent-tools))

(defmethod agent:tool-executable-p ((source shell-tools) name &key context)
  (declare (ignore name context))
  (%not-implemented 'tool-executable-p))

(defmethod agent:tool-needs-approval-p ((source shell-tools) name arguments
                                        &key context)
  "T for bash and job_input when SHELL-TOOLS-NEEDS-APPROVAL; else NIL."
  (declare (ignore name arguments context))
  (%not-implemented 'tool-needs-approval-p))

(defmethod agent:invoke-tool-async ((source shell-tools) name arguments
                                    &key context callback error-callback)
  "CALL-SHELL-TOOL off the event loop; the result string goes to CALLBACK, a
signalled condition to ERROR-CALLBACK."
  (declare (ignore name arguments context callback error-callback))
  (%not-implemented 'invoke-tool-async))

(defmethod agent:tool-permission-resources ((source shell-tools) name arguments
                                            &key context)
  "bash → SHELL-COMMAND-RESOURCES of the command in its workdir; job tools →
(\"job:<id>\"). Never signals; arguments that do not parse → (\"*\")."
  (declare (ignore name arguments context))
  (%not-implemented 'tool-permission-resources))

(defmethod agent:tool-instruction-paths ((source shell-tools) name arguments
                                         &key context)
  "bash → (\"<workdir>\") (\".\" when absent); NIL for job tools and for a
workdir that does not resolve under the root."
  (declare (ignore name arguments context))
  (%not-implemented 'tool-instruction-paths))
