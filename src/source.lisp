(in-package #:agent-tools)

;;; FILE-TOOLS as an ai-agent-protocol tool source. An agent lists the
;;; FILE-TOOLS object itself in :TOOLS; FIND-AGENT-TOOL then returns it as
;;; the invocation source, so these methods see every call.

(defmethod agent:list-agent-tools ((source file-tools) &key context)
  "→ fresh LLM-PROTOCOL:LLM-TOOL descriptors for the enabled names, in
+FILE-TOOL-NAMES+ order, with draft-07 JSON Schema parameters as
string-keyed hash tables."
  (declare (ignore context))
  (%not-implemented 'list-agent-tools))

(defmethod agent:tool-executable-p ((source file-tools) name &key context)
  (declare (ignore name context))
  (%not-implemented 'tool-executable-p))

(defmethod agent:invoke-tool-async ((source file-tools) name arguments
                                    &key context callback error-callback)
  "CALL-FILE-TOOL off the event loop; the result string goes to CALLBACK, a
signalled condition to ERROR-CALLBACK."
  (declare (ignore name arguments context callback error-callback))
  (%not-implemented 'invoke-tool-async))

(defmethod agent:tool-permission-resources ((source file-tools) name arguments
                                            &key context)
  "→ (\"path:<relative>\") for the file or directory the call touches. Never
signals: an argument that does not resolve gives (\"path:<argument>\")."
  (declare (ignore name arguments context))
  (%not-implemented 'tool-permission-resources))

(defmethod agent:tool-instruction-paths ((source file-tools) name arguments
                                         &key context)
  "→ (\"<relative>\") the touched path, for AGENTS.md discovery; NIL when the
argument does not resolve under the root."
  (declare (ignore name arguments context))
  (%not-implemented 'tool-instruction-paths))
