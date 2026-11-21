(in-package #:agent-tools)

;;; UNIT-TOOLS as an ai-agent-protocol tool source: descriptors, JSON Schemas
;;; (given, locked by unit-construct-test), approval, invocation. Permission
;;; resources are the implementation's (%UNIT-PERMISSION-RESOURCES).

(defparameter +unit-arg-description+
  "A unit id from unit_list (e.g. \"shell-job-3\"), or a previous session's log name.")

(defun %unit-tool-description (name)
  (cond
    ((string= name "unit_list")
     "List the processes and threads the harness supervises (shell jobs, servers, subagents …), oldest first, as TAB-separated rows: id, kind, state, owner, started, log lines, labels, command. Filter by kind, state (\"live\", \"finished\" or a state), owner (glob over the owner path) and labels.")
    ((string= name "unit_search")
     "Search the logs of supervised units. Returns ranked excerpts: each starts with a ref \"<unit>:<segment>:<line>\" (pass it to unit_read), its stream, time and kinds, then the lines around the hit (\">\" marks the hit). kinds finds errors, warnings, stack traces, test failures and exits even with an empty query. Narrow with units, streams, since / until; the last line says how many results there are.")
    ((string= name "unit_read")
     "Read lines of a unit's log: give a ref from unit_search (shows the lines around it) or a unit with start_line / limit. Lines are \"<line number> TAB <text>\"; dropped ranges show as [gap: …]; a last line tells how to continue.")
    ((string= name "unit_tail")
     "The last lines of a unit's log, or (with after_line) the lines after that one, waiting up to wait_ms for new output. The last line gives the unit's status and the after_line to continue with.")
    ((string= name "unit_control")
     "Stop, restart, or send input to a supervised unit. stop ends it and its child processes; restart relaunches it (same id; its log continues); input writes text to its stdin (\"\\n\" is Enter; eof true closes stdin after). Read what it does next with unit_tail.")))

(defun %enum-array (description values)
  (list "array" description "items" (%obj "type" "string" "enum" (coerce values 'vector))))

(defun %unit-tool-schema (tools name)
  (let ((streams (%enum-array "Streams to include: \"stdout\", \"stderr\" (default both)."
                              '("stdout" "stderr"))))
    (cond
      ((string= name "unit_list")
       (%object-schema
        `(("kind" "string" "Only units of this kind, e.g. \"shell-job\", \"rpc\", \"subagent\".")
          ("state" "string" "Only units in this state: \"live\" (starting or running), \"finished\" (exited, failed or killed), or one of starting, running, exited, failed, killed."
                   "enum" ,(coerce '("live" "finished" "starting" "running" "exited" "failed" "killed") 'vector))
          ("owner" "string" "Glob (* any run) over the owner path, e.g. \"session/*\".")
          ("label" "object" "Only units whose labels have all these key / value pairs."
                   "additionalProperties" ,(%obj "type" "string"))
          ("include_previous" "boolean" "Also list logs left by previous sessions (default false)."))))
      ((string= name "unit_search")
       (%object-schema
        `(("query" "string" "Words to search for (code identifiers, paths, messages); \"double quotes\" require an exact phrase. May be empty when kinds is given.")
          ("units" "array" "Unit ids or globs (* any run, ? one character) to search (default: every unit)."
                   "items" ,(%obj "type" "string"))
          ("kinds" ,@(%enum-array "Only hits of these kinds." +unit-search-kinds+))
          ("streams" ,@streams)
          ("since" "string" "Only lines written at or after this time: an RFC 3339 time (2026-11-21T12:00:00Z) or a duration ago (\"90s\", \"10m\", \"2h\", \"1d\").")
          ("until" "string" "Only lines written at or before this time; same forms as since.")
          ("k" "integer" ,(format nil "Number of results (default ~d)." (unit-tools-default-k tools))
               "minimum" 1 "maximum" ,(unit-tools-max-k tools))
          ("context" "integer" ,(format nil "Lines shown before and after each hit (default ~d)." (unit-tools-context tools))
                     "minimum" 0 "maximum" ,(unit-tools-max-context tools)))
        '("query")))
      ((string= name "unit_read")
       (%object-schema
        `(("ref" "string" "A ref \"<unit>:<segment>:<line>\" from unit_search.")
          ("unit" "string" ,+unit-arg-description+)
          ("start_line" "integer" "First line to show (default: 10 before the ref, or the first line)." "minimum" 1)
          ("limit" "integer" "Lines to show (default 40 with a ref, 200 otherwise)."
                   "minimum" 1 "maximum" ,(unit-tools-max-read-lines tools))
          ("streams" ,@streams))))
      ((string= name "unit_tail")
       (%object-schema
        `(("unit" "string" ,+unit-arg-description+)
          ("lines" "integer" "Lines to return at most (default 50)." "minimum" 1 "maximum" ,(unit-tools-max-read-lines tools))
          ("after_line" "integer" "Return the lines after this line number, waiting up to wait_ms for them (from the last line of an earlier result)." "minimum" 0)
          ("wait_ms" "integer" "With after_line: milliseconds to wait for new output (default 0)."
                     "minimum" 0 "maximum" ,(unit-tools-max-wait tools))
          ("streams" ,@streams))
        '("unit")))
      ((string= name "unit_control")
       (%object-schema
        `(("unit" "string" "The unit id from unit_list.")
          ("action" "string" "stop, restart or input." "enum" ,(coerce +unit-control-actions+ 'vector))
          ("input" "string" "For input: text written verbatim to the unit's stdin (UTF-8).")
          ("eof" "boolean" "For input: close stdin after writing (default false)."))
        '("unit" "action"))))))

(defmethod agent:list-agent-tools ((source unit-tools) &key context)
  "→ fresh LLM-PROTOCOL:LLM-TOOL descriptors for the enabled names, in
+UNIT-TOOL-NAMES+ order."
  (declare (ignore context))
  (loop for name in (unit-tools-names source)
        collect (llm:make-llm-tool :name name
                                   :description (%unit-tool-description name)
                                   :parameters (%unit-tool-schema source name))))

(defmethod agent:tool-executable-p ((source unit-tools) name &key context)
  (declare (ignore context))
  (%unit-enabled-p source name))

(defmethod agent:tool-needs-approval-p ((source unit-tools) name arguments &key context)
  "T for unit_control when UNIT-TOOLS-NEEDS-APPROVAL; reading never asks."
  (declare (ignore arguments context))
  (and (unit-tools-needs-approval source) (equal name "unit_control") t))

(defmethod agent:invoke-tool-async ((source unit-tools) name arguments
                                    &key context callback error-callback)
  "CALL-UNIT-TOOL off the event loop (the function-tool worker path)."
  (if (%unit-enabled-p source name)
      (agent:invoke-tool-async
       (agent:make-function-tool :name name
                                 :handler (lambda (args) (call-unit-tool source name args)))
       name arguments
       :context context :callback callback :error-callback error-callback)
      (call-next-method)))

(defmethod agent:tool-permission-resources ((source unit-tools) name arguments &key context)
  "unit_control → (\"<action>:<unit>\"); unit_read / unit_tail → (\"unit:<unit>\")
(a ref's unit for unit_read); unit_search → (\"unit:<pattern>\") per units entry,
(\"unit:*\") without one; unit_list → (\"*\"). Never signals: arguments that do
not parse, or a missing / non-string key → (\"*\")."
  (declare (ignore context))
  (%unit-permission-resources source name arguments))

(defun %unit-resources-of (name args)
  "The resources of tool NAME on decoded ARGS, or NIL when they do not parse."
  (flet ((str (key)
           (let ((v (gethash key args)))
             (and (stringp v) (plusp (length v)) v))))
    (cond
      ((equal name "unit_control")
       (let ((unit (str "unit")) (action (str "action")))
         (and unit (member action +unit-control-actions+ :test #'string=)
              (list (format nil "~a:~a" action unit)))))
      ((equal name "unit_tail")
       (let ((unit (str "unit")))
         (and unit (list (format nil "unit:~a" unit)))))
      ((equal name "unit_read")
       (let ((ref (str "ref")) (unit (str "unit")))
         (cond ((and ref (not unit))
                (let ((key (ss:parse-log-ref ref)))
                  (and key (list (format nil "unit:~a" key)))))
               ((and unit (not (nth-value 1 (gethash "ref" args))))
                (list (format nil "unit:~a" unit))))))
      ((equal name "unit_search")
       (multiple-value-bind (units present) (gethash "units" args)
         (cond ((or (not present) (eq units :null)
                    (and (vectorp units) (not (stringp units)) (zerop (length units))))
                (list "unit:*"))
               ((and (vectorp units) (not (stringp units))
                     (every (lambda (p) (and (stringp p) (plusp (length p)))) units))
                (map 'list (lambda (p) (format nil "unit:~a" p)) units))))))))

(defun %unit-permission-resources (source name arguments)
  "Never signals: arguments that do not decode (the only TOOL-INPUT-ERROR
%DECODE-ARGUMENTS signals) or do not name what the tool needs → (\"*\")."
  (declare (ignore source))
  (or (let ((args (handler-case (%decode-arguments arguments)
                    (agent:tool-input-error () nil))))
        (and args (%unit-resources-of name args)))
      (list "*")))
