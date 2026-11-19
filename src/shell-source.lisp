(in-package #:agent-tools)

;;; SHELL-TOOLS as an ai-agent-protocol tool source. An agent lists the
;;; SHELL-TOOLS object itself in :TOOLS (next to a FILE-TOOLS over the same
;;; root); FIND-AGENT-TOOL then returns it as the invocation source.

;;; --- descriptors -----------------------------------------------------------

(defparameter +job-id-description+
  "The job id from the bash run_in_background result, e.g. \"job-1\".")

(defun %shell-tool-description (source name)
  (cond
    ((string= name "bash")
     (format nil "Run a command line with ~{~a~^ ~} in a fresh shell (no state carries over between calls; pass workdir instead of cd). Output: stdout and stderr merged, cleaned, head + tail truncated, then \"[exit code N]\"; a non-zero exit is a normal result. Foreground commands get end of file on stdin and time out after timeout_ms. For servers, watchers and other long commands set run_in_background: true, then read them with job_output and stop them with job_kill."
             (shell-tools-shell source)))
    ((string= name "job_output")
     "Read the output a background job printed since the last read, then its status line ([job <id> running] or how it ended). wait_ms waits for the job to finish, at most that long.")
    ((string= name "job_input")
     "Write input to a running background job's stdin (\"\\n\" is Enter), then return what job_output would. On a terminal the input is echoed.")
    ((string= name "job_kill")
     "Stop a background job and its child processes; returns its unread output and final status. A finished job is not an error.")
    ((string= name "job_list")
     "List the background jobs of this session, oldest first: id, status (running, exited <code>, signal <name>, killed) and command, TAB-separated.")))

(defun %shell-tool-schema (source name)
  (let ((max (shell-tools-max-timeout source)))
    (cond
      ((string= name "bash")
       (%object-schema
        `(("command" "string" "The command line to run.")
          ("timeout_ms" "integer"
                        ,(format nil "Foreground timeout in milliseconds (default ~d). Ignored with run_in_background."
                                 (shell-tools-default-timeout source))
                        "minimum" 1 "maximum" ,max)
          ("workdir" "string" "Directory to run in, relative to the project root (default: the root).")
          ("run_in_background" "boolean" "Start the command as a background job and return its id at once (default false).")
          ("description" "string" "Short description of what the command does, for the user."))
        '("command")))
      ((string= name "job_output")
       (%object-schema
        `(("job_id" "string" ,+job-id-description+)
          ("wait_ms" "integer" "Milliseconds to wait for the job to finish before returning (default 0)."
                     "minimum" 0 "maximum" ,max))
        '("job_id")))
      ((string= name "job_input")
       (%object-schema
        `(("job_id" "string" ,+job-id-description+)
          ("input" "string" "Text written verbatim to the job's stdin (UTF-8).")
          ("wait_ms" "integer" "Milliseconds to wait for the job to finish before returning (default 0)."
                     "minimum" 0 "maximum" ,max))
        '("job_id" "input")))
      ((string= name "job_kill")
       (%object-schema `(("job_id" "string" ,+job-id-description+)) '("job_id")))
      ((string= name "job_list")
       (%object-schema '())))))

;;; --- protocol methods ------------------------------------------------------

(defmethod agent:list-agent-tools ((source shell-tools) &key context)
  "→ fresh LLM-PROTOCOL:LLM-TOOL descriptors for the enabled names, in
+SHELL-TOOL-NAMES+ order, with draft-07 JSON Schema parameters as
string-keyed hash tables."
  (declare (ignore context))
  (loop for name in (shell-tools-names source)
        collect (llm:make-llm-tool :name name
                                   :description (%shell-tool-description source name)
                                   :parameters (%shell-tool-schema source name))))

(defmethod agent:tool-executable-p ((source shell-tools) name &key context)
  (declare (ignore context))
  (%shell-enabled-p source name))

(defmethod agent:tool-needs-approval-p ((source shell-tools) name arguments
                                        &key context)
  "T for bash and job_input when SHELL-TOOLS-NEEDS-APPROVAL; else NIL."
  (declare (ignore arguments context))
  (and (shell-tools-needs-approval source)
       (member name '("bash" "job_input") :test #'equal)
       t))

(defmethod agent:invoke-tool-async ((source shell-tools) name arguments
                                    &key context callback error-callback)
  "CALL-SHELL-TOOL off the event loop; the result string goes to CALLBACK, a
signalled condition to ERROR-CALLBACK."
  (if (%shell-enabled-p source name)
      ;; The protocol's sync function-tool path: a worker thread, then the
      ;; value or the ERROR delivered on the loop.
      (agent:invoke-tool-async
       (agent:make-function-tool
        :name name
        :handler (lambda (args) (call-shell-tool source name args)))
       name arguments
       :context context :callback callback :error-callback error-callback)
      ;; Default (source t) method: AGENT-UNKNOWN-TOOL on the loop.
      (call-next-method)))

(defun %shell-arguments (arguments)
  "Decoded ARGUMENTS, or NIL when they do not parse."
  (handler-case (%decode-arguments arguments)
    (agent:tool-input-error () nil)))

(defun %bash-workdir (source args)
  "The root-relative workdir of bash ARGS (\".\" when absent), or NIL when
it does not resolve under the root."
  (multiple-value-bind (v present) (gethash "workdir" args)
    (if (or (not present) (eq v :null))
        "."
        (%relative-or-nil (%shell-files source) v))))

(defmethod agent:tool-permission-resources ((source shell-tools) name arguments
                                            &key context)
  "bash → SHELL-COMMAND-RESOURCES of the command in its workdir; job tools →
(\"job:<id>\"). Never signals; arguments that do not parse → (\"*\")."
  (declare (ignore context))
  (let ((args (%shell-arguments arguments)))
    (or (and args
             (cond
               ((equal name "bash")
                (let ((command (gethash "command" args))
                      (workdir (%bash-workdir source args)))
                  (and (stringp command) (not (%blank-p command)) workdir
                       (shell-command-resources
                        command
                        :dialect (shell-tools-dialect source)
                        :workdir workdir
                        :root (string-right-trim "/" (fs:as-posix (shell-tools-root source)))))))
               ((member name '("job_output" "job_input" "job_kill") :test #'equal)
                (let ((id (gethash "job_id" args)))
                  (and (stringp id) (plusp (length id))
                       (list (concatenate 'string "job:" id)))))))
        (list "*"))))

(defmethod agent:tool-instruction-paths ((source shell-tools) name arguments
                                         &key context)
  "bash → (\"<workdir>\") (\".\" when absent); NIL for job tools and for a
workdir that does not resolve under the root."
  (declare (ignore context))
  (when (equal name "bash")
    (let* ((args (%shell-arguments arguments))
           (workdir (and args (%bash-workdir source args))))
      (and workdir (list workdir)))))
