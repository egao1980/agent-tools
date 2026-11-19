(in-package #:agent-tools)

;;; H2c contract: SHELL-TOOLS runs shell commands for the coding agent:
;;; bash (foreground with a timeout, or a background job), job_output,
;;; job_input, job_kill and job_list. Unix runs commands on a pty
;;; (pty-protocol, pty-backend-native); Windows, and :EXECUTOR :PROCESS
;;; anywhere, through process-protocol pipes. SHELL-COMMAND-RESOURCES turns a
;;; command line into permission resources.
;;;
;;; The classes, readers and constants are fixed; every function and method
;;; body that does work calls %NOT-IMPLEMENTED. The implementation removes
;;; %NOT-IMPLEMENTED and every call to it.

(defun %not-implemented (what)
  (error "agent-tools: ~a is not implemented (H2c contract)" what))

(defparameter +shell-tool-names+
  '("bash" "job_output" "job_input" "job_kill" "job_list")
  "Every tool a SHELL-TOOLS source can expose, in LIST-AGENT-TOOLS order.")

(defparameter +shell-environment+
  '(("TERM" . "dumb") ("PAGER" . "cat") ("GIT_PAGER" . "cat")
    ("NO_COLOR" . "1") ("GIT_TERMINAL_PROMPT" . "0"))
  "Variables every command gets on top of the parent's environment, before
the source's own :ENV. They keep pagers, colours and credential prompts out
of non-interactive runs.")

(defun default-shell ()
  "→ the shell argv prefix used when MAKE-SHELL-TOOLS gets no :SHELL: the
command line is appended as one last argument. (\"/bin/sh\" \"-c\") on Unix,
(\"cmd.exe\" \"/d\" \"/s\" \"/c\") on Windows."
  #+(or windows win32) (list "cmd.exe" "/d" "/s" "/c")
  #-(or windows win32) (list "/bin/sh" "-c"))

(defun shell-dialect (shell)
  "→ the scanner dialect of SHELL (an argv prefix, a list of strings): the
basename of its first element, case-insensitively and without a .exe suffix,
is sh, bash, dash, zsh, ksh, mksh or ash → :POSIX; cmd → :CMD; anything else
→ :OPAQUE."
  (declare (ignore shell))
  (%not-implemented 'shell-dialect))

;;; --- jobs ------------------------------------------------------------------

(defclass shell-job ()
  ((id :initarg :id :reader shell-job-id
       :documentation "\"job-<n>\", n counting from 1 per SHELL-TOOLS source.")
   (command :initarg :command :reader shell-job-command
            :documentation "The command line as the model gave it.")
   (status :initarg :status :initform :running :reader shell-job-status
           :documentation ":RUNNING, :EXITED (it ended on its own) or :KILLED
(job_kill, CLOSE-SHELL-TOOLS).")
   (exit-code :initarg :exit-code :initform nil :reader shell-job-exit-code
              :documentation "Exit code once :EXITED normally, else NIL.")
   (signal :initarg :signal :initform nil :reader shell-job-signal
           :documentation "Terminating signal (a keyword such as :TERM, or a
number) when the job :EXITED by a signal on the pty executor, else NIL."))
  (:documentation "A background command started by bash run_in_background
(or the RUN-IN-BACKGROUND restart). The implementation may subclass it or add
slots."))

;;; --- the source ------------------------------------------------------------

(defclass shell-tools ()
  ((root :initarg :root :reader shell-tools-root
         :documentation "Absolute directory CL-STACK-PATHLIB:PATH on the local
filesystem (its truename). Commands start here unless workdir says otherwise.")
   (executor :initarg :executor :reader shell-tools-executor
             :documentation ":PTY or :PROCESS (never :AUTO).")
   (shell :initarg :shell :reader shell-tools-shell
          :documentation "Argv prefix; the command line is its last argument.")
   (dialect :initarg :dialect :reader shell-tools-dialect
            :documentation ":POSIX, :CMD or :OPAQUE: how SHELL-COMMAND-RESOURCES
reads the command line.")
   (env :initarg :env :initform nil :reader shell-tools-env
        :documentation "Alist (name . value) over the parent's environment and
+SHELL-ENVIRONMENT+; a NIL value unsets.")
   (needs-approval :initarg :needs-approval :initform t
                   :reader shell-tools-needs-approval
                   :documentation "When true, TOOL-NEEDS-APPROVAL-P is T for
bash and job_input, so with no deciding permission rule they ask.")
   (default-timeout :initarg :default-timeout :initform 120000
                    :reader shell-tools-default-timeout
                    :documentation "Milliseconds; bash timeout_ms default.")
   (max-timeout :initarg :max-timeout :initform 600000
                :reader shell-tools-max-timeout
                :documentation "Milliseconds; cap on timeout_ms and wait_ms.")
   (max-output :initarg :max-output :initform 30000
               :reader shell-tools-max-output
               :documentation "Characters of output in one result (the
status line is not counted); see TRUNCATE-SHELL-OUTPUT.")
   (max-jobs :initarg :max-jobs :initform 8 :reader shell-tools-max-jobs
             :documentation "Running background jobs at a time.")
   (max-job-buffer :initarg :max-job-buffer :initform 1000000
                   :reader shell-tools-max-job-buffer
                   :documentation "Characters of unread output a job keeps;
older unread output is dropped.")
   (names :initarg :names :reader shell-tools-names
          :documentation "Enabled tool names, a subset of +SHELL-TOOL-NAMES+
in that order.")
   (closed-p :initform nil :reader shell-tools-closed-p
             :documentation "True after CLOSE-SHELL-TOOLS."))
  (:documentation "Tool source running shell commands in one local directory
tree. The implementation may add slots (jobs, a lock, the id counter)."))

(defun shell-tools-p (x)
  (typep x 'shell-tools))

(defun make-shell-tools (&key root (executor :auto) shell dialect env
                           (needs-approval t) (default-timeout 120000)
                           (max-timeout 600000) (max-output 30000) (max-jobs 8)
                           (max-job-buffer 1000000) (tools +shell-tool-names+))
  "→ a SHELL-TOOLS source rooted at ROOT (a string, pathname or local
CL-STACK-PATHLIB:PATH naming an existing directory).
EXECUTOR: :AUTO (:PTY on Unix, :PROCESS on Windows), :PTY or :PROCESS; :PTY on
Windows signals an ERROR. SHELL defaults to (DEFAULT-SHELL); DIALECT to
(SHELL-DIALECT SHELL). TOOLS selects the exposed tool names.
A missing ROOT signals CL-STACK-PATHLIB:PATH-NOT-FOUND; a ROOT that is not a
directory signals CL-STACK-PATHLIB:PATH-ERROR; a PATH on another filesystem
than the local one, an unknown tool name, an unknown executor or dialect, or
DEFAULT-TIMEOUT above MAX-TIMEOUT signal an ERROR."
  (declare (ignore root executor shell dialect env needs-approval default-timeout
                   max-timeout max-output max-jobs max-job-buffer tools))
  (%not-implemented 'make-shell-tools))

(defun call-shell-tool (tools name arguments)
  "Run tool NAME of TOOLS synchronously on ARGUMENTS (a JSON object string or
a string-keyed hash table). → the result string. Failures signal
AI-AGENT-PROTOCOL:TOOL-FAILURE subtypes; a foreground timeout signals
SHELL-TIMEOUT with its restarts; a name that is not enabled signals
AI-AGENT-PROTOCOL:AGENT-UNKNOWN-TOOL; any call after CLOSE-SHELL-TOOLS
signals AI-AGENT-PROTOCOL:TOOL-UNAVAILABLE."
  (declare (ignore tools name arguments))
  (%not-implemented 'call-shell-tool))

(defun close-shell-tools (tools)
  "Kill every running job of TOOLS (status :KILLED) and refuse later calls.
Idempotent. → NIL."
  (declare (ignore tools))
  (%not-implemented 'close-shell-tools))

(defun shell-tools-jobs (tools)
  "→ the SHELL-JOBs of TOOLS, oldest first, finished ones included."
  (declare (ignore tools))
  (%not-implemented 'shell-tools-jobs))

(defun find-shell-job (tools id)
  "→ the SHELL-JOB of TOOLS named ID, or NIL."
  (declare (ignore tools id))
  (%not-implemented 'find-shell-job))

;;; --- pure helpers ----------------------------------------------------------

(defun shell-command-resources (command &key (dialect :posix) (workdir ".") root)
  "→ the permission resources of COMMAND (a command line string) for
AI-AGENT-PROTOCOL rules, in order of appearance, without duplicates:
\"bash:<words>\" and \"program:<argv0>\" per simple command, \"cd:<path>\",
\"read:<path>\", \"write:<path>\" for the paths it changes to, reads or
writes, and \"*\" (last) when the scan met a construct it does not model.
NIL for a blank or comment-only command. Never signals.
WORKDIR: root-relative POSIX directory the command starts in; when it is not
\".\" the first resource is \"cd:<workdir>\". ROOT: the root's absolute POSIX
string, or NIL; with it, absolute paths under the root and relative paths
that leave it are reported root-relative and absolute respectively."
  (declare (ignore command dialect workdir root))
  (%not-implemented 'shell-command-resources))

(defun clean-shell-output (output)
  "→ OUTPUT (an octet vector, decoded as UTF-8 with U+FFFD for each invalid
byte, or a string) as text for the model: CR LF → LF; ANSI escape sequences
removed; in each line only the text after its last remaining CR kept; other
control characters except TAB and LF removed."
  (declare (ignore output))
  (%not-implemented 'clean-shell-output))

(defun truncate-shell-output (string max)
  "→ STRING when it has at most MAX characters; else its head and tail in
whole lines around a \"[truncated: <n> characters omitted; ...]\" line."
  (declare (ignore string max))
  (%not-implemented 'truncate-shell-output))
