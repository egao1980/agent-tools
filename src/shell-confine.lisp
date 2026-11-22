(in-package #:agent-tools)

;;; R4d-4a contract: confined shell tools (brief: cl-stack
;;; docs/capabilities/confinement.md § R4d-4). MAKE-SHELL-TOOLS :CONFINEMENT C
;;; :COMPUTE-BACKEND B runs every bash command line under C:
;;;
;;; - Without a supervisor: STACK-COMPUTE:CONFINE-COMMAND B C ARGV (ARGV = the
;;;   shell prefix + the command line exactly as the unconfined path builds it)
;;;   with :DIRECTORY the resolved workdir and :ENV the shell overrides
;;;   (%SHELL-OVERRIDES); then the job's executor runs the launch:
;;;   :PTY → PTY:SPAWN-PTY (CONFINED-LAUNCH-ARGV) :ENVIRONMENT (CONFINED-LAUNCH-ENV)
;;;   :INHERIT-ENVIRONMENT NIL :DIRECTORY (CONFINED-LAUNCH-DIRECTORY);
;;;   :PROCESS → PROC:LAUNCH with :ENV (CONFINED-LAUNCH-ENV) :ENV-MODE :REPLACE.
;;;   The job keeps the launch (SHELL-JOB-LAUNCH). The launch is released
;;;   (STACK-COMPUTE:RELEASE-CONFINED-LAUNCH) exactly once, after the job's
;;;   handle is closed (its tree killed): normal exit, timeout (I-kill),
;;;   job_kill, CLOSE-SHELL-TOOLS, a pump error; a spawn failure after
;;;   CONFINE-COMMAND releases it before TOOL-UNAVAILABLE is signalled; so does
;;;   the closed-while-starting path. A caller that has seen the job end (bash
;;;   returned, job_kill returned, job_output showed the final status) finds it
;;;   released within a few seconds at most.
;;; - With a supervisor: SUPERVISOR-PROTOCOL:START-UNIT … :CONFINEMENT C
;;;   :EXECUTOR (the source's compute executor over B), everything else as
;;;   %START-SHELL-UNIT; the compute executor confines, launches and releases.
;;; - A refusal (any STACK-COMPUTE:COMPUTE-ERROR from CHECK-CONFINEMENT /
;;;   CONFINE-COMMAND, directly or as a SUPERVISOR-PROTOCOL:UNIT-START-ERROR's
;;;   cause) → SHELL-CONFINEMENT-REFUSED, signalled from a HANDLER-BIND (the
;;;   compute restarts stay live), before anything is spawned or registered: no
;;;   process, no unit, no job id.
;;; - job_list rows of a confined source: id TAB status TAB "confined" TAB command.
;;; - The bash description ends with CONFINEMENT-DESCRIPTION; a failed command's
;;;   result gets CONFINEMENT-DENIAL-NOTE's line right before its status line.
;;;
;;; Everything below is a stub until the R4d-4a run implements it. Unconfined
;;; sources never reach a stub.

(defun %r4d4-pending (what)
  (error "agent-tools R4d-4a: ~a is not implemented (contract stub)" what))

(defun confinement-description (confinement)
  "→ one paragraph for the bash tool description saying what CONFINEMENT allows,
starting \"Sandbox:\". It names every WRITABLE directory (native namestrings) and
says writes elsewhere fail (the private TMPDIR is writable when TMP is :PRIVATE,
and the text says \"TMPDIR\"); the network: \"network access is off\" (:NONE),
\"network access is allowed\" (:ALLOW), \"network access is limited\" (a policy);
reads: with READ :ALL and a DENY-READ, that credential locations are unreadable
\(the text says \"unreadable\" and names the entries' last components, e.g. .ssh);
with READ :SYSTEM, \"reads are limited to\" the writable, readable and system paths."
  (declare (ignore confinement))
  (%r4d4-pending "confinement-description"))

(defun confinement-denial-note (confinement output exit-code)
  "→ a one-line note, or NIL, for a confined command that ended with EXIT-CODE
and printed OUTPUT (the cleaned text of the result). Heuristic and cheap; NIL
unless EXIT-CODE is a non-zero integer. The note is
  \"[sandbox: <reason>[; <reason>…]. Do not retry the same command; it will be blocked again.]\"
with reasons, in this order, each at most once:
  :WRITE     \"writes are allowed only under <writable dirs, space-separated> and TMPDIR\"
  :NETWORK   \"network access is off\" (only when NETWORK is not :ALLOW)
  :DENY-READ \"credential locations such as ~/.ssh are unreadable\"
Classification, on OUTPUT downcased, line by line: a denial line contains
\"operation not permitted\", \"permission denied\", \"eperm\", \"eacces\",
\"read-only file system\" or \"sandbox\". A denial line naming a DENY-READ entry
\(its native namestring without a final /) → :DENY-READ; else one mentioning
\"connect\", \"socket\", \"network\" or \"resolve\" → :NETWORK; else → :WRITE.
Any line with \"could not resolve host\", \"network is unreachable\",
\"temporary failure in name resolution\", \"name or service not known\",
\"nodename nor servname\", \"couldn't connect\" or \"failed to connect\" → :NETWORK.
:NETWORK is dropped when NETWORK is :ALLOW. No reason → NIL."
  (declare (ignore confinement output exit-code))
  (%r4d4-pending "confinement-denial-note"))

(defun %spawn-confined-job (tools command directory &key foreground)
  "%SPAWN-JOB for a confined TOOLS: confine, spawn the launch, keep it on the job."
  (declare (ignore tools command directory foreground))
  (%r4d4-pending "confined bash (%spawn-confined-job)"))

(defun %start-confined-shell-unit (tools command directory)
  "%START-SHELL-UNIT for a confined TOOLS: START-UNIT :CONFINEMENT :EXECUTOR."
  (declare (ignore tools command directory))
  (%r4d4-pending "confined bash under a supervisor (%start-confined-shell-unit)"))

(defun %confined-job-list (tools)
  "job_list for a confined TOOLS: rows id TAB status TAB confined TAB command."
  (declare (ignore tools))
  (%r4d4-pending "confined job_list"))

(defmethod shell-job-launch ((job %unit-job))
  "A supervised job's launch: SUPERVISOR-PROTOCOL:UNIT-CONFINED-LAUNCH of its unit."
  (%r4d4-pending "shell-job-launch of a unit job"))
