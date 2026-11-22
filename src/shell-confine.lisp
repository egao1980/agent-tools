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
;;; Unconfined sources never reach this file's code: %SPAWN-JOB,
;;; %START-SHELL-UNIT and %TOOL-JOB-LIST route here only when
;;; SHELL-TOOLS-CONFINEMENT is set.

;;; --- the bash description and the denial note (pure) ---------------------------

(defun %native-namestrings (pathnames)
  (mapcar #'uiop:native-namestring pathnames))

(defun %last-component (pathname)
  "The last component of PATHNAME's native namestring (a final / ignored): .ssh"
  (let* ((ns (string-right-trim "/\\" (uiop:native-namestring pathname)))
         (cut (position-if (lambda (ch) (find ch "/\\")) ns :from-end t)))
    (if cut (subseq ns (1+ cut)) ns)))

(defun %network-text (confinement)
  (let ((network (sc:confinement-network confinement)))
    (case network
      (:none "network access is off")
      (:allow "network access is allowed")
      (t "network access is limited to the hosts and ports the policy allows"))))

(defun confinement-description (confinement)
  "→ one paragraph for the bash tool description saying what CONFINEMENT allows,
starting \"Sandbox:\". It names every WRITABLE directory (native namestrings) and
says writes elsewhere fail (the private TMPDIR is writable when TMP is :PRIVATE,
and the text says \"TMPDIR\"); the network: \"network access is off\" (:NONE),
\"network access is allowed\" (:ALLOW), \"network access is limited\" (a policy);
reads: with READ :ALL and a DENY-READ, that credential locations are unreadable
\(the text says \"unreadable\" and names the entries' last components, e.g. .ssh);
with READ :SYSTEM, \"reads are limited to\" the writable, readable and system paths."
  (let* ((c confinement)
         (writable (%native-namestrings (sc:confinement-writable c)))
         (deny (sc:confinement-deny-read c)))
    (with-output-to-string (s)
      (format s "Sandbox: every command runs confined. It can write only ~:[nowhere on disk~;under ~:*~{~a~^, ~}~]"
              writable)
      (ecase (sc:confinement-tmp c)
        (:private (write-string " and its private TMPDIR (a fresh temporary directory per command, removed when it ends)" s))
        (:shared (write-string " and the system temporary directory (TMPDIR)" s))
        (:none (write-string "; there is no writable TMPDIR" s)))
      (write-string "; writes anywhere else fail." s)
      (format s " Inside the sandbox, ~a." (%network-text c))
      (when (eq (sc:confinement-read c) :system)
        (format s " Outside the writable directories, reads are limited to ~@[~{~a~^, ~} and ~]the system paths."
                (%native-namestrings (sc:confinement-readable c))))
      (when deny
        (format s " Credential locations are unreadable: ~{~a~^, ~}."
                (remove-duplicates (mapcar #'%last-component deny) :test #'string= :from-end t)))
      (write-string " A command the sandbox blocks fails (EPERM, \"Operation not permitted\"); do not retry it, work inside the writable directories instead." s))))

(defparameter +denial-words+
  '("operation not permitted" "permission denied" "eperm" "eacces"
    "read-only file system" "sandbox")
  "A line of output naming one of these (downcased) reports a denial.")

(defparameter +network-words+ '("connect" "socket" "network" "resolve")
  "A denial line naming one of these is a network denial.")

(defparameter +network-failures+
  '("could not resolve host" "network is unreachable" "temporary failure in name resolution"
    "name or service not known" "nodename nor servname" "couldn't connect" "failed to connect")
  "A line naming one of these is a network failure, denial word or not.")

(defun %mentions-any (line words)
  (some (lambda (w) (search w line)) words))

(defun %deny-read-keys (confinement)
  "CONFINEMENT's deny-read entries as downcased native namestrings without a final /."
  (remove "" (mapcar (lambda (p)
                       (string-downcase (string-right-trim "/\\" (uiop:native-namestring p))))
                     (sc:confinement-deny-read confinement))
          :test #'string=))

(defun %denial-reason-text (confinement reason)
  (ecase reason
    (:write (format nil "writes are allowed only under ~{~a~^ ~} and TMPDIR"
                    (%native-namestrings (sc:confinement-writable confinement))))
    (:network "network access is off")
    ;; A literal, never a FORMAT control string: ~/ would be a directive.
    (:deny-read "credential locations such as ~/.ssh are unreadable")))

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
  (when (and (integerp exit-code) (/= 0 exit-code) (stringp output) (plusp (length output)))
    (let ((deny (%deny-read-keys confinement))
          (reasons '()))
      (dolist (line (uiop:split-string (string-downcase output)
                                       :separator '(#\Newline #\Return)))
        (when (%mentions-any line +denial-words+)
          (pushnew (cond ((%mentions-any line deny) :deny-read)
                         ((%mentions-any line +network-words+) :network)
                         (t :write))
                   reasons))
        (when (%mentions-any line +network-failures+)
          (pushnew :network reasons)))
      (when (eq :allow (sc:confinement-network confinement))
        (setf reasons (remove :network reasons)))
      (let ((texts (loop for r in '(:write :network :deny-read)
                         when (member r reasons)
                           collect (%denial-reason-text confinement r))))
        (and texts
             (format nil "[sandbox: ~{~a~^; ~}. Do not retry the same command; it will be blocked again.]"
                     texts))))))

;;; --- refusals --------------------------------------------------------------------

(defun %refusal-cause (condition)
  "The compute condition behind CONDITION (UNIT-START-ERROR causes unwrapped), or NIL."
  (loop while (typep condition 'sup:unit-start-error)
        do (setf condition (sup:unit-start-error-cause condition)))
  (and (typep condition 'sc:compute-error) condition))

(defun %call-refusing-unconfined (command thunk)
  "Call THUNK (CONFINE-COMMAND, or START-UNIT :CONFINEMENT). A compute refusal,
directly or as a UNIT-START-ERROR's cause → SHELL-CONFINEMENT-REFUSED, signalled
from inside the handler, so CHECK-CONFINEMENT's USE-VALUE restart is still live.
Other start errors are declined and keep their own path."
  (flet ((refuse (condition)
           (let ((cause (%refusal-cause condition)))
             (when cause
               (error 'shell-confinement-refused
                      :command command :cause cause
                      :message (format nil "the confinement was refused; the command was not run (~a)"
                                       cause))))))
    (handler-bind ((sc:compute-error #'refuse)
                   (sup:unit-start-error #'refuse))
      (funcall thunk))))

;;; --- confined bash without a supervisor -------------------------------------------

(defun %check-shell-program (tools)
  "The shell program named by a path must exist (checked before confining)."
  (let ((program (first (shell-tools-shell tools))))
    (when (and (find-if (lambda (c) (find c "/\\")) program)
               (null (uiop:probe-file* (uiop:parse-native-namestring program))))
      (%cannot-start tools "no such file"))))

(defun %confine-argv (tools command argv directory)
  "→ the CONFINED-LAUNCH for ARGV in DIRECTORY; a refusal signals
SHELL-CONFINEMENT-REFUSED before anything exists."
  (%call-refusing-unconfined
   command
   (lambda ()
     (sc:confine-command (shell-tools-compute-backend tools) (shell-tools-confinement tools)
                         argv :directory directory :env (%shell-overrides tools)))))

(defun %spawn-launch-handle (tools launch)
  "Run LAUNCH on the tools' executor with exactly its argv, environment and
directory. A spawn failure releases LAUNCH, then signals TOOL-UNAVAILABLE."
  (let ((argv (sc:confined-launch-argv launch))
        (env (sc:confined-launch-env launch))
        (directory (uiop:native-namestring (sc:confined-launch-directory launch))))
    (flet ((fail (condition)
             (sc:release-confined-launch launch)
             (%cannot-start tools condition)))
      (handler-case
          (ecase (shell-tools-executor tools)
            (:pty
             (pty:spawn-pty argv :directory directory :environment env
                                 :inherit-environment nil :cols 512 :rows 50))
            (:process
             (proc:launch argv :input nil :output nil :error-output :output
                               :element-type :octets :directory directory
                               :env env :env-mode :replace
                               :process-group (%process-groups-p))))
        (pty:pty-spawn-error (c) (fail c))
        (proc:process-error (c) (fail c))))))

(defun %spawn-confined-job (tools command directory &key foreground)
  "%SPAWN-JOB for a confined TOOLS: confine, spawn the launch, keep it on the job.
The launch is released by %FINISH once the pump runs; before that, by this function."
  (%check-shell-program tools)
  (let* ((executor (shell-tools-executor tools))
         (posix-eof (and foreground (eq executor :pty)
                         (eq (shell-tools-dialect tools) :posix)))
         (argv (append (shell-tools-shell tools)
                       (list (if posix-eof
                                 (concatenate 'string "exec </dev/null; " command)
                                 command))))
         ;; A refusal unwinds from here: no job, no handle, no id.
         (launch (%confine-argv tools command argv directory))
         (pumped nil))
    (unwind-protect
         (let ((job (make-instance '%job :tools tools :executor executor :id nil
                                         :command command :launch launch
                                         :cap (shell-tools-max-job-buffer tools))))
           (setf (%job-handle job) (%spawn-launch-handle tools launch))
           (when (bt2:with-lock-held ((%shell-lock tools))
                   (if (shell-tools-closed-p tools)
                       t
                       (progn (push job (%shell-live tools)) nil)))
             ;; Closed while it started: the tree first, then the launch.
             (%close-handle job)
             (sc:release-confined-launch launch)
             (%closed-error "bash"))
           (when (and foreground (eq executor :process))
             (handler-case (close (proc:stdin (%job-handle job)))
               (stream-error () nil)))
           (bt2:make-thread (lambda () (%run-pump job)) :name "agent-tools shell job")
           (setf pumped t)                ; from here %FINISH releases the launch
           (when (and foreground (eq executor :pty) (not posix-eof))
             (%post job (list :write (make-array 1 :element-type '(unsigned-byte 8)
                                                   :initial-element 4)
                              (list nil))))
           job)
      ;; Any other exit before the pump started (idempotent backstop).
      (unless pumped
        (sc:release-confined-launch launch)))))

;;; --- confined bash under a supervisor ---------------------------------------------

(defun %start-confined-shell-unit (tools command directory)
  "%START-SHELL-UNIT for a confined TOOLS: START-UNIT :CONFINEMENT :EXECUTOR (the
source's compute executor, which confines, launches and releases)."
  (%check-shell-program tools)
  (or (handler-case
          ;; Inside the HANDLER-CASE: the refusal is signalled before it unwinds.
          (%call-refusing-unconfined
           command
           (lambda ()
             (sup:start-unit (shell-tools-supervisor tools) :shell-job
                             :command (append (shell-tools-shell tools) (list command))
                             :owner (shell-tools-scope tools)
                             :labels (list :tool "bash")
                             :cwd directory
                             :env (%shell-overrides tools)
                             :restart :never
                             :confinement (shell-tools-confinement tools)
                             :executor (%shell-compute-executor tools))))
        (sup:scope-closed () (%closed-error "bash"))
        (sup:supervisor-closed () (%closed-error "bash"))
        (sup:unit-start-error (c) (%cannot-start tools c))
        (sup:unit-limit-reached (c)
          (error 'agent:tool-unavailable :message (format nil "bash cannot start: ~a" c))))
      (error 'agent:tool-unavailable
             :message "bash cannot start: the supervisor refused the unit")))

(defmethod shell-job-launch ((job %unit-job))
  "A supervised job's launch: SUPERVISOR-PROTOCOL:UNIT-CONFINED-LAUNCH of its unit."
  (let ((unit (shell-job-unit job)))
    (and unit (sup:unit-confined-launch unit))))

;;; --- job_list --------------------------------------------------------------------

(defun %confined-job-list (tools)
  "job_list for a confined TOOLS: rows id TAB status TAB confined TAB command."
  (let ((jobs (shell-tools-jobs tools)))
    (if (null jobs)
        "[no jobs]"
        (format nil "~{~a~^~%~}"
                (mapcar (lambda (j)
                          (format nil "~a~c~a~cconfined~c~a"
                                  (shell-job-id j) #\Tab (%job-list-status j) #\Tab #\Tab
                                  (shell-job-command j)))
                        jobs)))))
