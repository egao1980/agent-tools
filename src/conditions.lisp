(in-package #:agent-tools)

;;; Typed tool failures. All are AI-AGENT-PROTOCOL:TOOL-INPUT-ERROR: the
;;; model can fix the call and retry. The run turns them into tool results
;;; through AI-AGENT-PROTOCOL:TOOL-FAILURE-TEXT; the report is the MESSAGE
;;; slot, which says what went wrong and what to do next (brief: "Failure
;;; texts").

(define-condition path-outside-root (agent:tool-input-error)
  ((path :initarg :path :reader path-outside-root-path :initform nil
         :documentation "The path argument as the model gave it."))
  (:documentation "A path argument resolves outside the tools' root, lexically
(\"..\", an absolute path elsewhere) or through a symlink."))

(define-condition edit-match-error (agent:tool-input-error)
  ((path :initarg :path :reader edit-match-error-path :initform nil
         :documentation "Root-relative POSIX path of the file.")
   (count :initarg :count :reader edit-match-error-count :initform 0
          :documentation "How many times old_string occurs: 0, or more than
one without replace_all.")
   (index :initarg :index :reader edit-match-error-index :initform nil
          :documentation "1-based index of the failing edit in multi_edit;
NIL for edit."))
  (:documentation "old_string does not occur exactly once (edit, multi_edit)."))

(define-condition stale-file-error (agent:tool-input-error)
  ((path :initarg :path :reader stale-file-error-path :initform nil
         :documentation "Root-relative POSIX path of the file.")
   (expected :initarg :expected :reader stale-file-error-expected :initform nil
             :documentation "The digest the call expected.")
   (actual :initarg :actual :reader stale-file-error-actual :initform nil
           :documentation "The digest of the current content, or NIL when the
file does not exist."))
  (:documentation "The file changed since the model read it: expected_hash
(or, for edit and multi_edit, the hash of the content the tool read) does not
match the current content. For write it is signalled inside the
CL-STACK-PATHLIB:CONTENT-HASH-MISMATCH handler, so pathlib's OVERWRITE and
USE-CURRENT restarts are available."))

;;; --- shell tools (H2c) -------------------------------------------------------

(define-condition shell-timeout (agent:tool-timeout)
  ((command :initarg :command :reader shell-timeout-command :initform nil
            :documentation "The command line as the model gave it.")
   (timeout :initarg :timeout :reader shell-timeout-timeout :initform nil
            :documentation "The timeout that expired, in milliseconds.")
   (output :initarg :output :reader shell-timeout-output :initform ""
           :documentation "The output so far, cleaned and truncated as a
result would be."))
  (:documentation "A foreground bash command ran past its timeout. Signalled
while the command is still running, with two restarts: CONTINUE waits another
timeout, RUN-IN-BACKGROUND turns the command into a background job and makes
the call return the job-start text. When the condition leaves the call
unhandled, the command's process tree is killed."))

(define-condition job-not-found (agent:tool-input-error)
  ((id :initarg :id :reader job-not-found-id :initform nil
       :documentation "The job_id argument as given."))
  (:documentation "job_output / job_input / job_kill named no job of this
SHELL-TOOLS source."))

(define-condition job-not-running (agent:tool-input-error)
  ((id :initarg :id :reader job-not-running-id :initform nil)
   (status :initarg :status :reader job-not-running-status :initform nil
           :documentation ":EXITED or :KILLED."))
  (:documentation "job_input on a job that has already finished."))

;;; --- unit tools (H8c) --------------------------------------------------------

(define-condition unit-not-found (agent:tool-input-error)
  ((unit :initarg :unit :reader unit-not-found-unit :initform nil
         :documentation "The unit / ref argument as given."))
  (:documentation "A unit tool named a unit (or a ref's unit) that this
UNIT-TOOLS source cannot see: unknown, evicted with no log left, or owned
outside its scope. The message names the argument and unit_list."))
