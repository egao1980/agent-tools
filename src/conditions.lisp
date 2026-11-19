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
