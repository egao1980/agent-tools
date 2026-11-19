(in-package #:agent-tools)

;;; H2b contract: the class and its readers are fixed; every function and
;;; method body that does work calls %NOT-IMPLEMENTED. The implementation
;;; removes %NOT-IMPLEMENTED and every call to it.

(defun %not-implemented (what)
  (error "agent-tools: ~a is not implemented (H2b contract)" what))

(defparameter +file-tool-names+
  '("read" "write" "edit" "multi_edit" "glob" "grep" "ls")
  "Every tool a FILE-TOOLS source can expose, in LIST-AGENT-TOOLS order.")

(defclass file-tools ()
  ((root :initarg :root :reader file-tools-root
         :documentation "Absolute directory CL-STACK-PATHLIB:PATH every path
argument resolves under (the truename on the local filesystem).")
   (filesystem :initarg :filesystem :reader file-tools-filesystem
               :documentation "The CL-STACK-PATHLIB:FILESYSTEM of ROOT.")
   (ignore :initarg :ignore :reader file-tools-ignore :initform :auto
           :documentation ":AUTO, NIL or a CL-STACK-PATHLIB:IGNORE-MATCHER
(patterns relative to ROOT). Applies to glob, grep and ls only.")
   (names :initarg :names :reader file-tools-names
          :documentation "Enabled tool names, a subset of +FILE-TOOL-NAMES+
in that order.")
   (max-read-lines :initarg :max-read-lines :reader file-tools-max-read-lines
                   :initform 2000)
   (max-output :initarg :max-output :reader file-tools-max-output
               :initform 50000
               :documentation "Cap in characters on one tool result (the
read hash footer is not counted).")
   (max-results :initarg :max-results :reader file-tools-max-results
                :initform 100
                :documentation "Default and cap for glob and grep results.")
   (tools :initarg :tools :initform nil
          :documentation "Internal: the LLM-TOOL descriptors, by name."))
  (:documentation "Tool source over one directory tree of a pathlib filesystem."))

(defun file-tools-p (x)
  (typep x 'file-tools))

(defun make-file-tools (&key root filesystem (ignore :auto)
                          (max-read-lines 2000) (max-output 50000)
                          (max-results 100) (tools +file-tool-names+))
  "→ a FILE-TOOLS source rooted at ROOT (a string, pathname or
CL-STACK-PATHLIB:PATH naming an existing directory). FILESYSTEM defaults to
ROOT's when ROOT is a PATH, else CL-STACK-PATHLIB:*FILESYSTEM*. TOOLS selects
the exposed tool names. A missing ROOT signals CL-STACK-PATHLIB:PATH-NOT-FOUND;
a ROOT that is not a directory signals CL-STACK-PATHLIB:PATH-ERROR; an unknown
tool name signals an ERROR."
  (declare (ignore root filesystem ignore max-read-lines max-output max-results
                   tools))
  (%not-implemented 'make-file-tools))

(defun resolve-tool-path (tools designator &key (allow-root t))
  "Resolve the path argument DESIGNATOR (a string: relative to the root, or
absolute and under it) on TOOLS. → (values path relative) where PATH is an
absolute CL-STACK-PATHLIB:PATH and RELATIVE the root-relative POSIX string
(\".\" for the root itself). Outside the root signals PATH-OUTSIDE-ROOT; an
empty string, or the root when ALLOW-ROOT is NIL, signals
AI-AGENT-PROTOCOL:TOOL-INPUT-ERROR."
  (declare (ignore tools designator allow-root))
  (%not-implemented 'resolve-tool-path))

(defun call-file-tool (tools name arguments)
  "Run tool NAME of TOOLS synchronously on ARGUMENTS (a JSON object string or
a string-keyed hash table, as json-protocol decodes one). → the result
string. Failures signal AI-AGENT-PROTOCOL:TOOL-FAILURE subtypes; a name that
is not enabled signals AI-AGENT-PROTOCOL:AGENT-UNKNOWN-TOOL."
  (declare (ignore tools name arguments))
  (%not-implemented 'call-file-tool))
