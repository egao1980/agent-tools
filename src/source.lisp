(in-package #:agent-tools)

;;; FILE-TOOLS as an ai-agent-protocol tool source. An agent lists the
;;; FILE-TOOLS object itself in :TOOLS; FIND-AGENT-TOOL then returns it as
;;; the invocation source, so these methods see every call.

;;; --- schemas ---------------------------------------------------------------

(defun %obj (&rest kv)
  "String-keyed EQUAL hash table from alternating keys and values."
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kv by #'cddr do (setf (gethash k h) v))
    h))

(defun %object-schema (properties &optional required)
  "Draft-07 object schema. PROPERTIES: list of (name type description
. extra-plist)."
  (let ((props (make-hash-table :test #'equal)))
    (loop for (name type description . extra) in properties
          do (setf (gethash name props)
                   (apply #'%obj "type" type "description" description extra)))
    (let ((h (%obj "type" "object" "properties" props "additionalProperties" nil)))
      (when required
        (setf (gethash "required" h) (coerce required 'vector)))
      h)))

(defparameter +path-description+
  "Path relative to the project root (POSIX \"/\" separators), or an absolute path under the root.")

(defparameter +expected-hash-description+
  "Optional \"sha256:...\" digest from an earlier read / write / edit result (its hash: line). The call is refused with a stale-file error when the file changed since.")

(defun %tool-description (name)
  (cond
    ((string= name "read")
     "Read a UTF-8 text file. Output: one line per file line as a 6-column line number, TAB, the text; then any truncation notice; then a final line \"hash: sha256:...\" (the whole file's digest, to pass as expected_hash). Use offset / limit for large files.")
    ((string= name "write")
     "Create or replace a file with content, written verbatim and atomically; missing parent directories are created. Output: \"Wrote <path> (<n> bytes)\" then \"hash: <digest>\". Pass expected_hash to refuse the write when the file changed since you read it.")
    ((string= name "edit")
     "Replace old_string with new_string in a file. old_string must match exactly (including whitespace and line endings) and exactly once, unless replace_all is true. Output: the replacement count, a unified diff, then \"hash: <digest>\".")
    ((string= name "multi_edit")
     "Apply several edits to one file in order, each to the result of the previous one, with edit's rules; all or nothing. Output: the total replacement count, one unified diff, then \"hash: <digest>\".")
    ((string= name "glob")
     "List files whose path relative to path matches a gitignore-style glob (*, ?, [...], ** for any depth; \"*.lisp\" is top level only). Output: root-relative paths, sorted, one per line. Ignored files and .git/ are left out.")
    ((string= name "grep")
     "Search text files for a literal string (not a regex). Output: one \"<path>:<line>:<text>\" line per match, sorted by path then line. Binary, ignored and .git/ files are skipped.")
    ((string= name "ls")
     "List the entries directly in a directory, sorted by name, one per line; directories end with \"/\". Ignored entries and .git/ are left out.")))

(defun %tool-schema (name)
  (cond
    ((string= name "read")
     (%object-schema
      `(("file_path" "string" ,+path-description+)
        ("offset" "integer" "1-based number of the first line to show (default 1)." "minimum" 1)
        ("limit" "integer" "Number of lines to show (default and maximum: the tools' max-read-lines)." "minimum" 1))
      '("file_path")))
    ((string= name "write")
     (%object-schema
      `(("file_path" "string" ,+path-description+)
        ("content" "string" "The complete new content of the file.")
        ("expected_hash" "string" ,+expected-hash-description+))
      '("file_path" "content")))
    ((string= name "edit")
     (%object-schema
      `(("file_path" "string" ,+path-description+)
        ("old_string" "string" "Exact text to replace; must occur exactly once unless replace_all is true.")
        ("new_string" "string" "Replacement text (empty deletes old_string); must differ from old_string.")
        ("replace_all" "boolean" "Replace every non-overlapping occurrence (default false).")
        ("expected_hash" "string" ,+expected-hash-description+))
      '("file_path" "old_string" "new_string")))
    ((string= name "multi_edit")
     (%object-schema
      `(("file_path" "string" ,+path-description+)
        ("edits" "array" "Edits applied in order, each to the previous edit's result; any failure writes nothing."
                 "minItems" 1
                 "items" ,(%object-schema
                           '(("old_string" "string" "Exact text to replace; must occur exactly once unless replace_all is true.")
                             ("new_string" "string" "Replacement text; must differ from old_string.")
                             ("replace_all" "boolean" "Replace every non-overlapping occurrence (default false)."))
                           '("old_string" "new_string")))
        ("expected_hash" "string" ,+expected-hash-description+))
      '("file_path" "edits")))
    ((string= name "glob")
     (%object-schema
      `(("pattern" "string" "gitignore-style glob matched against the whole path relative to path, e.g. \"**/*.lisp\".")
        ("path" "string" ,(concatenate 'string "Directory to search (default: the project root). " +path-description+)))
      '("pattern")))
    ((string= name "grep")
     (%object-schema
      `(("pattern" "string" "Literal text to search for.")
        ("path" "string" ,(concatenate 'string "Directory or file to search (default: the project root). " +path-description+))
        ("include" "string" "Only search files matching this gitignore-style pattern relative to path, e.g. \"*.lisp\" (any depth).")
        ("regex" "boolean" "Must be false or absent: no regex engine is loaded.")
        ("case_insensitive" "boolean" "Ignore case when matching (default false).")
        ("max_results" "integer" "Maximum number of matching lines (default and maximum: the tools' max-results)." "minimum" 1))
      '("pattern")))
    ((string= name "ls")
     (%object-schema
      `(("path" "string" ,(concatenate 'string "Directory to list (default: the project root). " +path-description+)))))))

;;; --- protocol methods ------------------------------------------------------

(defmethod agent:list-agent-tools ((source file-tools) &key context)
  "→ fresh LLM-PROTOCOL:LLM-TOOL descriptors for the enabled names, in
+FILE-TOOL-NAMES+ order, with draft-07 JSON Schema parameters as
string-keyed hash tables."
  (declare (ignore context))
  (loop for name in (file-tools-names source)
        collect (llm:make-llm-tool :name name
                                   :description (%tool-description name)
                                   :parameters (%tool-schema name))))

(defmethod agent:tool-executable-p ((source file-tools) name &key context)
  (declare (ignore context))
  (%enabled-p source name))

(defmethod agent:invoke-tool-async ((source file-tools) name arguments
                                    &key context callback error-callback)
  "CALL-FILE-TOOL off the event loop; the result string goes to CALLBACK, a
signalled condition to ERROR-CALLBACK."
  (if (%enabled-p source name)
      ;; The protocol's sync function-tool path: a worker thread, then the
      ;; value or the ERROR delivered on the loop.
      (agent:invoke-tool-async
       (agent:make-function-tool
        :name name
        :handler (lambda (args) (call-file-tool source name args)))
       name arguments
       :context context :callback callback :error-callback error-callback)
      ;; Default (source t) method: AGENT-UNKNOWN-TOOL on the loop.
      (call-next-method)))

(defun %path-argument (name arguments)
  "The path argument string of tool NAME's ARGUMENTS (\".\" when glob / grep /
ls omit it), or NIL when ARGUMENTS do not parse or the key is missing or not a
non-empty string."
  (let ((args (handler-case (%decode-arguments arguments)
                (agent:tool-input-error () nil)))
        (key (cond ((member name '("read" "write" "edit" "multi_edit") :test #'equal)
                    "file_path")
                   ((member name '("glob" "grep" "ls") :test #'equal)
                    "path"))))
    (when (and args key)
      (multiple-value-bind (v present) (gethash key args)
        (cond ((or (not present) (eq v :null))
               (and (string= key "path") "."))
              ((and (stringp v) (plusp (length v))) v))))))

(defun %relative-or-nil (source arg)
  (handler-case (nth-value 1 (resolve-tool-path source arg))
    (agent:tool-failure () nil)
    (fs:path-error () nil)))

(defmethod agent:tool-permission-resources ((source file-tools) name arguments
                                            &key context)
  "→ (\"path:<relative>\") for the file or directory the call touches. Never
signals: an argument that does not resolve gives (\"path:<argument>\")."
  (declare (ignore context))
  (let ((arg (%path-argument name arguments)))
    (if arg
        (list (concatenate 'string "path:" (or (%relative-or-nil source arg) arg)))
        (list "*"))))

(defmethod agent:tool-instruction-paths ((source file-tools) name arguments
                                         &key context)
  "→ (\"<relative>\") the touched path, for AGENTS.md discovery; NIL when the
argument does not resolve under the root."
  (declare (ignore context))
  (let* ((arg (%path-argument name arguments))
         (rel (and arg (%relative-or-nil source arg))))
    (and rel (list rel))))
