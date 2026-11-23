(in-package #:agent-tools/tests)

;;; Shared helpers. Each backend test runs against a fresh empty directory:
;;; /work on a new memory filesystem, or a temp directory on the local
;;; filesystem (its truename). CL-STACK-PATHLIB:*FILESYSTEM* is bound to that
;;; backend while BODY runs.

(defun call-with-root (kind fn)
  (ecase kind
    (:memory
     (let ((fs (p:make-memory-filesystem)))
       (p:with-filesystem (fs)
         (p:mkdir "/work" :parents t)
         (funcall fn (p:ensure-path "/work" :directory t)))))
    (:local
     (let ((dir (uiop:ensure-directory-pathname
                 (merge-pathnames (format nil "agent-tools-~36R" (random (expt 36 8)))
                                  (uiop:temporary-directory)))))
       (ensure-directories-exist dir)
       (unwind-protect
            (p:with-filesystem (p::*local-filesystem*)
              (funcall fn (p:ensure-path (truename dir) :directory t)))
         (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))))

(defmacro with-root ((root kind) &body body)
  "Run BODY with ROOT bound to an empty directory PATH on backend KIND."
  `(call-with-root ,kind (lambda (,root) ,@body)))

(defun make-tree (root entries)
  "ENTRIES: (relpath content) with content a string or octet vector, or
\"dir/\" alone for an empty directory."
  (dolist (e entries root)
    (destructuring-bind (rel &optional (content "")) (if (consp e) e (list e))
      (if (and (plusp (length rel)) (char= (char rel (1- (length rel))) #\/))
          (p:mkdir (p:join root rel) :parents t :exist-ok t)
          (let ((path (p:join root rel)))
            (p:mkdir (p:parent path) :parents t :exist-ok t)
            (if (stringp content)
                (p:write-text path content)
                (p:write-bytes path content)))))))

(defmacro with-tools ((tools root kind &rest keys) entries &body body)
  "ROOT on backend KIND holding ENTRIES (see MAKE-TREE); TOOLS a FILE-TOOLS
over it made with KEYS."
  `(with-root (,root ,kind)
     (make-tree ,root ,entries)
     (let ((,tools (make-file-tools :root ,root ,@keys)))
       ,@body)))

(defun args (&rest kv)
  "String-keyed hash table from alternating keys and values (json-protocol's
object shape). NIL values are JSON false."
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kv by #'cddr do (setf (gethash k h) v))
    h))

(defun json-args (&rest kv)
  "JSON object string (what a model sends) from alternating keys and values."
  (json:encode (apply #'args kv)))

(defun tool (tools name &rest kv)
  "CALL-FILE-TOOL with arguments given as a JSON object string."
  (call-file-tool tools name (apply #'json-args kv)))

(defmacro failure (form)
  "The ERROR (or AI-AGENT-PROTOCOL:AGENT-CONDITION, e.g. AGENT-UNKNOWN-TOOL,
which is not an ERROR) FORM signals, or NIL when it returns."
  `(handler-case (progn ,form nil)
     (error (c) c)
     (a:agent-condition (c) c)))

(defun message (condition)
  (and condition (princ-to-string condition)))

(defun has (needle haystack)
  (and (stringp haystack) (search needle haystack) t))

(defun octets (string)
  (p::utf8-string-to-octets string))

(defun digest (string)
  "\"sha256:…\" of the UTF-8 octets of STRING."
  (p:octets-hash (octets string)))

(defun text-at (root rel)
  (p:read-text (p:join root rel)))

(defun exists-at (root rel)
  (p:exists-p (p:join root rel)))

(defun nl (&rest lines)
  "LINES each followed by LF."
  (format nil "~{~a~%~}" lines))

(defun crlf (&rest lines)
  "LINES each followed by CR LF."
  (with-output-to-string (s)
    (dolist (l lines)
      (write-string l s)
      (write-char #\Return s)
      (write-char #\Linefeed s))))

(defun lines (string)
  "STRING split at LF; a final LF does not start an empty line."
  (let ((out '()) (start 0))
    (loop for i = (position #\Linefeed string :start start)
          while i
          do (push (subseq string start i) out)
             (setf start (1+ i)))
    (when (< start (length string))
      (push (subseq string start) out))
    (nreverse out)))

(defun last-line (string)
  (car (last (lines string))))

(defun result-hash (result)
  "The digest of a result whose last line is \"hash: <digest>\", else NIL."
  (let ((l (last-line result)))
    (and l (> (length l) 6) (string= "hash: " l :end2 6) (subseq l 6))))

(defun numbered (n line)
  "One read output line: N right-aligned in 6 columns, TAB, LINE."
  (format nil "~6d~c~a" n #\Tab line))

(defun body-lines (result)
  "Lines of a read RESULT before its \"hash: \" footer."
  (butlast (lines result)))

(defun unified (old new)
  "diff-protocol's unified hunks for OLD → NEW with 3 lines of context."
  (d:render-diff (d:compute-diff (d:find-diff-engine :text) old new :context 3)
                 :unified nil))

(defun fixture-octets (name)
  (with-open-file (in (asdf:system-relative-pathname
                       "agent-tools/tests" (format nil "tests/fixtures/~a" name))
                      :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence v in)
      v)))

(defmacro deftest-backends (name &body body)
  "A test run once per backend, with KIND bound to :MEMORY or :LOCAL."
  `(deftest-parametrize ,name
       ((kind) :ids ("memory" "local") (:memory) (:local))
     ,@body))

(defun list-names (dir)
  "Sorted names of the entries directly in DIR."
  (sort (mapcar #'p:name (p:iterdir dir)) #'string<))

;;; A memory filesystem that lets another writer win the race: when ARMED
;;; holds a string, the next FS-WRITE-ATOMIC first overwrites the target with
;;; it, as a concurrent edit landing between the tool's read and its write.

(defclass racing-filesystem (p:memory-filesystem)
  ((armed :initform nil :accessor racing-armed)))

(defmethod p:fs-write-atomic :around ((fs racing-filesystem) pathname bytes
                                      &key expected-hash &allow-other-keys)
  (declare (ignore bytes expected-hash))
  (let ((theirs (shiftf (racing-armed fs) nil)))
    (when theirs
      (p:fs-write-bytes fs pathname (octets theirs) :if-exists :supersede)))
  (call-next-method))

(defun make-racing-filesystem ()
  (let ((fs (make-instance 'racing-filesystem)))
    (setf (gethash "/" (p::memory-fs-root fs)) :dir)
    fs))

;;; A memory filesystem whose reads meet (cl-stack-code#30): while GATE is a
;;; count N > 0, each FS-READ-BYTES waits, up to a second, until N reads have
;;; arrived, so tool calls that would read before any of them writes really
;;; overlap. The first wait that times out opens the gate for every later read.

(defclass rendezvous-filesystem (p:memory-filesystem)
  ((lock :initform (bt2:make-lock :name "rendezvous") :reader %rendezvous-lock)
   (cv :initform (bt2:make-condition-variable :name "rendezvous") :reader %rendezvous-cv)
   (arrived :initform 0 :accessor %rendezvous-arrived)
   (gate :initform 0 :accessor rendezvous-gate)))

(defmethod p:fs-read-bytes :before ((fs rendezvous-filesystem) pathname)
  (declare (ignore pathname))
  (bt2:with-lock-held ((%rendezvous-lock fs))
    (when (plusp (rendezvous-gate fs))
      (incf (%rendezvous-arrived fs))
      (let ((deadline (+ (get-internal-real-time) internal-time-units-per-second)))
        (loop while (and (plusp (rendezvous-gate fs))
                         (< (%rendezvous-arrived fs) (rendezvous-gate fs))
                         (< (get-internal-real-time) deadline))
              do (bt2:condition-wait (%rendezvous-cv fs) (%rendezvous-lock fs) :timeout 0.05)))
      (setf (rendezvous-gate fs) 0)
      (bt2:condition-broadcast (%rendezvous-cv fs)))))

(defun make-rendezvous-filesystem ()
  (let ((fs (make-instance 'rendezvous-filesystem)))
    (setf (gethash "/" (p::memory-fs-root fs)) :dir)
    fs))

(defun make-zip-root (entries)
  "Root PATH \"/\" of a read-only zip filesystem over ENTRIES ((name text) …)."
  (p:clear-zip-filesystem-cache)
  (p:ensure-path "/" :directory t
                     :filesystem (p:make-zip-filesystem
                                  "<agent-tools>" :bytes (p:write-zip-bytes entries)
                                                  :cache nil)))

;;; Agent runs against the mock LLM backend.

(defmacro with-agent-loop (&body body)
  `(let* ((eb (event-backend-libuv:make-libuv-backend))
          (el (event-protocol:make-event-loop eb)))
     (event-protocol:with-event-backend (eb)
       (event-protocol:with-event-loop-var (el)
         ,@body))))

(defun scripted-backend (steps box)
  "STEPS: tool calls (name . json-arguments) or a string (final reply). BOX's
car collects the turns each generate saw, oldest first."
  (let ((n 0))
    (llm:make-mock-llm-backend
     :handler (lambda (b turns &key &allow-other-keys)
                (declare (ignore b))
                (setf (car box) (append (car box) (list (copy-list turns))))
                (let ((step (nth (min n (1- (length steps))) steps)))
                  (incf n)
                  (if (stringp step)
                      (llm:make-llm-response
                       :parts (list (llm:make-llm-text-part :text step))
                       :finish-reason :stop)
                      (llm:make-llm-response
                       :parts (list (llm:make-llm-tool-call-part
                                     :id (format nil "c~d" n)
                                     :name (car step) :arguments (cdr step)))
                       :finish-reason :tool-use)))))))

(defun tool-results (run)
  "Tool result parts of RUN's turns, oldest first."
  (loop for turn in (a:agent-run-turns run)
        append (remove-if-not #'llm:llm-tool-result-part-p (llm:llm-turn-parts turn))))

(defun result-content (part) (llm:llm-tool-result-part-content part))
(defun result-error-p (part) (llm:llm-tool-result-part-error-p part))
