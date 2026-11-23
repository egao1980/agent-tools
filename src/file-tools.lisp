(in-package #:agent-tools)

;;; FILE-TOOLS: read, write, edit, multi_edit, glob, grep and ls over one
;;; directory tree of a cl-stack-pathlib filesystem.
;;;
;;; Invariants (see .team-solve/notes.md, Proposal A):
;;; - I-thread: handlers run in worker threads. Only PATHs derived from
;;;   FILE-TOOLS-ROOT reach pathlib, so no handler reads *FILESYSTEM* (a
;;;   string designator would).
;;; - I-confine: every touched path is (JOIN ROOT REL) with REL lexically
;;;   normalized; every existing prefix that is a symlink resolves under the
;;;   root; walkers never descend a directory symlink.
;;; - I-cas: edit / multi_edit decode the same octets they hash and write with
;;;   that hash as :EXPECTED-HASH.
;;; - I-restart: STALE-FILE-ERROR for a CAS write is signalled from a
;;;   HANDLER-BIND handler on CONTENT-HASH-MISMATCH, and nothing between the
;;;   write and the caller unwinds on it, so pathlib's OVERWRITE and
;;;   USE-CURRENT stay visible.
;;; - I-serial: write / edit / multi_edit hold their target path's lock
;;;   (%WITH-PATH-LOCK) from the read to the write, so calls on one file
;;;   apply one after the other, each to what the previous one wrote.

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
          :documentation "Internal: the LLM-TOOL descriptors, by name. Unused:
LIST-AGENT-TOOLS builds fresh descriptors on every call."))
  (:documentation "Tool source over one directory tree of a pathlib filesystem."))

(defun file-tools-p (x)
  (typep x 'file-tools))

;;; --- failures --------------------------------------------------------------

(defun %input-error (control &rest args)
  (error 'agent:tool-input-error :message (apply #'format nil control args)))

(defun %outside (tools designator)
  (error 'path-outside-root
         :path designator
         :message (format nil "~a is outside the project root ~a; use a path relative to the root"
                          designator (fs:as-posix (file-tools-root tools)))))

(defun %stale-message (rel expected actual)
  (format nil "~a changed since it was read (expected ~a, now ~a); read it again and retry"
          rel expected (or actual "missing")))

;;; --- arguments -------------------------------------------------------------

(defun %blank-p (string)
  (every (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) string))

(defun %decode-arguments (arguments)
  "ARGUMENTS (a JSON object string or a string-keyed hash table) → a
string-keyed hash table. NIL or a blank string count as {}."
  (let ((value (typecase arguments
                 (hash-table arguments)
                 (null (make-hash-table :test #'equal))
                 (string
                  (if (%blank-p arguments)
                      (make-hash-table :test #'equal)
                      (handler-case (json:decode arguments)
                        (json:json-error (c)
                          (%input-error "arguments are not valid JSON (~a); send one JSON object with named keys"
                                        c)))))
                 (t arguments))))
    (unless (hash-table-p value)
      (%input-error "arguments must be a JSON object with named keys"))
    value))

(defun %describe-value (v)
  (cond ((stringp v) (format nil "~s" v))
        ((eq v t) "true")
        ((null v) "false")
        ((hash-table-p v) "an object")
        ((vectorp v) "an array")
        (t (format nil "~a" v))))

(defun %arg (args key type &key required default)
  "Typed argument KEY of the decoded ARGS. Absent or JSON null → DEFAULT, or
a TOOL-INPUT-ERROR when REQUIRED. TYPE: :STRING, :BOOLEAN (JSON false is NIL),
:INTEGER (>= 1), :ARRAY (a vector), :OBJECT (a hash table)."
  (multiple-value-bind (v present) (gethash key args)
    (when (eq v :null) (setf present nil))
    (cond ((not present)
           (if required (%input-error "~a is required" key) default))
          ((ecase type
             (:string (stringp v))
             (:boolean (or (eq v t) (null v)))
             (:integer (and (integerp v) (>= v 1)))
             (:array (and (vectorp v) (not (stringp v))))
             (:object (hash-table-p v)))
           v)
          (t (%input-error "~a must be ~a, got ~a" key
                           (ecase type
                             (:string "a string")
                             (:boolean "true or false")
                             (:integer "an integer >= 1")
                             (:array "an array")
                             (:object "an object"))
                           (%describe-value v))))))

;;; --- paths -----------------------------------------------------------------

(defun %segments (string)
  "Non-empty segments of STRING split at / (and \\, which pathlib's POSIX
parser also treats as a separator)."
  (remove "" (uiop:split-string (substitute #\/ #\\ string) :separator "/")
          :test #'string=))

(defun %normalize (segments)
  "Lexical normalization: . drops, .. pops (\"/..\" stays \"/\")."
  (let ((out '()))
    (dolist (s segments (nreverse out))
      (cond ((string= s "."))
            ((string= s "..") (pop out))
            (t (push s out))))))

(defun %prefix-p (prefix list)
  (and (<= (length prefix) (length list))
       (every #'string= prefix list)))

(defun %root-segments (tools)
  (%segments (fs:as-posix (file-tools-root tools))))

(defun %inside-root-p (tools path)
  "PATH (a symlink) resolves under the root. Fails closed."
  (let ((true (handler-case (fs:resolve path :strict nil)
                (fs:path-error () nil))))
    (and true
         (%prefix-p (%root-segments tools) (%normalize (%segments (fs:as-posix true)))))))

(defun %join-segments (segments)
  (format nil "~{~a~^/~}" segments))

(defun %absolute-designator-p (designator)
  "DESIGNATOR names an absolute path: it starts with / or \\ (a UNC
\\\\server\\share too), or with a drive letter (C:/x, C:\\x, C:x)."
  (let ((c (char designator 0)))
    (or (char= c #\/)
        (char= c #\\)
        (and (alpha-char-p c)
             (> (length designator) 1)
             (char= (char designator 1) #\:)))))

(defun resolve-tool-path (tools designator &key (allow-root t))
  "Resolve the path argument DESIGNATOR (a string: relative to the root, or
absolute and under it; / and \\ both separate segments) on TOOLS.
→ (values path relative) where PATH is an absolute CL-STACK-PATHLIB:PATH and
RELATIVE the root-relative POSIX string (\".\" for the root itself). Outside the root signals PATH-OUTSIDE-ROOT; an
empty string, a NUL, a segment below the root holding ':' (a drive or
stream designator), or the root when ALLOW-ROOT is NIL, signals
AI-AGENT-PROTOCOL:TOOL-INPUT-ERROR."
  (unless (stringp designator)
    (%input-error "the path must be a string, got ~a" (%describe-value designator)))
  (when (zerop (length designator))
    (%input-error "the path is empty; give a path relative to the project root"))
  (when (find (code-char 0) designator)
    (%input-error "~s contains a NUL character; use a path relative to the project root ~a"
                  designator (fs:as-posix (file-tools-root tools))))
  (let* ((root (file-tools-root tools))
         (rsegs (%root-segments tools))
         (full (%normalize (if (%absolute-designator-p designator)
                               (%segments designator)
                               (append rsegs (%segments designator))))))
    (unless (%prefix-p rsegs full)
      (%outside tools designator))
    (let* ((tail (nthcdr (length rsegs) full))
           (rel (if tail (%join-segments tail) ".")))
      ;; Below the root a ":" is a drive (C:) or stream (a:b) designator:
      ;; FS:JOIN may parse it as a path outside the root (issue #1).
      (when (some (lambda (s) (find #\: s)) tail)
        (%input-error "~a has a ':' in a path segment (a drive or stream designator); use a path relative to the project root ~a"
                      designator (fs:as-posix root)))
      (when (and (null tail) (not allow-root))
        (%input-error "~a is the project root, not a file; give a file path" designator))
      ;; Each existing prefix: a symlink must stay under the root. A dangling
      ;; link counts (a write would follow it), so SYMLINK-P comes first.
      (loop for i from 1 to (length tail)
            for prefix = (fs:join root (%join-segments (subseq tail 0 i)))
            do (cond ((fs:symlink-p prefix)
                      (unless (%inside-root-p tools prefix)
                        (%outside tools designator)))
                     ((not (fs:exists-p prefix))
                      (loop-finish))))
      (values (if tail (fs:join root rel) root) rel))))

;;; --- text ------------------------------------------------------------------

(defun %utf8-decode (bytes)
  "Strict UTF-8 decode of the octet vector BYTES → a simple string, or NIL
when BYTES is not valid UTF-8."
  (let ((n (length bytes))
        (i 0)
        (out (make-array (length bytes) :element-type 'character :fill-pointer 0)))
    (flet ((cont (k)
             (let ((b (and (< k n) (aref bytes k))))
               (and b (= (logand b #xC0) #x80) (logand b #x3F)))))
      (loop while (< i n)
            do (let ((b0 (aref bytes i)))
                 (multiple-value-bind (code width)
                     (cond ((< b0 #x80) (values b0 1))
                           ((< b0 #xC2) (values nil 1))
                           ((< b0 #xE0)
                            (let ((c1 (cont (+ i 1))))
                              (values (and c1 (logior (ash (logand b0 #x1F) 6) c1)) 2)))
                           ((< b0 #xF0)
                            (let ((c1 (cont (+ i 1))) (c2 (cont (+ i 2))))
                              (values (and c1 c2
                                           (logior (ash (logand b0 #x0F) 12) (ash c1 6) c2))
                                      3)))
                           ((< b0 #xF5)
                            (let ((c1 (cont (+ i 1))) (c2 (cont (+ i 2))) (c3 (cont (+ i 3))))
                              (values (and c1 c2 c3
                                           (logior (ash (logand b0 #x07) 18) (ash c1 12)
                                                   (ash c2 6) c3))
                                      4)))
                           (t (values nil 1)))
                   (when (or (null code)
                             (and (= width 3) (< code #x800))
                             (and (= width 4) (or (< code #x10000) (> code #x10FFFF)))
                             (<= #xD800 code #xDFFF))
                     (return-from %utf8-decode nil))
                   (vector-push (code-char code) out)
                   (incf i width)))))
    (coerce out 'simple-string)))

(defun %decode-text (bytes rel)
  (or (%utf8-decode bytes)
      (%input-error "~a is not valid UTF-8 text; only text files can be read or edited" rel)))

(defun %utf8-length (string)
  (loop for c across string
        for k = (char-code c)
        sum (cond ((< k #x80) 1) ((< k #x800) 2) ((< k #x10000) 3) (t 4))))

(defun %binary-p (bytes)
  (find 0 bytes :end (min 8000 (length bytes))))

(defun %split-lines (text)
  "Lines of TEXT as a vector: a final LF starts no line; one CR before each LF
is dropped."
  (let ((out '()) (start 0) (n (length text)))
    (loop for nl = (position #\Linefeed text :start start)
          while nl
          do (push (subseq text start nl) out)
             (setf start (1+ nl)))
    (when (< start n)
      (push (subseq text start) out))
    (map 'vector (lambda (l)
                   (let ((k (length l)))
                     (if (and (plusp k) (char= (char l (1- k)) #\Return))
                         (subseq l 0 (1- k))
                         l)))
         (nreverse out))))

(defun %join-lines (lines)
  (format nil "~{~a~^~%~}" lines))

;;; --- one writer per path (I-serial) ----------------------------------------
;;;
;;; An agent runs the tool calls of one step at once, each on a worker thread.
;;; Two edits of one file would both read the original, and pathlib's CAS
;;; check and its rename are separate steps, so both writes could pass the
;;; check and the second would drop the first one's change while both report
;;; success (cl-stack-code#30). The locks are shared by every FILE-TOOLS
;;; source in the image (two sources over one tree serialize too) and striped:
;;; a fixed set indexed by a hash of the path, so unrelated files rarely wait
;;; on each other and the set never grows. Recursive, so a handler that runs
;;; inside a write (a STALE-FILE-ERROR handler choosing OVERWRITE) may call
;;; the tools again on the same thread.

(defconstant +path-lock-count+ 64)

(defvar *path-locks*
  (let ((v (make-array +path-lock-count+)))
    (dotimes (i +path-lock-count+ v)
      (setf (svref v i) (bt2:make-recursive-lock :name "agent-tools file write"))))
  "The striped write locks, see %PATH-LOCK.")

(defun %path-lock (path)
  "The write lock of PATH (an absolute CL-STACK-PATHLIB:PATH)."
  (svref *path-locks* (mod (sxhash (fs:as-posix path)) +path-lock-count+)))

(defmacro %with-path-lock ((path) &body body)
  "Run BODY holding PATH's write lock."
  `(bt2:with-recursive-lock-held ((%path-lock ,path))
     ,@body))

;;; --- CAS write -------------------------------------------------------------

(defun %cas-write (path rel text expected)
  "Atomic write of TEXT to PATH, compare-and-swap on EXPECTED (NIL: blind).
A mismatch signals STALE-FILE-ERROR inside pathlib's CONTENT-HASH-MISMATCH
handler, so OVERWRITE and USE-CURRENT stay available.
→ (values new-hash written-p)."
  (handler-bind ((fs:content-hash-mismatch
                   (lambda (c)
                     (let ((actual (fs:content-hash-mismatch-actual c)))
                       (error 'stale-file-error
                              :path rel :expected expected :actual actual
                              :message (%stale-message rel expected actual))))))
    (multiple-value-bind (p hash written-p)
        (fs:write-text path text :atomic t :expected-hash expected)
      (declare (ignore p))
      (values hash written-p))))

;;; --- read ------------------------------------------------------------------

(defun %require-file (path rel directory-hint missing-hint)
  (cond ((fs:directory-p path)
         (%input-error "~a is a directory; ~a" rel directory-hint))
        ((not (fs:exists-p path))
         (%input-error "~a not found~@[; ~a~]" rel missing-hint))))

(defun %tool-read (tools args)
  (let* ((arg (%arg args "file_path" :string :required t))
         (cap (file-tools-max-read-lines tools))
         (offset (%arg args "offset" :integer :default 1))
         (limit (min (%arg args "limit" :integer :default cap) cap)))
    (multiple-value-bind (path rel) (resolve-tool-path tools arg :allow-root nil)
      (%require-file path rel "use ls to list it" "check the path with ls or glob")
      (let* ((bytes (fs:read-bytes path))
             (hash (fs:octets-hash bytes)))
        (when (%binary-p bytes)
          (%input-error "~a looks binary (NUL byte); read only shows text files" rel))
        (let* ((lines (%split-lines (%decode-text bytes rel)))
               (n (length lines)))
          (when (zerop n)
            (when (> offset 1)
              (%input-error "offset ~d is past the end of ~a (0 lines)" offset rel))
            (return-from %tool-read (format nil "[empty file]~%hash: ~a" hash)))
          (when (> offset n)
            (%input-error "offset ~d is past the end of ~a (~d line~:p)" offset rel n))
          (let ((last (min n (+ offset limit -1)))
                (budget (file-tools-max-output tools))
                (used 0)
                (shown (1- offset))
                (out '()))
            ;; The first line in range is always shown, so the notice always
            ;; moves the model forward.
            (loop for i from offset to last
                  for s = (format nil "~6d~c~a" i #\Tab (aref lines (1- i)))
                  while (or (= i offset) (<= (+ used (length s) 1) budget))
                  do (push s out)
                     (incf used (1+ (length s)))
                     (setf shown i))
            (when (< shown n)
              (push (format nil "[truncated: lines ~d-~d shown; call read with offset=~d for more]"
                            offset shown (1+ shown))
                    out))
            (push (format nil "hash: ~a" hash) out)
            (%join-lines (nreverse out))))))))

;;; --- write -----------------------------------------------------------------

(defun %tool-write (tools args)
  (let* ((arg (%arg args "file_path" :string :required t))
         (content (%arg args "content" :string :required t))
         (expected (%arg args "expected_hash" :string)))
    (multiple-value-bind (path rel) (resolve-tool-path tools arg :allow-root nil)
      (%with-path-lock (path)
        (when (fs:directory-p path)
          (%input-error "~a is a directory; write needs a file path" rel))
        (multiple-value-bind (hash written-p) (%cas-write path rel content expected)
          (if written-p
              (format nil "Wrote ~a (~d bytes)~%hash: ~a" rel (%utf8-length content) hash)
              ;; Only when a caller chose pathlib's USE-CURRENT.
              (format nil "Kept ~a unchanged (current content)~@[~%hash: ~a~]" rel hash)))))))

;;; --- edit / multi_edit -----------------------------------------------------

(defun %count-matches (needle text)
  "Non-overlapping occurrences of NEEDLE in TEXT, left to right."
  (loop with k = (length needle)
        for at = (search needle text) then (search needle text :start2 (+ at k))
        while at
        count t))

(defun %replace-matches (needle new text all)
  "TEXT with the first (ALL: every non-overlapping) NEEDLE replaced by NEW."
  (with-output-to-string (s)
    (let ((k (length needle)) (start 0))
      (loop for at = (search needle text :start2 start)
            while at
            do (write-string text s :start start :end at)
               (write-string new s)
               (setf start (+ at k))
            while all)
      (write-string text s :start start))))

(defun %edit-prefix (index)
  (if index (format nil "edit ~d: " index) ""))

(defun %check-edit (old new index)
  (when (zerop (length old))
    (%input-error "~aold_string must not be empty; copy the exact text to replace"
                  (%edit-prefix index)))
  (when (string= old new)
    (%input-error "~aold_string and new_string are identical; nothing to change"
                  (%edit-prefix index))))

(defun %apply-edit (text old new all rel index)
  "→ (values new-text replacements). Signals EDIT-MATCH-ERROR."
  (let ((n (%count-matches old text))
        (pre (%edit-prefix index)))
    (cond ((zerop n)
           (error 'edit-match-error
                  :path rel :count 0 :index index
                  :message (format nil "~aold_string not found in ~a; read the file and copy the exact text"
                                   pre rel)))
          ((and (> n 1) (not all))
           (error 'edit-match-error
                  :path rel :count n :index index
                  :message (format nil "~aold_string matched ~d times in ~a; add surrounding context to make it unique, or set replace_all"
                                   pre n rel)))
          (t (values (%replace-matches old new text all) (if all n 1))))))

(defun %edit-file (tools arg expected edits)
  "Apply EDITS, a list of (old new all index), to the file ARG names. One CAS
write against the digest of the content read, under the path's write lock
(I-serial); nothing is written on failure."
  (multiple-value-bind (path rel) (resolve-tool-path tools arg :allow-root nil)
    (multiple-value-bind (old-text text total new-hash)
        (%with-path-lock (path)
          (%require-file path rel "edit needs a file" "use write to create a new file")
          (let* ((bytes (fs:read-bytes path))
                 (hash (fs:octets-hash bytes))
                 (old-text (%decode-text bytes rel)))
            (when (and expected (string/= expected hash))
              (error 'stale-file-error
                     :path rel :expected expected :actual hash
                     :message (%stale-message rel expected hash)))
            (let ((text old-text) (total 0))
              (loop for (old new all index) in edits
                    do (multiple-value-bind (next n) (%apply-edit text old new all rel index)
                         (setf text next)
                         (incf total n)))
              (values old-text text total (%cas-write path rel text hash)))))
      ;; The diff outside the lock: the next writer need not wait for it.
      (format nil "Edited ~a: ~d replacement~:p~%--- a/~a~%+++ b/~a~%~ahash: ~a"
              rel total rel rel
              (diff:render-diff (diff:compute-diff (diff:find-diff-engine :text)
                                                   old-text text :context 3)
                                :unified nil)
              new-hash))))

(defun %tool-edit (tools args)
  (let* ((arg (%arg args "file_path" :string :required t))
         (old (%arg args "old_string" :string :required t))
         (new (%arg args "new_string" :string :required t))
         (all (%arg args "replace_all" :boolean))
         (expected (%arg args "expected_hash" :string)))
    (%check-edit old new nil)
    (%edit-file tools arg expected (list (list old new all nil)))))

(defun %tool-multi-edit (tools args)
  (let* ((arg (%arg args "file_path" :string :required t))
         (items (%arg args "edits" :array :required t))
         (expected (%arg args "expected_hash" :string)))
    (when (zerop (length items))
      (%input-error "edits must contain at least one edit"))
    ;; Every argument error before anything is read.
    (let ((edits (loop for item across items
                       for i from 1
                       collect (progn
                                 (unless (hash-table-p item)
                                   (%input-error "edits[~d] must be an object with old_string and new_string" i))
                                 (let ((old (%arg item "old_string" :string :required t))
                                       (new (%arg item "new_string" :string :required t))
                                       (all (%arg item "replace_all" :boolean)))
                                   (%check-edit old new i)
                                   (list old new all i))))))
      (%edit-file tools arg expected edits))))

;;; --- tree walking (glob, grep, ls) -----------------------------------------
;;;
;;; Own walker, not FS-WALK / FS-GLOB / FS-GREP / LOAD-IGNORE-FILES: on the
;;; local backend those descend directory symlinks (cl-stack-pathlib#23).
;;; Ignore files load as the walk goes (root to leaf, .gitignore then
;;; .ignore per directory, each with its own base), and the matcher always
;;; sees root-relative paths.

(defun %rel-join (dir-rel name)
  (if (string= dir-rel ".") name (format nil "~a/~a" dir-rel name)))

(defun %entry-name (path)
  (let ((s (string-right-trim "/" (fs:as-posix path))))
    (subseq s (1+ (or (position #\/ s :from-end t) -1)))))

(defun %dir-matcher (tools dir-path dir-rel parent)
  "The matcher in force for the entries of DIR: PARENT plus DIR's own ignore
files under :AUTO, the given matcher, or NIL."
  (let ((ignore (file-tools-ignore tools)))
    (cond ((null ignore) nil)
          ((fs:ignore-matcher-p ignore) ignore)
          (t (let ((base (if (string= dir-rel ".") "" (format nil "~a/" dir-rel)))
                   (m parent))
               (dolist (nm '(".gitignore" ".ignore") m)
                 (let ((f (fs:join dir-path nm)))
                   (when (and (fs:file-p f) (not (fs:directory-p f)))
                     (let ((text (%utf8-decode (fs:read-bytes f))))
                       (when text
                         (setf m (if m
                                     (fs:merge-ignore m (fs:parse-ignore text :base base))
                                     (fs:parse-ignore text :base base)))))))))))))

(defun %matcher-for (tools dir-rel)
  "The matcher in force inside DIR-REL: the ignore files of the root and of
every directory down to DIR-REL."
  (let* ((path (file-tools-root tools))
         (rel ".")
         (m (%dir-matcher tools path rel nil)))
    (unless (string= dir-rel ".")
      (dolist (seg (%segments dir-rel))
        (setf path (fs:join path seg)
              rel (%rel-join rel seg)
              m (%dir-matcher tools path rel m))))
    m))

(defun %entries (tools dir-path dir-rel matcher)
  "→ list of (name path rel dir-p link-p) directly in DIR, without .git/,
ignored entries and symlinks that resolve outside the root."
  (let ((seen (make-hash-table :test #'equal))
        (out '()))
    (dolist (e (fs:iterdir dir-path) (nreverse out))
      (let ((name (%entry-name e)))
        (unless (or (zerop (length name)) (gethash name seen))
          (setf (gethash name seen) t)
          ;; File form: SYMLINK-P on a directory-form entry is NIL
          ;; (cl-stack-pathlib#23).
          (let* ((path (fs:join dir-path name))
                 (rel (%rel-join dir-rel name))
                 (link-p (fs:symlink-p path))
                 (dir-p (fs:directory-p path)))
            (unless (or (and dir-p (string= name ".git"))
                        (and link-p (not (%inside-root-p tools path)))
                        (and matcher
                             (eq :ignored (fs:ignore-path-status matcher rel :directory dir-p))))
              (push (list name path rel dir-p link-p) out))))))))

(defun %walk-files (tools dir-path dir-rel matcher fn)
  "Call FN with (path rel) on every file below DIR; never descends a
directory symlink."
  (dolist (e (%entries tools dir-path dir-rel matcher))
    (destructuring-bind (name path rel dir-p link-p) e
      (declare (ignore name))
      (cond (dir-p
             (unless link-p
               (%walk-files tools path rel (%dir-matcher tools path rel matcher) fn)))
            ((fs:file-p path)
             (funcall fn path rel))))))

(defun %search-dir (tools args not-dir-hint &key allow-file)
  "Resolve the optional \"path\" argument. → (values path rel directory-p)."
  (let ((arg (%arg args "path" :string :default ".")))
    (multiple-value-bind (path rel) (resolve-tool-path tools arg)
      (let ((dir-p (fs:directory-p path)))
        (cond ((not (fs:exists-p path))
               (%input-error "~a not found; check the path with ls" rel))
              ((or dir-p allow-file)
               (values path rel dir-p))
              (t (%input-error "~a is not a directory; ~a" rel not-dir-hint)))))))

(defun %below (rel base-rel)
  "REL relative to the search directory BASE-REL."
  (if (string= base-rel ".") rel (subseq rel (1+ (length base-rel)))))

(defun %collect-files (tools path rel)
  "Sorted (rel . path) of every searchable file below the directory PATH."
  (let ((files '()))
    (%walk-files tools path rel (%matcher-for tools rel)
                 (lambda (p r) (push (cons r p) files)))
    (sort files #'string< :key #'car)))

;;; --- glob ------------------------------------------------------------------

(defun %tool-glob (tools args)
  (let ((pattern (%arg args "pattern" :string :required t)))
    (when (zerop (length pattern))
      (%input-error "pattern must not be empty; use **/* for every file"))
    (multiple-value-bind (path rel) (%search-dir tools args "glob needs a directory path")
      ;; A leading "/" anchors the pattern to the search directory and keeps a
      ;; leading # or ! literal.
      (let* ((pm (fs:parse-ignore (list (concatenate 'string "/" pattern))))
             (hits (loop for (r . nil) in (%collect-files tools path rel)
                         when (eq :ignored (fs:ignore-path-status pm (%below r rel)))
                           collect r))
             (n (length hits))
             (m (file-tools-max-results tools)))
        (cond ((zerop n) (format nil "No files matched ~a" pattern))
              ((> n m)
               (%join-lines
                (append (subseq hits 0 m)
                        (list (format nil "[truncated: showing ~d of ~d matches; narrow the pattern or path]"
                                      m n)))))
              (t (%join-lines hits)))))))

;;; --- grep ------------------------------------------------------------------

(defun %tool-grep (tools args)
  (let* ((pattern (%arg args "pattern" :string :required t))
         (regex (%arg args "regex" :boolean))
         (ci (%arg args "case_insensitive" :boolean))
         (include (%arg args "include" :string))
         (cap (file-tools-max-results tools))
         (limit (min (%arg args "max_results" :integer :default cap) cap)))
    (when regex
      (%input-error "regex: true is not supported (no regex engine is loaded); grep matches pattern literally, so send regex false"))
    (when (zerop (length pattern))
      (%input-error "pattern must not be empty"))
    (multiple-value-bind (path rel dir-p) (%search-dir tools args "" :allow-file t)
      (let* ((inc (and include (plusp (length include))
                       (fs:parse-ignore (list include))))
             (files (if dir-p
                        (%collect-files tools path rel)
                        (list (cons rel path))))
             (test (if ci #'char-equal #'char=))
             (matches '())
             (count 0))
        (block search
          (loop for (r . p) in files
                when (or (null inc)
                         (fs:ignore-path-p inc (if dir-p (%below r rel) (%entry-name p))))
                  do (let* ((bytes (fs:read-bytes p))
                            (text (and (not (%binary-p bytes)) (%utf8-decode bytes))))
                       (when text
                         (loop for line across (%split-lines text)
                               for i from 1
                               when (search pattern line :test test)
                                 do (push (format nil "~a:~d:~a" r i line) matches)
                                    (when (> (incf count) limit)
                                      (return-from search)))))))
        (setf matches (nreverse matches))
        (if (null matches)
            (format nil "No matches for ~a" pattern)
            (let ((budget (file-tools-max-output tools))
                  (used 0)
                  (out '())
                  (over nil))
              (loop for l in matches
                    for k from 1 to limit
                    do (if (<= (+ used (length l) 1) budget)
                           (progn (push l out) (incf used (1+ (length l))))
                           (progn (setf over t) (loop-finish))))
              (cond (over
                     (push (format nil "[truncated: output over ~d characters; narrow the pattern or path]"
                                   budget)
                           out))
                    ((> count limit)
                     (push (format nil "[truncated at ~d ~a; narrow the pattern or path]"
                                   limit (if (= limit 1) "match" "matches"))
                           out)))
              (%join-lines (nreverse out))))))))

;;; --- ls --------------------------------------------------------------------

(defun %tool-ls (tools args)
  (multiple-value-bind (path rel) (%search-dir tools args "use read to view a file")
    (let ((names (mapcar (lambda (e)
                           (destructuring-bind (name p r dir-p link-p) e
                             (declare (ignore p r link-p))
                             (cons name (if dir-p (concatenate 'string name "/") name))))
                         (%entries tools path rel (%matcher-for tools rel)))))
      (if (null names)
          "[empty directory]"
          (%join-lines (mapcar #'cdr (sort names #'string< :key #'car)))))))

;;; --- facade ----------------------------------------------------------------

(defparameter +file-tool-handlers+
  '(("read" . %tool-read) ("write" . %tool-write) ("edit" . %tool-edit)
    ("multi_edit" . %tool-multi-edit) ("glob" . %tool-glob) ("grep" . %tool-grep)
    ("ls" . %tool-ls)))

(defun %enabled-p (tools name)
  (and (stringp name) (member name (file-tools-names tools) :test #'string=) t))

(defun call-file-tool (tools name arguments)
  "Run tool NAME of TOOLS synchronously on ARGUMENTS (a JSON object string or
a string-keyed hash table, as json-protocol decodes one). → the result
string. Failures signal AI-AGENT-PROTOCOL:TOOL-FAILURE subtypes; a name that
is not enabled signals AI-AGENT-PROTOCOL:AGENT-UNKNOWN-TOOL."
  (unless (%enabled-p tools name)
    ;; An AGENT-CONDITION, not an ERROR; ERROR still makes it impossible to
    ;; ignore.
    (error 'agent:agent-unknown-tool
           :name name
           :message (format nil "~a is not enabled on this file-tools source (enabled: ~{~a~^, ~})"
                            name (file-tools-names tools))))
  (let ((fn (cdr (assoc name +file-tool-handlers+ :test #'string=))))
    ;; Only UNSUPPORTED-OPERATION is translated; never add an ERROR or
    ;; PATH-ERROR clause here (I-restart: CONTENT-HASH-MISMATCH is a
    ;; PATH-ERROR, and its restarts must survive to the caller's handler).
    (handler-case (funcall fn tools (%decode-arguments arguments))
      (fs:unsupported-operation ()
        (error 'agent:tool-unavailable
               :message (format nil "~a cannot run here: the filesystem is read-only" name))))))

;;; --- constructor -----------------------------------------------------------

(defun make-file-tools (&key root filesystem (ignore :auto)
                          (max-read-lines 2000) (max-output 50000)
                          (max-results 100) (tools +file-tool-names+))
  "→ a FILE-TOOLS source rooted at ROOT (a string, pathname or
CL-STACK-PATHLIB:PATH naming an existing directory). FILESYSTEM defaults to
ROOT's when ROOT is a PATH, else CL-STACK-PATHLIB:*FILESYSTEM*. TOOLS selects
the exposed tool names. A missing ROOT signals CL-STACK-PATHLIB:PATH-NOT-FOUND;
a ROOT that is not a directory signals CL-STACK-PATHLIB:PATH-ERROR; an unknown
tool name signals an ERROR."
  (check-type ignore (or (eql :auto) null fs:ignore-matcher))
  (check-type max-read-lines (integer 1))
  (check-type max-output (integer 1))
  (check-type max-results (integer 1))
  (check-type tools list)
  (let ((unknown (remove-if (lambda (n) (member n +file-tool-names+ :test #'equal)) tools)))
    (when unknown
      (error "make-file-tools: unknown tool name~p ~{~s~^, ~}; known: ~{~a~^, ~}"
             (length unknown) unknown +file-tool-names+)))
  (let* ((fsys (or filesystem
                   (if (fs:path-p root) (fs:path-filesystem root) fs:*filesystem*)))
         (p (fs:absolute (if (and (fs:path-p root) (null filesystem))
                             root
                             (fs:ensure-path root :filesystem fsys)))))
    (unless (fs:exists-p p)
      (error 'fs:path-not-found :path p :filesystem fsys
                                :message "file-tools root does not exist"))
    (unless (fs:directory-p p)
      (error 'fs:path-error :path p :filesystem fsys
                            :message "file-tools root is not a directory"))
    (make-instance 'file-tools
                   :root (fs:ensure-directory (fs:resolve p))
                   :filesystem fsys
                   :ignore ignore
                   :names (remove-if-not (lambda (n) (member n tools :test #'equal))
                                         +file-tool-names+)
                   :max-read-lines max-read-lines
                   :max-output max-output
                   :max-results max-results)))
