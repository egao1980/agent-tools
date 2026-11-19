(in-package #:agent-tools/tests)

;;; read: numbered lines (cat -n style: 6-column right-aligned number, TAB,
;;; line without terminator), a truncation notice when lines remain, and a
;;; final "hash: <digest>" line (the whole file's content hash, for CAS).

(defun read-notice (from to)
  (format nil "[truncated: lines ~d-~d shown; call read with offset=~d for more]"
          from to (1+ to)))

(defun ten-lines ()
  (apply #'nl (loop for i from 1 to 10 collect (format nil "line ~d" i))))

(deftest-backends read-whole-file
  (let ((text (nl "a" "b" "c")))
    (with-tools (tools root kind) `(("src/a.txt" ,text))
      (let ((out (tool tools "read" "file_path" "src/a.txt")))
        (ok (equal (list (numbered 1 "a") (numbered 2 "b") (numbered 3 "c")
                         (concatenate 'string "hash: " (digest text)))
                   (lines out)))
        (ok (char/= #\Linefeed (char out (1- (length out)))) "no LF after the hash line")
        (ok (equal (digest text) (result-hash out)))
        (ok (equal (p:content-hash (p:join root "src/a.txt")) (result-hash out)))))))

(deftest-backends read-text-shapes
  (with-tools (tools root kind) '(("noeol.txt" "a
b")
                                  ("empty.txt" "")
                                  ("blank.txt" "

")
                                  ("utf8.txt" "héllo ✓
"))
    (ok (equal (list (numbered 1 "a") (numbered 2 "b")) (body-lines (tool tools "read" "file_path" "noeol.txt")))
        "no final LF")
    (ok (equal (list "[empty file]" (concatenate 'string "hash: " (digest "")))
               (lines (tool tools "read" "file_path" "empty.txt"))))
    (ok (equal (list (numbered 1 "") (numbered 2 ""))
               (body-lines (tool tools "read" "file_path" "blank.txt"))))
    (ok (equal (list (numbered 1 "héllo ✓"))
               (body-lines (tool tools "read" "file_path" "utf8.txt"))))))

(deftest-backends read-crlf-fixture
  (with-root (root kind)
    (let ((bytes (fixture-octets "crlf.txt")))
      (p:write-bytes (p:join root "crlf.txt") bytes)
      (let ((out (tool (make-file-tools :root root) "read" "file_path" "crlf.txt")))
        (ok (equal (list (numbered 1 "alpha") (numbered 2 "beta") (numbered 3 "gamma"))
                   (body-lines out))
            "CR is not shown")
        (ok (equal (p:octets-hash bytes) (result-hash out))
            "the hash is over the bytes on disk")))))

(deftest-backends read-offset-and-limit
  (with-tools (tools root kind) `(("f.txt" ,(ten-lines)))
    (ok (equal (list (numbered 3 "line 3") (numbered 4 "line 4") (read-notice 3 4))
               (body-lines (tool tools "read" "file_path" "f.txt" "offset" 3 "limit" 2))))
    (ok (equal (list (numbered 9 "line 9") (numbered 10 "line 10"))
               (body-lines (tool tools "read" "file_path" "f.txt" "offset" 9 "limit" 5)))
        "no notice when the range reaches the end")
    (ok (equal (list (numbered 10 "line 10"))
               (body-lines (tool tools "read" "file_path" "f.txt" "offset" 10))))
    (ok (equal (digest (ten-lines))
               (result-hash (tool tools "read" "file_path" "f.txt" "offset" 3 "limit" 2)))
        "a ranged read still carries the whole file's hash")))

(deftest-backends read-max-read-lines
  (with-tools (tools root kind :max-read-lines 4) `(("f.txt" ,(ten-lines)))
    (ok (equal (append (loop for i from 1 to 4 collect (numbered i (format nil "line ~d" i)))
                       (list (read-notice 1 4)))
               (body-lines (tool tools "read" "file_path" "f.txt"))))
    (ok (equal (append (loop for i from 5 to 8 collect (numbered i (format nil "line ~d" i)))
                       (list (read-notice 5 8)))
               (body-lines (tool tools "read" "file_path" "f.txt" "offset" 5 "limit" 50)))
        "limit is capped at max-read-lines")))

(deftest-backends read-max-output
  ;; Each numbered line is 6 + 1 + 20 + LF = 28 characters; 100 fit 3.
  (let ((text (apply #'nl (loop for i from 1 to 20
                                collect (format nil "~20,,,'xa" (format nil "~d" i))))))
    (with-tools (tools root kind :max-output 100) `(("wide.txt" ,text))
      (let ((body (body-lines (tool tools "read" "file_path" "wide.txt"))))
        (ok (= 4 (length body)))
        (ok (equal (read-notice 1 3) (car (last body))))
        (ok (equal (numbered 1 (format nil "~20,,,'xa" "1")) (first body)))))))

(deftest-backends read-argument-errors
  (with-tools (tools root kind) '(("f.txt" "a
b
c
"))
    (flet ((input-error-p (c &rest needles)
             (and (typep c 'a:tool-input-error)
                  (every (lambda (n) (has n (message c))) needles))))
      (ok (input-error-p (failure (tool tools "read" "file_path" "f.txt" "offset" 5))
                         "offset" "3 lines")
          "offset past the end")
      (ok (input-error-p (failure (tool tools "read" "file_path" "f.txt" "offset" 4))
                         "offset"))
      (ok (input-error-p (failure (tool tools "read" "file_path" "f.txt" "offset" 0)) "offset"))
      (ok (input-error-p (failure (tool tools "read" "file_path" "f.txt" "limit" 0)) "limit"))
      (ok (input-error-p (failure (tool tools "read" "file_path" "f.txt" "offset" "2")) "offset")
          "offset must be an integer")
      (ok (input-error-p (failure (tool tools "read")) "file_path") "file_path is required")
      (ok (input-error-p (failure (tool tools "read" "file_path" 7)) "file_path"))
      (ok (input-error-p (failure (call-file-tool tools "read" "{not json")))
          "malformed JSON")
      (ok (input-error-p (failure (call-file-tool tools "read" "[\"f.txt\"]")))
          "not a JSON object"))))

(deftest-backends read-file-errors
  (with-tools (tools root kind) `(("dir/x.txt" "x")
                                  ("bin.dat" ,(coerce #(102 111 0 111) '(vector (unsigned-byte 8)))))
    (let ((c (failure (tool tools "read" "file_path" "nope.txt"))))
      (ok (typep c 'a:tool-input-error))
      (ok (not (typep c 'path-outside-root)))
      (ok (has "nope.txt" (message c)))
      (ok (has "not found" (message c))))
    (let ((c (failure (tool tools "read" "file_path" "dir"))))
      (ok (typep c 'a:tool-input-error))
      (ok (has "directory" (message c)))
      (ok (has "ls" (message c)) "points at ls"))
    (let ((c (failure (tool tools "read" "file_path" "bin.dat"))))
      (ok (typep c 'a:tool-input-error))
      (ok (has "binary" (message c))))))

(deftest-backends read-arguments-forms
  (with-tools (tools root kind) '(("f.txt" "a
") (".gitignore" "*.log
") ("x.log" "logged
"))
    (ok (equal (list (numbered 1 "a"))
               (body-lines (call-file-tool tools "read" (args "file_path" "f.txt"))))
        "a decoded hash table works like the JSON string")
    (ok (equal (list (numbered 1 "a"))
               (body-lines (tool tools "read" "file_path" "f.txt" "unknown_key" 1)))
        "unknown keys are ignored")
    (ok (equal (list (numbered 1 "logged"))
               (body-lines (tool tools "read" "file_path" "x.log")))
        "ignore rules do not apply to an explicit path")))

(deftest read-zip-filesystem
  (let* ((root (make-zip-root '(("src/a.txt" "hi
"))))
         (tools (make-file-tools :root root)))
    (ok (equal (list (numbered 1 "hi")) (body-lines (tool tools "read" "file_path" "src/a.txt"))))
    (ok (equal (digest (nl "hi")) (result-hash (tool tools "read" "file_path" "src/a.txt"))))))
