(in-package #:agent-tools/tests)

;;; edit: exact old_string → new_string, exactly once unless replace_all.
;;; Always a CAS write against the content the tool read. Result:
;;;   Edited <rel>: <n> replacement(s)
;;;   --- a/<rel>
;;;   +++ b/<rel>
;;;   <diff-protocol :unified hunks, 3 lines of context>
;;;   hash: <digest>

(defun edit-result (rel n old new)
  "The exact result text of an edit of REL from OLD to NEW with N replacements."
  (format nil "Edited ~a: ~d replacement~:p~%--- a/~a~%+++ b/~a~%~ahash: ~a"
          rel n rel rel (unified old new) (digest new)))

(deftest-backends edit-unique-match
  (let ((old (nl "one" "two" "three"))
        (new (nl "one" "TWO" "three")))
    (with-tools (tools root kind) `(("src/a.txt" ,old))
      (let ((out (tool tools "edit" "file_path" "src/a.txt"
                       "old_string" "two" "new_string" "TWO")))
        (ok (equal new (text-at root "src/a.txt")))
        (ok (equal (edit-result "src/a.txt" 1 old new) out))
        (ok (equal (p:content-hash (p:join root "src/a.txt")) (result-hash out)))))))

(deftest-backends edit-diff-context
  (let* ((old (apply #'nl (loop for i from 1 to 12 collect (format nil "l~d" i))))
         (new (apply #'nl (loop for i from 1 to 12
                                collect (if (= i 6) "SIX" (format nil "l~d" i))))))
    (with-tools (tools root kind) `(("f.txt" ,old))
      (let ((out (tool tools "edit" "file_path" "f.txt" "old_string" "l6
" "new_string" "SIX
")))
        (ok (equal (edit-result "f.txt" 1 old new) out))
        (ok (has (nl "@@ -3,7 +3,7 @@" " l3" " l4" " l5" "-l6" "+SIX" " l7" " l8" " l9") out)
            "three lines of context either side")))))

(deftest-backends edit-match-count-errors
  (let ((text (nl "x = 1" "y = 1" "z = 1")))
    (with-tools (tools root kind) `(("f.txt" ,text))
      (testing "no match"
        (let ((c (failure (tool tools "edit" "file_path" "f.txt"
                                "old_string" "w = 1" "new_string" "w = 2"))))
          (ok (typep c 'edit-match-error))
          (ok (typep c 'a:tool-input-error))
          (ok (eql 0 (and c (edit-match-error-count c))))
          (ok (equal "f.txt" (and c (edit-match-error-path c))))
          (ok (null (and c (edit-match-error-index c))))
          (ok (has "old_string not found in f.txt" (message c)))
          (ok (has "read" (message c)) "suggests reading the file again")))
      (testing "three matches"
        (let ((c (failure (tool tools "edit" "file_path" "f.txt"
                                "old_string" " = 1" "new_string" " = 2"))))
          (ok (typep c 'edit-match-error))
          (ok (eql 3 (and c (edit-match-error-count c))))
          (ok (has "old_string matched 3 times in f.txt" (message c)))
          (ok (has "add surrounding context" (message c)))
          (ok (has "replace_all" (message c)))))
      (testing "replace_all false is the default"
        (ok (typep (failure (tool tools "edit" "file_path" "f.txt" "old_string" " = 1"
                                  "new_string" " = 2" "replace_all" nil))
                   'edit-match-error)))
      (ok (equal text (text-at root "f.txt")) "nothing written on failure"))))

(deftest-backends edit-replace-all
  (with-tools (tools root kind) '(("f.txt" "a.b.c
") ("g.txt" "aaaa"))
    (let ((out (tool tools "edit" "file_path" "f.txt" "old_string" "." "new_string" "::"
                     "replace_all" t)))
      (ok (equal "a::b::c
" (text-at root "f.txt")))
      (ok (equal (edit-result "f.txt" 2 "a.b.c
" "a::b::c
") out)))
    (testing "matches are counted left to right without overlap"
      (let ((out (tool tools "edit" "file_path" "g.txt" "old_string" "aa" "new_string" "b"
                       "replace_all" t)))
        (ok (equal "bb" (text-at root "g.txt")))
        (ok (has "Edited g.txt: 2 replacements" out))))
    (testing "replace_all with a single match"
      (ok (has "Edited g.txt: 1 replacement
" (tool tools "edit" "file_path" "g.txt" "old_string" "bb" "new_string" "c"
              "replace_all" t)))
      (ok (equal "c" (text-at root "g.txt"))))))

(deftest-backends edit-multiline-and-delete
  (with-tools (tools root kind) `(("f.lisp" ,(nl "(defun f ()" "  1)" "" "(defun g ()" "  2)")))
    (tool tools "edit" "file_path" "f.lisp" "old_string" (nl "(defun f ()" "  1)" "")
          "new_string" "")
    (ok (equal (nl "(defun g ()" "  2)") (text-at root "f.lisp")) "empty new_string deletes")))

(deftest-backends edit-crlf-is-exact
  (with-root (root kind)
    (p:write-bytes (p:join root "crlf.txt") (fixture-octets "crlf.txt"))
    (let* ((tools (make-file-tools :root root))
           (out (tool tools "edit" "file_path" "crlf.txt" "old_string" "beta"
                      "new_string" "BETA")))
      (ok (equalp (octets (crlf "alpha" "BETA" "gamma"))
                  (p:read-bytes (p:join root "crlf.txt")))
          "other line endings untouched")
      (ok (equal (p:octets-hash (octets (crlf "alpha" "BETA" "gamma"))) (result-hash out)))
      (ok (typep (failure (tool tools "edit" "file_path" "crlf.txt"
                                "old_string" (nl "alpha" "BETA") "new_string" "x"))
                 'edit-match-error)
          "LF in old_string does not match CR LF: matching is exact"))))

(deftest-backends edit-argument-errors
  (with-tools (tools root kind) '(("f.txt" "abc
") "dir/")
    (let ((c (failure (tool tools "edit" "file_path" "f.txt" "old_string" "abc" "new_string" "abc"))))
      (ok (typep c 'a:tool-input-error))
      (ok (not (typep c 'edit-match-error)))
      (ok (has "identical" (message c))))
    (let ((c (failure (tool tools "edit" "file_path" "f.txt" "old_string" "" "new_string" "x"))))
      (ok (typep c 'a:tool-input-error))
      (ok (has "old_string" (message c))))
    (ok (has "new_string" (message (failure (tool tools "edit" "file_path" "f.txt"
                                                  "old_string" "abc")))))
    (let ((c (failure (tool tools "edit" "file_path" "nope.txt" "old_string" "a" "new_string" "b"))))
      (ok (typep c 'a:tool-input-error))
      (ok (has "not found" (message c)))
      (ok (has "write" (message c)) "points at write for new files"))
    (ok (has "directory" (message (failure (tool tools "edit" "file_path" "dir"
                                                 "old_string" "a" "new_string" "b")))))
    (ok (equal "abc
" (text-at root "f.txt")))))

(deftest-backends edit-expected-hash
  (with-tools (tools root kind) '(("f.txt" "v1
"))
    (let ((h (result-hash (tool tools "read" "file_path" "f.txt"))))
      (tool tools "write" "file_path" "f.txt" "content" "v2
")
      (let ((c (failure (tool tools "edit" "file_path" "f.txt" "old_string" "v2"
                              "new_string" "v3" "expected_hash" h))))
        (ok (typep c 'stale-file-error))
        (ok (equal h (and c (stale-file-error-expected c))))
        (ok (equal (digest "v2
") (and c (stale-file-error-actual c)))))
      (ok (equal "v2
" (text-at root "f.txt")))
      (let ((out (tool tools "edit" "file_path" "f.txt" "old_string" "v2" "new_string" "v3"
                       "expected_hash" (digest "v2
"))))
        (ok (equal "v3
" (text-at root "f.txt")))
        (ok (equal (digest "v3
") (result-hash out)))))))

(deftest edit-concurrent-write-is-detected
  ;; Another writer lands between the tool's read and its write: the edit's
  ;; CAS write (expected = hash of what it read) must refuse, keeping theirs.
  (let ((fs (make-racing-filesystem)))
    (p:with-filesystem (fs)
      (p:mkdir "/work" :parents t)
      (let ((root (p:ensure-path "/work" :directory t)))
        (make-tree root `(("f.txt" ,(nl "mine"))))
        (let ((tools (make-file-tools :root root)))
          (setf (racing-armed fs) (nl "theirs"))
          (let ((c (failure (tool tools "edit" "file_path" "f.txt"
                                  "old_string" "mine" "new_string" "edited"))))
            (ok (typep c 'stale-file-error))
            (ok (equal (digest (nl "mine")) (and c (stale-file-error-expected c))))
            (ok (equal (digest (nl "theirs")) (and c (stale-file-error-actual c)))))
          (ok (equal (nl "theirs") (text-at root "f.txt")) "the concurrent write is kept")
          (setf (racing-armed fs) (nl "again"))
          (ok (typep (failure (tool tools "multi_edit" "file_path" "f.txt"
                                    "edits" (vector (args "old_string" "theirs"
                                                          "new_string" "x"))))
                     'stale-file-error)
              "multi_edit too")
          (ok (equal (nl "again") (text-at root "f.txt"))))))))

(deftest-backends edit-ignored-file
  (with-tools (tools root kind) '((".gitignore" "*.log
") ("x.log" "a
"))
    (tool tools "edit" "file_path" "x.log" "old_string" "a" "new_string" "b")
    (ok (equal "b
" (text-at root "x.log")) "ignore rules do not apply to an explicit path")))

(deftest edit-read-only-filesystem
  (let* ((tools (make-file-tools :root (make-zip-root '(("a.txt" "x"))))))
    (ok (typep (failure (tool tools "edit" "file_path" "a.txt" "old_string" "x"
                              "new_string" "y"))
               'a:tool-unavailable))))
