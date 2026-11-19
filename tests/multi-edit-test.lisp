(in-package #:agent-tools/tests)

;;; multi_edit: edits apply in order, each to the text the previous one left,
;;; each with edit's rules; all or nothing; one CAS write; one diff.

(defun edits (&rest specs)
  "Vector of edit objects from (old new &optional replace-all) specs."
  (map 'vector (lambda (s)
                 (destructuring-bind (old new &optional (all nil all-p)) s
                   (if all-p
                       (args "old_string" old "new_string" new "replace_all" all)
                       (args "old_string" old "new_string" new))))
       specs))

(deftest-backends multi-edit-in-order
  (let ((old (nl "alpha" "beta" "gamma"))
        (new (nl "ALPHA!" "beta" "delta")))
    (with-tools (tools root kind) `(("f.txt" ,old))
      (let ((out (tool tools "multi_edit" "file_path" "f.txt"
                       "edits" (edits '("alpha" "ALPHA") '("ALPHA" "ALPHA!")
                                      '("gamma" "delta")))))
        (ok (equal new (text-at root "f.txt")) "the second edit sees the first one's output")
        (ok (equal (format nil "Edited f.txt: 3 replacements~%--- a/f.txt~%+++ b/f.txt~%~ahash: ~a"
                           (unified old new) (digest new))
                   out)
            "one diff from the original to the final text")))))

(deftest-backends multi-edit-replace-all-per-edit
  (with-tools (tools root kind) '(("f.txt" "a a a
b
"))
    (let ((out (tool tools "multi_edit" "file_path" "f.txt"
                     "edits" (edits '("a" "c" t) '("b" "d")))))
      (ok (equal "c c c
d
" (text-at root "f.txt")))
      (ok (has "Edited f.txt: 4 replacements" out)))))

(deftest-backends multi-edit-all-or-nothing
  (let ((text (nl "one" "two" "two")))
    (with-tools (tools root kind) `(("f.txt" ,text))
      (testing "a later edit that does not match"
        (let ((c (failure (tool tools "multi_edit" "file_path" "f.txt"
                                "edits" (edits '("one" "ONE") '("three" "THREE"))))))
          (ok (typep c 'edit-match-error))
          (ok (eql 2 (and c (edit-match-error-index c))))
          (ok (eql 0 (and c (edit-match-error-count c))))
          (ok (has "edit 2" (message c)))
          (ok (has "old_string not found in f.txt" (message c)))))
      (testing "an edit that matches twice"
        (let ((c (failure (tool tools "multi_edit" "file_path" "f.txt"
                                "edits" (edits '("one" "ONE") '("two" "2"))))))
          (ok (eql 2 (and c (edit-match-error-index c))))
          (ok (eql 2 (and c (edit-match-error-count c))))
          (ok (has "edit 2" (message c)))
          (ok (has "matched 2 times" (message c)))))
      (testing "an edit that only fails because of an earlier one"
        (let ((c (failure (tool tools "multi_edit" "file_path" "f.txt"
                                "edits" (edits '("one" "ONE") '("one" "x"))))))
          (ok (eql 2 (and c (edit-match-error-index c))))))
      (ok (equal text (text-at root "f.txt")) "nothing written"))))

(deftest-backends multi-edit-argument-errors
  (with-tools (tools root kind) '(("f.txt" "abc
"))
    (flet ((input-error (c &optional needle)
             (and (typep c 'a:tool-input-error)
                  (not (typep c 'edit-match-error))
                  (or (null needle) (has needle (message c))))))
      (ok (input-error (failure (tool tools "multi_edit" "file_path" "f.txt" "edits" #()))
                       "edits")
          "at least one edit")
      (ok (input-error (failure (tool tools "multi_edit" "file_path" "f.txt" "edits" "abc"))
                       "edits"))
      (ok (input-error (failure (tool tools "multi_edit" "file_path" "f.txt")) "edits"))
      (ok (input-error (failure (tool tools "multi_edit" "file_path" "f.txt"
                                      "edits" (vector (args "old_string" "abc"))))
                       "new_string"))
      (ok (input-error (failure (tool tools "multi_edit" "file_path" "f.txt"
                                      "edits" (edits '("abc" "x") '("x" "x"))))
                       "identical"))
      (ok (input-error (failure (tool tools "multi_edit" "file_path" "f.txt"
                                      "edits" (edits '("" "x"))))
                       "old_string")))
    (ok (equal "abc
" (text-at root "f.txt")))))

(deftest-backends multi-edit-expected-hash
  (with-tools (tools root kind) '(("f.txt" "v1
"))
    (ok (typep (failure (tool tools "multi_edit" "file_path" "f.txt"
                              "edits" (edits '("v1" "v2")) "expected_hash" (digest "old")))
               'stale-file-error))
    (ok (equal "v1
" (text-at root "f.txt")))
    (ok (equal (digest "v2
")
               (result-hash (tool tools "multi_edit" "file_path" "f.txt"
                                  "edits" (edits '("v1" "v2"))
                                  "expected_hash" (digest "v1
")))))))
