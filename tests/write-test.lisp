(in-package #:agent-tools/tests)

;;; write: atomic replace through FS-WRITE-ATOMIC, parents created, optional
;;; expected_hash (compare-and-swap). Result: "Wrote <rel> (<n> bytes)" then
;;; "hash: <digest of the new content>".

(deftest-backends write-creates-and-replaces
  (with-tools (tools root kind) '(("old.txt" "before
"))
    (let ((out (tool tools "write" "file_path" "src/deep/new.txt" "content" "héllo
")))
      (ok (equal (list "Wrote src/deep/new.txt (7 bytes)"
                       (concatenate 'string "hash: " (digest "héllo
")))
                 (lines out)))
      (ok (equal "héllo
" (text-at root "src/deep/new.txt")) "missing parents are created"))
    (let ((out (tool tools "write" "file_path" "old.txt" "content" "after")))
      (ok (equal "after" (text-at root "old.txt")) "no expected_hash: blind overwrite")
      (ok (equal (digest "after") (result-hash out))))
    (ok (equal (list "Wrote empty.txt (0 bytes)" (concatenate 'string "hash: " (digest "")))
               (lines (tool tools "write" "file_path" "empty.txt" "content" ""))))
    (ok (equal "" (text-at root "empty.txt")))
    (ok (equal '("empty.txt" "old.txt" "src") (list-names root)) "no temp files left")))

(deftest-backends write-keeps-content-verbatim
  (with-tools (tools root kind) '()
    (let ((text (crlf "a" "b")))
      (tool tools "write" "file_path" "w.txt" "content" text)
      (ok (equalp (octets text) (p:read-bytes (p:join root "w.txt")))
          "no line-ending conversion"))))

(deftest-backends write-expected-hash
  (with-tools (tools root kind) '(("f.txt" "v1
"))
    (let* ((h1 (result-hash (tool tools "read" "file_path" "f.txt")))
           (out (tool tools "write" "file_path" "f.txt" "content" "v2
" "expected_hash" h1)))
      (ok (equal "v2
" (text-at root "f.txt")) "matching hash: written")
      (ok (equal (digest "v2
") (result-hash out)))
      (testing "the returned hash is the token for the next write"
        (tool tools "write" "file_path" "f.txt" "content" "v3
" "expected_hash" (result-hash out))
        (ok (equal "v3
" (text-at root "f.txt"))))
      (testing "a stale hash refuses the write"
        (let ((c (failure (tool tools "write" "file_path" "f.txt" "content" "v4"
                                "expected_hash" h1))))
          (ok (typep c 'stale-file-error))
          (ok (typep c 'a:tool-input-error))
          (ok (equal "f.txt" (and c (stale-file-error-path c))))
          (ok (equal h1 (and c (stale-file-error-expected c))))
          (ok (equal (digest "v3
") (and c (stale-file-error-actual c))))
          (ok (has "f.txt changed since it was read" (message c)))
          (ok (has "read it again" (message c))))
        (ok (equal "v3
" (text-at root "f.txt")) "unchanged")))))

(deftest-backends write-expected-hash-on-missing-file
  (with-tools (tools root kind) '()
    (let ((c (failure (tool tools "write" "file_path" "nope.txt" "content" "x"
                            "expected_hash" (digest "x")))))
      (ok (typep c 'stale-file-error))
      (ok (and c (null (stale-file-error-actual c))) "actual NIL: the file does not exist"))
    (ok (not (exists-at root "nope.txt")))))

(deftest-backends write-stale-restarts
  (with-tools (tools root kind) '(("f.txt" "theirs
"))
    (let ((seen '()))
      (ok (equal (digest "mine
")
                 (handler-bind ((stale-file-error
                                  (lambda (c)
                                    (declare (ignore c))
                                    ;; pathlib's restarts belong to its own
                                    ;; CONTENT-HASH-MISMATCH: look them up by
                                    ;; name, not by this condition.
                                    (when (find-restart 'p:use-current) (push :use-current seen))
                                    (when (find-restart 'p:overwrite) (push :overwrite seen))
                                    (invoke-restart 'p:overwrite))))
                   (result-hash (tool tools "write" "file_path" "f.txt" "content" "mine
"
                                      "expected_hash" (digest "stale")))))
          "OVERWRITE writes anyway and the tool returns success")
      (ok (member :overwrite seen))
      (ok (member :use-current seen) "pathlib's USE-CURRENT is visible too")
      (ok (equal "mine
" (text-at root "f.txt"))))))

(deftest-backends write-errors
  (with-tools (tools root kind) '("dir/")
    (let ((c (failure (tool tools "write" "file_path" "dir" "content" "x"))))
      (ok (typep c 'a:tool-input-error))
      (ok (has "directory" (message c))))
    (ok (has "content" (message (failure (tool tools "write" "file_path" "a.txt")))))
    (ok (typep (failure (tool tools "write" "file_path" "a.txt")) 'a:tool-input-error))
    (ok (typep (failure (tool tools "write" "file_path" "a.txt" "content" 5)) 'a:tool-input-error))
    (ok (typep (failure (tool tools "write" "file_path" "." "content" "x")) 'a:tool-input-error)
        "the root is not a file")
    (ok (not (exists-at root "a.txt")))))

(deftest write-read-only-filesystem
  (let* ((root (make-zip-root '(("a.txt" "x"))))
         (tools (make-file-tools :root root))
         (c (failure (tool tools "write" "file_path" "a.txt" "content" "y"))))
    (ok (typep c 'a:tool-unavailable) "a read-only filesystem makes write unavailable")
    (ok (has "read-only" (message c)))))
