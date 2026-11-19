(in-package #:agent-tools/tests)

;;; Local filesystem only (no symlinks on the Windows local filesystem; this
;;; file is :if-feature (:not (:or :windows :win32))). A symlink inside the
;;; root that points outside it is an escape; one that stays inside is fine.

(deftest symlink-escape-is-refused
  (with-root (outside :local)
    (make-tree outside '(("secret.txt" "s3cret")))
    (with-root (root :local)
      (make-tree root '(("real/a.txt" "inside")))
      (p:create-symlink (p:join root "out") (p:ensure-directory outside))
      (p:create-symlink (p:join root "lnk") (p:ensure-directory (p:join root "real")))
      (let ((tools (make-file-tools :root root)))
        (ok (typep (failure (tool tools "read" "file_path" "out/secret.txt"))
                   'path-outside-root))
        (ok (typep (failure (tool tools "write" "file_path" "out/new.txt" "content" "x"))
                   'path-outside-root))
        (ok (not (exists-at outside "new.txt")) "nothing written through the link")
        (ok (typep (failure (tool tools "edit" "file_path" "out/secret.txt"
                                  "old_string" "s3cret" "new_string" "x"))
                   'path-outside-root))
        (ok (equal "s3cret" (text-at outside "secret.txt")))
        (ok (typep (failure (tool tools "ls" "path" "out")) 'path-outside-root))
        (ok (typep (failure (tool tools "grep" "pattern" "s3cret" "path" "out"))
                   'path-outside-root))
        (ok (not (has "secret.txt" (tool tools "grep" "pattern" "s3cret")))
            "a root-wide grep does not follow the link out")
        (ok (equal '("real/a.txt") (lines (tool tools "glob" "pattern" "**/*")))
            "nor does a root-wide glob, and directory links are not descended")
        (ok (equal '("lnk/" "real/") (lines (tool tools "ls")))
            "ls leaves out a link that resolves outside the root; lnk/ is listed")
        (testing "a link that stays inside the root works"
          (ok (has "inside" (tool tools "read" "file_path" "lnk/a.txt"))))))))
