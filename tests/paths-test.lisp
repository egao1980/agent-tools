(in-package #:agent-tools/tests)

;;; Every path argument resolves under the root: relative (POSIX "/"), or
;;; absolute and under the root. ".." is resolved lexically; anything that
;;; ends outside the root is PATH-OUTSIDE-ROOT (a TOOL-INPUT-ERROR).

(deftest-backends resolve-tool-path-inside
  (with-tools (tools root kind) '(("src/a.txt" "x"))
    (multiple-value-bind (path rel) (resolve-tool-path tools "src/a.txt")
      (ok (p:path-p path))
      (ok (p:path-equal (p:join root "src/a.txt") path))
      (ok (equal "src/a.txt" rel)))
    (ok (equal "src/a.txt" (nth-value 1 (resolve-tool-path tools "./src/../src/a.txt")))
        "lexical normalization")
    (ok (equal "src/a.txt" (nth-value 1 (resolve-tool-path tools "src//a.txt"))))
    (ok (equal "src/new/b.txt" (nth-value 1 (resolve-tool-path tools "src/new/b.txt")))
        "paths that do not exist yet resolve")
    (ok (equal "src/a.txt"
               (nth-value 1 (resolve-tool-path
                             tools (concatenate 'string (p:as-posix root) "/src/a.txt"))))
        "an absolute path under the root")
    (ok (equal "." (nth-value 1 (resolve-tool-path tools "."))) "the root itself")
    (ok (equal "." (nth-value 1 (resolve-tool-path tools "src/.."))))
    (ok (equal "." (nth-value 1 (resolve-tool-path tools (p:as-posix root)))))))

(deftest-backends resolve-tool-path-refusals
  (with-tools (tools root kind) '(("src/a.txt" "x"))
    (flet ((outside (arg)
             (let ((c (failure (resolve-tool-path tools arg))))
               (and (typep c 'path-outside-root)
                    (typep c 'a:tool-input-error)
                    (equal arg (path-outside-root-path c))
                    (has arg (message c))
                    (has "outside" (message c))))))
      (ok (outside "../x"))
      (ok (outside "src/../../x"))
      (ok (outside ".."))
      (ok (outside "/etc/passwd"))
      (ok (outside (concatenate 'string (p:as-posix root) "-sibling/x"))
          "a sibling sharing the root's name prefix is outside"))
    (let ((c (failure (resolve-tool-path tools ""))))
      (ok (typep c 'a:tool-input-error) "an empty path is an input error")
      (ok (not (typep c 'path-outside-root))))
    (ok (typep (failure (resolve-tool-path tools "." :allow-root nil)) 'a:tool-input-error))
    (ok (typep (failure (resolve-tool-path tools "src/.." :allow-root nil))
               'a:tool-input-error))))

(deftest-backends tools-refuse-escapes
  (with-root (root kind)
    (make-tree root '(("w/a.txt" "inside") ("above.txt" "above")))
    (let ((tools (make-file-tools :root (p:join root "w/"))))
      (ok (typep (failure (tool tools "read" "file_path" "../above.txt")) 'path-outside-root)
          "even when the target exists")
      (ok (has "inside" (tool tools "read" "file_path" "../w/a.txt"))
          "out and back in is inside: resolution is lexical")
      (ok (typep (failure (tool tools "write" "file_path" "../escaped.txt" "content" "x"))
                 'path-outside-root))
      (ok (not (exists-at root "escaped.txt")) "nothing is written outside")
      (ok (typep (failure (tool tools "edit" "file_path" "../above.txt"
                                "old_string" "above" "new_string" "x"))
                 'path-outside-root))
      (ok (typep (failure (tool tools "multi_edit" "file_path" "../above.txt"
                                "edits" (vector (args "old_string" "above" "new_string" "x"))))
                 'path-outside-root))
      (ok (typep (failure (tool tools "glob" "pattern" "*" "path" "..")) 'path-outside-root))
      (ok (typep (failure (tool tools "grep" "pattern" "inside" "path" "..")) 'path-outside-root))
      (ok (typep (failure (tool tools "ls" "path" "..")) 'path-outside-root))
      (ok (equal "above" (text-at root "above.txt"))))))
