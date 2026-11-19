(in-package #:agent-tools/tests)

;;; ls: the entries directly in a directory, one name per line, sorted
;;; (string< on the name), directories with a trailing "/". Ignored entries
;;; and .git/ are left out.

(defparameter *ls-tree*
  '(("b.txt" "b") ("a.txt" "a") ("src/x.lisp" "x") "docs/" (".gitignore" "*.log
build/
")
    ("x.log" "l") ("build/o" "o") (".git/HEAD" "ref")))

(deftest-backends ls-listing
  (with-tools (tools root kind) *ls-tree*
    (ok (equal '(".gitignore" "a.txt" "b.txt" "docs/" "src/") (lines (tool tools "ls"))))
    (ok (equal (lines (tool tools "ls")) (lines (tool tools "ls" "path" "."))))
    (ok (equal '("x.lisp") (lines (tool tools "ls" "path" "src"))))
    (ok (equal "[empty directory]" (tool tools "ls" "path" "docs")))))

(deftest-backends ls-ignore-option
  (with-tools (tools root kind :ignore nil) *ls-tree*
    (ok (equal '(".gitignore" "a.txt" "b.txt" "build/" "docs/" "src/" "x.log")
               (lines (tool tools "ls")))
        ".git/ is left out even with :ignore nil")))

(deftest-backends ls-errors
  (with-tools (tools root kind) *ls-tree*
    (let ((c (failure (tool tools "ls" "path" "a.txt"))))
      (ok (typep c 'a:tool-input-error))
      (ok (has "not a directory" (message c)))
      (ok (has "read" (message c)) "points at read"))
    (let ((c (failure (tool tools "ls" "path" "nope"))))
      (ok (typep c 'a:tool-input-error))
      (ok (has "not found" (message c))))))
