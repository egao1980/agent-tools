(in-package #:agent-tools/tests)

;;; glob: gitignore glob syntax (* ? [..] **) matched against the whole path
;;; relative to the search directory (as if anchored with a leading "/"), so
;;; "*.lisp" is top level only and "**/*.lisp" any depth. Files only, one
;;; root-relative path per line, sorted (string<). Ignore rules apply, .git/
;;; never appears.

(defparameter *glob-tree*
  '(("a.md" "a") ("b.lisp" "b") ("src/c.lisp" "c") ("src/deep/d.lisp" "d")
    ("src/e.txt" "e") ("src/z.log" "z") ("build/out.lisp" "o") ("x.log" "x")
    (".gitignore" "build/
*.log
")
    (".git/config" "[core]") ("docs/")))

(deftest-backends glob-patterns
  (with-tools (tools root kind) *glob-tree*
    (flet ((glob (&rest kv) (lines (apply #'tool tools "glob" kv))))
      (ok (equal '("b.lisp") (glob "pattern" "*.lisp")) "no slash: top level only")
      (ok (equal '("b.lisp" "src/c.lisp" "src/deep/d.lisp") (glob "pattern" "**/*.lisp"))
          "** spans zero or more directories; build/ is ignored")
      (ok (equal '("src/c.lisp") (glob "pattern" "src/*.lisp")))
      (ok (equal '("src/c.lisp" "src/deep/d.lisp") (glob "pattern" "src/**/*.lisp")))
      (ok (equal '("src/c.lisp" "src/e.txt") (glob "pattern" "src/?.*")))
      (ok (equal '("src/c.lisp" "src/deep/d.lisp") (glob "pattern" "**/[cd].lisp")))
      (ok (equal '(".gitignore" "a.md" "b.lisp") (glob "pattern" "*"))
          "files only; ignored x.log left out; ignore files are listed")
      (ok (notany (lambda (l) (search ".git/" l)) (glob "pattern" "**/*"))
          ".git/ never appears"))))

(deftest-backends glob-under-path
  (with-tools (tools root kind) *glob-tree*
    (flet ((glob (&rest kv) (lines (apply #'tool tools "glob" kv))))
      (ok (equal '("src/c.lisp") (glob "pattern" "*.lisp" "path" "src"))
          "pattern relative to path; output relative to the root")
      (ok (equal '("src/c.lisp" "src/deep/d.lisp") (glob "pattern" "**/*.lisp" "path" "src")))
      (ok (equal '("src/c.lisp" "src/deep/d.lisp" "src/e.txt") (glob "pattern" "**/*" "path" "src"))
          "the root .gitignore applies below the root (src/z.log)")
      (ok (equal '("src/deep/d.lisp") (glob "pattern" "*" "path" "src/deep/"))))))

(deftest-backends glob-ignore-options
  (with-tools (tools root kind :ignore nil) *glob-tree*
    (ok (equal '("b.lisp" "build/out.lisp" "src/c.lisp" "src/deep/d.lisp")
               (lines (tool tools "glob" "pattern" "**/*.lisp")))
        ":ignore nil lists ignored files")
    (ok (notany (lambda (l) (search ".git/" l)) (lines (tool tools "glob" "pattern" "**/*")))
        ".git/ is skipped even with :ignore nil"))
  (with-root (root kind)
    (make-tree root *glob-tree*)
    (let ((tools (make-file-tools :root root :ignore (p:parse-ignore "src/deep/"))))
      (ok (equal '("b.lisp" "build/out.lisp" "src/c.lisp")
                 (lines (tool tools "glob" "pattern" "**/*.lisp")))
          "a matcher replaces the .gitignore files"))))

(deftest-backends glob-nested-ignore-file
  (with-tools (tools root kind) '(("src/.gitignore" "deep/
") ("src/c.lisp" "c") ("src/deep/d.lisp" "d") ("deep/e.lisp" "e"))
    (ok (equal '("deep/e.lisp" "src/c.lisp") (lines (tool tools "glob" "pattern" "**/*.lisp")))
        "src/.gitignore applies under src only")))

(deftest-backends glob-results-and-errors
  (with-tools (tools root kind :max-results 2) *glob-tree*
    (ok (equal '("b.lisp" "src/c.lisp" "[truncated: showing 2 of 3 matches; narrow the pattern or path]")
               (lines (tool tools "glob" "pattern" "**/*.lisp"))))
    (ok (equal "No files matched **/*.rs" (tool tools "glob" "pattern" "**/*.rs")))
    (ok (typep (failure (tool tools "glob")) 'a:tool-input-error) "pattern is required")
    (ok (typep (failure (tool tools "glob" "pattern" "")) 'a:tool-input-error))
    (let ((c (failure (tool tools "glob" "pattern" "*" "path" "nope"))))
      (ok (typep c 'a:tool-input-error))
      (ok (has "not found" (message c))))
    (let ((c (failure (tool tools "glob" "pattern" "*" "path" "a.md"))))
      (ok (typep c 'a:tool-input-error))
      (ok (has "not a directory" (message c))))))
