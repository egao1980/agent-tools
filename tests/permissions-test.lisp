(in-package #:agent-tools/tests)

;;; TOOL-PERMISSION-RESOURCES → ("path:<root-relative POSIX path>") for the
;;; file or directory a call touches ("path:." for the root); never signals.
;;; TOOL-INSTRUCTION-PATHS → ("<root-relative path>") for AGENTS.md discovery,
;;; NIL when the path does not resolve under the root.

(defun resources (tools name &rest kv)
  (a:tool-permission-resources tools name (apply #'json-args kv)))

(defun instruction-paths (tools name &rest kv)
  (a:tool-instruction-paths tools name (apply #'json-args kv)))

(deftest-backends permission-resources-per-tool
  (with-tools (tools root kind) '(("src/a.lisp" "(a)"))
    (ok (equal '("path:src/a.lisp") (resources tools "read" "file_path" "src/a.lisp")))
    (ok (equal '("path:src/a.lisp") (resources tools "read" "file_path" "./src/../src/a.lisp"))
        "normalized")
    (ok (equal '("path:src/a.lisp")
               (resources tools "read" "file_path"
                          (concatenate 'string (p:as-posix root) "/src/a.lisp")))
        "absolute under the root → relative")
    (ok (equal '("path:new/b.txt") (resources tools "write" "file_path" "new/b.txt" "content" "x"))
        "a file that does not exist yet")
    (ok (equal '("path:src/a.lisp")
               (resources tools "edit" "file_path" "src/a.lisp" "old_string" "a" "new_string" "b")))
    (ok (equal '("path:src/a.lisp")
               (resources tools "multi_edit" "file_path" "src/a.lisp"
                          "edits" (vector (args "old_string" "a" "new_string" "b")))))
    (ok (equal '("path:.") (resources tools "glob" "pattern" "**/*.lisp")) "default path: the root")
    (ok (equal '("path:src") (resources tools "glob" "pattern" "*" "path" "src/")))
    (ok (equal '("path:.") (resources tools "grep" "pattern" "x")))
    (ok (equal '("path:src/a.lisp") (resources tools "grep" "pattern" "x" "path" "src/a.lisp")))
    (ok (equal '("path:.") (resources tools "ls")))
    (ok (equal '("path:src") (resources tools "ls" "path" "src")))
    (ok (equal '("path:src/a.lisp")
               (a:tool-permission-resources tools "read" (args "file_path" "src/a.lisp")))
        "hash-table arguments")))

(deftest-backends permission-resources-never-signal
  (with-tools (tools root kind) '()
    (ok (equal '("path:../etc/passwd") (resources tools "read" "file_path" "../etc/passwd"))
        "an escaping path is reported as given (the tool still refuses it)")
    (ok (equal '("path:/elsewhere/x") (resources tools "write" "file_path" "/elsewhere/x"
                                                 "content" "x")))
    (ok (equal '("*") (a:tool-permission-resources tools "read" "{not json"))
        "unparsable arguments: the protocol's catch-all")
    (ok (equal '("*") (resources tools "read")) "missing file_path")
    (ok (equal '("*") (resources tools "read" "file_path" 7)))))

(deftest-backends instruction-paths-per-tool
  (with-tools (tools root kind) '(("src/a.lisp" "(a)"))
    (ok (equal '("src/a.lisp") (instruction-paths tools "read" "file_path" "src/a.lisp")))
    (ok (equal '("src/a.lisp") (instruction-paths tools "edit" "file_path" "./src/a.lisp"
                                                  "old_string" "a" "new_string" "b")))
    (ok (equal '("new/b.txt") (instruction-paths tools "write" "file_path" "new/b.txt"
                                                 "content" "x")))
    (ok (equal '(".") (instruction-paths tools "ls")))
    (ok (equal '("src") (instruction-paths tools "grep" "pattern" "x" "path" "src")))
    (ok (equal '("src") (instruction-paths tools "glob" "pattern" "*" "path" "src")))
    (ok (null (instruction-paths tools "read" "file_path" "../x")) "outside the root: none")
    (ok (null (a:tool-instruction-paths tools "read" "{not json")))))
