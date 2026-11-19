(in-package #:agent-tools/tests)

;;; grep: literal substring by default, one "<rel>:<line>:<text>" line per
;;; match, sorted by path then line. Binary files are skipped; ignore rules
;;; apply (relative to the root); include filters files (.gitignore syntax,
;;; "*.lisp" at any depth, as cl-stack-pathlib's FS-GREP).

(defparameter *grep-tree*
  `(("a.lisp" ,(format nil "(defun foo ()~%  (bar))~%; Foo here~%"))
    ("src/b.lisp" ,(format nil "foo~%nothing~%"))
    ("src/c.txt" ,(format nil "foo txt~%"))
    ("src/gen.log" ,(format nil "foo log~%"))
    ("build/x.lisp" ,(format nil "foo built~%"))
    (".gitignore" ,(format nil "build/~%*.log~%"))
    ("bin.dat" ,(coerce #(102 111 111 0 10) '(vector (unsigned-byte 8))))))

(deftest-backends grep-literal
  (with-tools (tools root kind) *grep-tree*
    (flet ((grep (&rest kv) (lines (apply #'tool tools "grep" kv))))
      (ok (equal '("a.lisp:1:(defun foo ()" "src/b.lisp:1:foo" "src/c.txt:1:foo txt")
                 (grep "pattern" "foo"))
          "sorted; build/ and *.log ignored; bin.dat skipped")
      (ok (equal '("a.lisp:1:(defun foo ()" "a.lisp:3:; Foo here" "src/b.lisp:1:foo"
                   "src/c.txt:1:foo txt")
                 (grep "pattern" "FOO" "case_insensitive" t)))
      (ok (equal '("a.lisp:2:  (bar))") (grep "pattern" "(bar)"))
          "regex metacharacters are literal")
      (ok (equal '("a.lisp:2:  (bar))") (grep "pattern" "(bar)" "regex" nil))))))

(deftest-backends grep-path-and-include
  (with-tools (tools root kind) *grep-tree*
    (flet ((grep (&rest kv) (lines (apply #'tool tools "grep" kv))))
      (ok (equal '("src/b.lisp:1:foo" "src/c.txt:1:foo txt") (grep "pattern" "foo" "path" "src"))
          "paths stay root-relative; the root .gitignore applies under src")
      (ok (equal '("a.lisp:1:(defun foo ()") (grep "pattern" "foo" "path" "a.lisp"))
          "a single file")
      (ok (equal '("a.lisp:1:(defun foo ()" "src/b.lisp:1:foo")
                 (grep "pattern" "foo" "include" "*.lisp"))
          "include *.lisp at any depth")
      (ok (equal '("src/b.lisp:1:foo") (grep "pattern" "foo" "path" "src" "include" "*.lisp"))))))

(deftest-backends grep-ignore-option
  (with-tools (tools root kind :ignore nil) *grep-tree*
    (ok (equal '("a.lisp:1:(defun foo ()" "build/x.lisp:1:foo built" "src/b.lisp:1:foo"
                 "src/c.txt:1:foo txt" "src/gen.log:1:foo log")
               (lines (tool tools "grep" "pattern" "foo"))))))

(deftest-backends grep-limits
  (with-tools (tools root kind) *grep-tree*
    (ok (equal '("a.lisp:1:(defun foo ()" "[truncated at 1 match; narrow the pattern or path]")
               (lines (tool tools "grep" "pattern" "foo" "max_results" 1))))
    (ok (equal '("a.lisp:1:(defun foo ()" "src/b.lisp:1:foo"
                 "[truncated at 2 matches; narrow the pattern or path]")
               (lines (tool tools "grep" "pattern" "foo" "max_results" 2))))
    (ok (equal 3 (length (lines (tool tools "grep" "pattern" "foo" "max_results" 3))))
        "exactly max_results matches: no notice"))
  (with-tools (tools root kind :max-results 2) *grep-tree*
    (ok (equal "[truncated at 2 matches; narrow the pattern or path]"
               (last-line (tool tools "grep" "pattern" "foo" "max_results" 50)))
        "max_results is capped at the tools' max-results"))
  (with-tools (tools root kind :max-output 40) *grep-tree*
    (let* ((out (tool tools "grep" "pattern" "foo"))
           (body (butlast (lines out))))
      (ok (has "[truncated: output over 40 characters; narrow the pattern or path]"
               (last-line out)))
      (ok (<= (reduce #'+ body :key (lambda (l) (1+ (length l)))) 40)
          "whole lines up to max-output, then the notice"))))

(deftest-backends grep-no-match-and-errors
  (with-tools (tools root kind) *grep-tree*
    (ok (equal "No matches for nothing-here" (tool tools "grep" "pattern" "nothing-here")))
    (let ((c (failure (tool tools "grep" "pattern" "fo+" "regex" t))))
      (ok (typep c 'a:tool-input-error) "no regex engine is loaded")
      (ok (has "regex" (message c))))
    (ok (typep (failure (tool tools "grep" "pattern" "")) 'a:tool-input-error))
    (ok (typep (failure (tool tools "grep")) 'a:tool-input-error))
    (ok (typep (failure (tool tools "grep" "pattern" "foo" "max_results" 0)) 'a:tool-input-error))
    (ok (has "not found" (message (failure (tool tools "grep" "pattern" "foo" "path" "nope")))))))
