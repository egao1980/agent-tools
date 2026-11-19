(in-package #:agent-tools/tests)

;;; MAKE-FILE-TOOLS: root, filesystem, defaults, tool selection.

(deftest-backends make-file-tools-defaults
  (with-tools (tools root kind) '(("a.txt" "x"))
    (ok (file-tools-p tools))
    (ok (p:path-p (file-tools-root tools)))
    (ok (p:path-equal root (file-tools-root tools)) "root is the directory given")
    (ok (eq p:*filesystem* (file-tools-filesystem tools))
        "filesystem defaults to the root path's")
    (ok (eq :auto (file-tools-ignore tools)))
    (ok (equal +file-tool-names+ (file-tools-names tools)))
    (ok (equal '("read" "write" "edit" "multi_edit" "glob" "grep" "ls")
               (file-tools-names tools)))
    (ok (= 2000 (file-tools-max-read-lines tools)))
    (ok (= 50000 (file-tools-max-output tools)))
    (ok (= 100 (file-tools-max-results tools)))))

(deftest-backends make-file-tools-root-designators
  (with-root (root kind)
    (make-tree root '(("a.txt" "x")))
    (testing "a POSIX string on an explicit filesystem"
      (let ((tools (make-file-tools :root (p:as-posix root)
                                    :filesystem (p:path-filesystem root))))
        (ok (p:path-equal root (file-tools-root tools)))
        (ok (eq (p:path-filesystem root) (file-tools-filesystem tools)))))
    (testing "a string without :filesystem uses *filesystem*"
      (let ((tools (make-file-tools :root (p:as-posix root))))
        (ok (eq p:*filesystem* (file-tools-filesystem tools)))
        (ok (has "x" (tool tools "read" "file_path" "a.txt")))))
    (testing "limits are kept"
      (let ((tools (make-file-tools :root root :max-read-lines 7 :max-output 300
                                    :max-results 4 :ignore nil)))
        (ok (= 7 (file-tools-max-read-lines tools)))
        (ok (= 300 (file-tools-max-output tools)))
        (ok (= 4 (file-tools-max-results tools)))
        (ok (null (file-tools-ignore tools)))))))

(deftest-backends make-file-tools-bad-root
  (with-root (root kind)
    (make-tree root '(("f.txt" "x")))
    (ok (signals (make-file-tools :root (p:join root "missing/")) 'p:path-not-found))
    (ok (signals (make-file-tools :root (p:join root "f.txt")) 'p:path-error)
        "a file is not a root")
    (ok (signals (make-file-tools :root root :tools '("read" "bash")) 'error)
        "unknown tool names are refused")))

(deftest-backends make-file-tools-subset
  (with-tools (tools root kind :tools '("grep" "read")) '(("a.txt" "x"))
    (ok (equal '("read" "grep") (file-tools-names tools))
        "canonical order whatever order was given")
    (ok (equal '("read" "grep")
               (mapcar #'llm:llm-tool-name (a:list-agent-tools tools))))
    (ok (a:tool-executable-p tools "read"))
    (ok (not (a:tool-executable-p tools "write")))
    (ok (not (a:tool-executable-p tools "bash")))
    (ok (typep (failure (tool tools "write" "file_path" "b.txt" "content" "y"))
               'a:agent-unknown-tool)
        "a disabled tool is unknown")
    (ok (not (exists-at root "b.txt")))))
