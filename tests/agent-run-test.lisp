(in-package #:agent-tools/tests)

;;; FILE-TOOLS inside a real agent run (mock LLM backend, libuv loop): the
;;; source is listed in :TOOLS, calls go through INVOKE-TOOL-ASYNC, failures
;;; become tool results via TOOL-FAILURE-TEXT, permission rules match the
;;; "path:" resources, and touched paths drive AGENTS.md discovery.

(defun file-agent (tools steps box &rest keys)
  (apply #'a:make-ai-agent :name "coder" :backend (scripted-backend steps box)
                           :tools (list tools) keys))

(deftest agent-sees-and-finds-the-tools
  (with-tools (tools root :memory) '()
    (let ((agent (file-agent tools '("done") (list nil))))
      (ok (equal +file-tool-names+
                 (mapcar #'llm:llm-tool-name (a:collect-run-tools agent))))
      (ok (eq tools (a:find-agent-tool agent "grep")))
      (ok (eq tools (a:find-agent-tool agent "multi_edit"))))))

(deftest-backends agent-run-write-then-read
  (with-agent-loop
    (with-tools (tools root kind) '()
      (let* ((box (list nil))
             (run (a:run-ai-agent
                   (file-agent tools
                               (list (cons "write" (json-args "file_path" "notes/a.txt"
                                                              "content" (nl "hello")))
                                     (cons "read" (json-args "file_path" "notes/a.txt"))
                                     "done")
                               box)
                   "go"))
             (results (tool-results run)))
        (ok (eq :stop (a:agent-run-finish-reason run)))
        (ok (equal (nl "hello") (text-at root "notes/a.txt")))
        (ok (= 2 (length results)))
        (ok (equal (format nil "Wrote notes/a.txt (6 bytes)~%hash: ~a" (digest (nl "hello")))
                   (result-content (first results))))
        (ok (equal (format nil "~a~%hash: ~a" (numbered 1 "hello") (digest (nl "hello")))
                   (result-content (second results))))
        (ok (notany #'result-error-p results))))))

(deftest agent-run-failure-is-a-tool-result
  (with-agent-loop
    (with-tools (tools root :memory) '(("f.txt" "x x
"))
      (let* ((run (a:run-ai-agent
                   (file-agent tools
                               (list (cons "edit" (json-args "file_path" "f.txt"
                                                             "old_string" "x" "new_string" "y"))
                                     (cons "read" (json-args "file_path" "../outside"))
                                     "done")
                               (list nil))
                   "go"))
             (results (tool-results run)))
        (ok (eq :stop (a:agent-run-finish-reason run)) "failures do not abort the run")
        (ok (= 2 (length results)))
        (ok (every #'result-error-p results))
        (ok (has "Tool edit rejected its arguments: old_string matched 2 times in f.txt"
                 (result-content (first results))))
        (ok (has "replace_all" (result-content (first results))))
        (ok (has "Tool read rejected its arguments" (result-content (second results))))
        (ok (has "outside" (result-content (second results))))
        (ok (equal "x x
" (text-at root "f.txt")))))))

(deftest agent-run-permission-rules-match-paths
  (with-agent-loop
    (with-tools (tools root :memory) '(("secret/k.txt" "key
") ("src/ok.txt" "ok
"))
      (let* ((run (a:run-ai-agent
                   (file-agent tools
                               (list (cons "edit" (json-args "file_path" "secret/k.txt"
                                                             "old_string" "key" "new_string" "x"))
                                     (cons "edit" (json-args "file_path" "./src/ok.txt"
                                                             "old_string" "ok" "new_string" "fine"))
                                     "done")
                               (list nil)
                               :permissions (list (a:make-permission-rule
                                                   "edit" "path:secret/*" :deny
                                                   :feedback "secrets are off limits")))
                   "go"))
             (results (tool-results run)))
        (ok (= 2 (length results)))
        (ok (has "was not run" (result-content (first results))))
        (ok (has "secrets are off limits" (result-content (first results))))
        (ok (equal "key
" (text-at root "secret/k.txt")) "denied: not run")
        (ok (not (result-error-p (second results))))
        (ok (equal "fine
" (text-at root "src/ok.txt")) "allowed")))))

(defun instruction-turns (turns)
  (remove-if-not (lambda (turn)
                   (and (eq :user (llm:llm-turn-role turn))
                        (search "<instructions " (llm:turn-text turn))))
                 turns))

(deftest agent-run-discovers-instructions-from-touched-paths
  (with-agent-loop
    (with-tools (tools root :local) '(("AGENTS.md" "ROOT-RULES") ("src/AGENTS.md" "SRC-RULES")
                                      ("src/a.lisp" "(a)"))
      (let* ((box (list nil))
             (run (a:run-ai-agent
                   (file-agent tools
                               (list (cons "read" (json-args "file_path" "src/a.lisp")) "done")
                               box
                               :settings (a:make-agent-settings
                                          :extra (list :instructions-discovery
                                                       (p:path-pathname root))))
                   "go")))
        (ok (eq :stop (a:agent-run-finish-reason run)))
        (let ((instr (instruction-turns (second (car box)))))
          (ok (= 1 (length instr)) "injected before the second generate")
          (ok (and instr (search "ROOT-RULES" (llm:turn-text (first instr)))))
          (ok (and instr (search "SRC-RULES" (llm:turn-text (first instr))))))))))

(deftest agent-run-two-edits-of-one-file-in-one-step
  ;; cl-stack-code#30: the tool calls of one step run at once. Two edits of
  ;; one file, both reading before either writes (the rendezvous), must both
  ;; land: each applies to what the other wrote.
  (let ((fs (make-rendezvous-filesystem)))
    (p:with-filesystem (fs)
      (p:mkdir "/work" :parents t)
      (let* ((root (make-tree (p:ensure-path "/work" :directory t)
                              `(("f.txt" ,(nl "alpha" "middle" "omega")))))
             (tools (make-file-tools :root root))
             (calls 0)
             (backend
               (llm:make-mock-llm-backend
                :handler (lambda (b turns &key &allow-other-keys)
                           (declare (ignore b turns))
                           (if (= 1 (incf calls))
                               (llm:make-llm-response
                                :parts (list (llm:make-llm-tool-call-part
                                              :id "c1" :name "edit"
                                              :arguments (json-args "file_path" "f.txt"
                                                                    "old_string" "alpha"
                                                                    "new_string" "ALPHA"))
                                             (llm:make-llm-tool-call-part
                                              :id "c2" :name "edit"
                                              :arguments (json-args "file_path" "f.txt"
                                                                    "old_string" "omega"
                                                                    "new_string" "OMEGA")))
                                :finish-reason :tool-use)
                               (llm:make-llm-response
                                :parts (list (llm:make-llm-text-part :text "done"))
                                :finish-reason :stop))))))
        (setf (rendezvous-gate fs) 2)
        (with-agent-loop
          (let* ((run (a:run-ai-agent (a:make-ai-agent :name "coder" :backend backend
                                                       :tools (list tools))
                                      "go"))
                 (results (tool-results run))
                 (final (nl "ALPHA" "middle" "OMEGA")))
            (ok (eq :stop (a:agent-run-finish-reason run)))
            (ok (= 2 (length results)))
            (ok (notany #'result-error-p results) "both edits succeed")
            (ok (equal final (text-at root "f.txt")) "both changes are on disk")
            (ok (member (digest final) (mapcar (lambda (r) (result-hash (result-content r)))
                                               results)
                        :test #'equal)
                "the later edit reports the file's final hash")))))))
