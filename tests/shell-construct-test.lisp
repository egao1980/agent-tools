(in-package #:agent-tools/tests)

;;; MAKE-SHELL-TOOLS, SHELL-DIALECT, DEFAULT-SHELL, CLOSE-SHELL-TOOLS and the
;;; LIST-AGENT-TOOLS descriptors of a SHELL-TOOLS source.

(deftest make-shell-tools-defaults
  (with-shell (tools root :auto) '()
    (ok (shell-tools-p tools))
    (ok (not (file-tools-p tools)))
    (ok (p:path-p (shell-tools-root tools)))
    (ok (p:path-equal root (shell-tools-root tools)) "root is the directory given")
    (ok (eq #+(or windows win32) :process #-(or windows win32) :pty
            (shell-tools-executor tools))
        ":auto is :pty on Unix, :process on Windows")
    (ok (equal (default-shell) (shell-tools-shell tools)))
    (ok (eq #+(or windows win32) :cmd #-(or windows win32) :posix
            (shell-tools-dialect tools)))
    (ok (null (shell-tools-env tools)))
    (ok (shell-tools-needs-approval tools))
    (ok (= 120000 (shell-tools-default-timeout tools)))
    (ok (= 600000 (shell-tools-max-timeout tools)))
    (ok (= 30000 (shell-tools-max-output tools)))
    (ok (= 8 (shell-tools-max-jobs tools)))
    (ok (= 1000000 (shell-tools-max-job-buffer tools)))
    (ok (equal +shell-tool-names+ (shell-tools-names tools)))
    (ok (equal '("bash" "job_output" "job_input" "job_kill" "job_list")
               (shell-tools-names tools)))
    (ok (null (shell-tools-jobs tools)))
    (ok (not (shell-tools-closed-p tools)))))

(deftest default-shell-per-os
  (ok (equal #+(or windows win32) '("cmd.exe" "/d" "/s" "/c")
             #-(or windows win32) '("/bin/sh" "-c")
             (default-shell))))

(deftest shell-environment-defaults
  (ok (equal '(("TERM" . "dumb") ("PAGER" . "cat") ("GIT_PAGER" . "cat")
               ("NO_COLOR" . "1") ("GIT_TERMINAL_PROMPT" . "0"))
             +shell-environment+)))

(deftest shell-dialect-from-argv
  (dolist (c '((("/bin/sh" "-c") :posix)
               (("bash" "-lc") :posix)
               (("/usr/local/bin/zsh" "-c") :posix)
               (("dash" "-c") :posix)
               (("/bin/ksh" "-c") :posix)
               (("C:\\Program Files\\Git\\bin\\bash.exe" "-c") :posix)
               (("cmd.exe" "/d" "/s" "/c") :cmd)
               (("C:\\Windows\\System32\\CMD.EXE" "/c") :cmd)
               (("cmd") :cmd)
               (("powershell.exe" "-Command") :opaque)
               (("pwsh" "-c") :opaque)
               (("fish" "-c") :opaque)
               (("/usr/bin/python3" "-c") :opaque)))
    (ok (eq (second c) (handler-case (shell-dialect (first c)) (error () :error)))
        (format nil "~s" (first c)))))

(deftest make-shell-tools-root-designators
  (with-root (root :local)
    (dolist (designator (list root (p:path-pathname root) (p:as-posix root)
                              (namestring (p:path-pathname root))))
      (let ((tools (make-shell-tools :root designator)))
        (unwind-protect
             (ok (p:path-equal root (shell-tools-root tools)) (format nil "~s" designator))
          (close-shell-tools tools))))))

(deftest make-shell-tools-options
  (with-shell (tools root :process
                     :shell '("/bin/bash" "-c") :env '(("A" . "1") ("B" . nil))
                     :needs-approval nil :default-timeout 5000 :max-timeout 9000
                     :max-output 300 :max-jobs 2 :max-job-buffer 4000
                     :tools '("job_kill" "bash"))
      '()
    (ok (eq :process (shell-tools-executor tools)))
    (ok (equal '("/bin/bash" "-c") (shell-tools-shell tools)))
    (ok (eq :posix (shell-tools-dialect tools)) "dialect follows the shell")
    (ok (equal '(("A" . "1") ("B" . nil)) (shell-tools-env tools)))
    (ok (null (shell-tools-needs-approval tools)))
    (ok (= 5000 (shell-tools-default-timeout tools)))
    (ok (= 9000 (shell-tools-max-timeout tools)))
    (ok (= 300 (shell-tools-max-output tools)))
    (ok (= 2 (shell-tools-max-jobs tools)))
    (ok (= 4000 (shell-tools-max-job-buffer tools)))
    (ok (equal '("bash" "job_kill") (shell-tools-names tools)) "canonical order")
    (ok (equal '("bash" "job_kill") (mapcar #'llm:llm-tool-name (a:list-agent-tools tools))))
    (ok (typep (failure (shell tools "job_list")) 'a:agent-unknown-tool)))
  (with-shell (tools root :process :shell '("pwsh" "-Command")) '()
    (ok (eq :opaque (shell-tools-dialect tools))))
  (with-shell (tools root :process :shell '("/bin/sh" "-c") :dialect :cmd) '()
    (ok (eq :cmd (shell-tools-dialect tools)) "an explicit dialect wins")))

(deftest make-shell-tools-bad-arguments
  (with-root (root :local)
    (make-tree root '(("f.txt" "x")))
    (ok (typep (failure (make-shell-tools :root (p:join root "missing/")))
               'p:path-not-found))
    (let ((c (failure (make-shell-tools :root (p:join root "f.txt")))))
      (ok (typep c 'p:path-error))
      (ok (not (typep c 'p:path-not-found))))
    (let ((memory (p:make-memory-filesystem)))
      (p:mkdir (p:ensure-path "/work" :directory t :filesystem memory) :parents t)
      (ok (real-failure (failure (make-shell-tools
                                  :root (p:ensure-path "/work" :directory t
                                                               :filesystem memory))))
          "a root on a non-local filesystem"))
    (ok (real-failure (failure (make-shell-tools :root root :tools '("bash" "sudo")))))
    (ok (real-failure (failure (make-shell-tools :root root :executor :docker))))
    (ok (real-failure (failure (make-shell-tools :root root :dialect :fish))))
    (ok (real-failure (failure (make-shell-tools :root root :default-timeout 700000))))
    #+(or windows win32)
    (ok (real-failure (failure (make-shell-tools :root root :executor :pty))))))

(deftest close-shell-tools-refuses-later-calls
  (with-shell (tools root :auto) '()
    (ok (null (close-shell-tools tools)))
    (ok (shell-tools-closed-p tools))
    (ok (null (close-shell-tools tools)) "idempotent")
    (let ((c (failure (bash tools (pick "echo hi" "echo hi")))))
      (ok (typep c 'a:tool-unavailable))
      (ok (has "closed" (message c))))))

(deftest unknown-shell-tool-name
  (with-shell (tools root :auto) '()
    (let ((c (failure (shell tools "zsh"))))
      (ok (typep c 'a:agent-unknown-tool))
      (ok (not (typep c 'a:tool-failure))))))

;;; --- descriptors -----------------------------------------------------------

(deftest shell-list-agent-tools
  (with-shell (tools root :auto) '()
    (let ((descs (a:list-agent-tools tools)))
      (ok (= 5 (length descs)))
      (ok (every #'llm:llm-tool-p descs))
      (ok (equal +shell-tool-names+ (mapcar #'llm:llm-tool-name descs)))
      (ok (every (lambda (d)
                   (let ((s (llm:llm-tool-description d)))
                     (and (stringp s) (> (length s) 20))))
                 descs))
      (ok (not (eq (first descs) (first (a:list-agent-tools tools)))) "fresh per call")
      (let ((bash (llm:llm-tool-description (first descs))))
        (ok (has (format nil "~{~a~^ ~}" (default-shell)) bash) "names the shell")
        (ok (has "run_in_background" bash))
        (ok (has "job_output" bash))))))

(deftest-parametrize shell-tool-parameter-schemas
    ((name required props)
     :ids ("bash" "job_output" "job_input" "job_kill" "job_list")
     ("bash" '("command")
      '(("command" "string") ("timeout_ms" "integer") ("workdir" "string")
        ("run_in_background" "boolean") ("description" "string")))
     ("job_output" '("job_id")
      '(("job_id" "string") ("wait_ms" "integer")))
     ("job_input" '("job_id" "input")
      '(("job_id" "string") ("input" "string") ("wait_ms" "integer")))
     ("job_kill" '("job_id")
      '(("job_id" "string")))
     ("job_list" '()
      '()))
  (with-shell (tools root :auto) '()
    (let* ((desc (tool-named tools name))
           (params (and desc (llm:llm-tool-parameters desc))))
      (ok (hash-table-p params))
      (when (hash-table-p params)
        (let ((properties (gethash "properties" params)))
          (ok (equal "object" (gethash "type" params)))
          (ok (hash-table-p properties))
          (ok (equal (sort (mapcar #'first props) #'string<) (hash-keys properties)))
          (dolist (prop props)
            (let ((schema (gethash (first prop) properties)))
              (ok (and (hash-table-p schema) (equal (second prop) (gethash "type" schema)))
                  (first prop))
              (ok (and (hash-table-p schema) (stringp (gethash "description" schema)))
                  (format nil "~a has a description" (first prop)))))
          (multiple-value-bind (v present) (gethash "additionalProperties" params)
            (ok (and present (null v))))
          (ok (null (nth-value 1 (gethash "$schema" params))))
          (if required
              (ok (equalp (coerce required 'vector) (gethash "required" params)))
              (ok (null (nth-value 1 (gethash "required" params)))))
          (ok (stringp (json:encode params)) "JSON-encodable"))))))

(deftest shell-schema-bounds-follow-the-limits
  (with-shell (tools root :auto :max-timeout 9000 :default-timeout 1000) '()
    (flet ((prop (tool key)
             (gethash key (gethash "properties"
                                   (llm:llm-tool-parameters (tool-named tools tool))))))
      (ok (eql 1 (gethash "minimum" (prop "bash" "timeout_ms"))))
      (ok (eql 9000 (gethash "maximum" (prop "bash" "timeout_ms"))))
      (ok (eql 0 (gethash "minimum" (prop "job_output" "wait_ms"))))
      (ok (eql 9000 (gethash "maximum" (prop "job_output" "wait_ms"))))
      (ok (eql 9000 (gethash "maximum" (prop "job_input" "wait_ms")))))))
