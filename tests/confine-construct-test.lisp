(in-package #:agent-tools/tests)

;;; R4d-4a: MAKE-SHELL-TOOLS :CONFINEMENT / :COMPUTE-BACKEND, the readers, the
;;; bash description of a confined source (CONFINEMENT-DESCRIPTION) and the
;;; denial note (CONFINEMENT-DENIAL-NOTE). Pure or construction only: every OS.

;;; --- construction --------------------------------------------------------------

(deftest confined-construction
  (with-root (root :local)
    (let* ((c (area-confinement root))
           (b (sc:make-model-confinement-backend))
           (tools (make-shell-tools :root root :confinement c :compute-backend b)))
      (unwind-protect
           (progn
             (ok (eq c (shell-tools-confinement tools)))
             (ok (eq b (shell-tools-compute-backend tools)))
             (ok (shell-tools-confined-p tools))
             (ok (member (shell-tools-executor tools) '(:pty :process))
                 ":auto still picks the OS executor"))
        (close-shell-tools tools)))
    (let ((tools (make-shell-tools :root root)))
      (unwind-protect
           (progn
             (ok (null (shell-tools-confinement tools)) "no confinement by default")
             (ok (null (shell-tools-compute-backend tools)))
             (ok (not (shell-tools-confined-p tools))))
        (close-shell-tools tools)))))

(deftest confined-construction-does-not-check
  (with-root (root :local)
    (dolist (b (list (make-instance 'unavailable-backend)
                     (sc:make-model-confinement-backend :features '(:filesystem))))
      (let ((tools (make-shell-tools :root root :confinement (area-confinement root)
                                     :compute-backend b)))
        (ok (shell-tools-confined-p tools)
            (format nil "~a: refusals are per command, not at construction"
                    (class-name (class-of b))))
        (close-shell-tools tools)))))

(deftest-parametrize confined-construction-errors
    ((label make-keys type)
     :ids ("confinement-without-backend" "backend-without-confinement"
           "confinement-not-a-confinement" "backend-not-confining" "backend-not-a-backend")
     ("confinement without backend"
      (lambda (root) (list :confinement (area-confinement root))) 'error)
     ("backend without confinement"
      (lambda (root) (declare (ignore root))
        (list :compute-backend (sc:make-model-confinement-backend))) 'error)
     ("confinement of the wrong type"
      (lambda (root) (list :confinement (native-root root)
                           :compute-backend (sc:make-model-confinement-backend)))
      'type-error)
     ("a non-confining compute backend"
      (lambda (root) (list :confinement (area-confinement root)
                           :compute-backend (sc:make-native-process-backend)))
      'type-error)
     ("backend of the wrong type"
      (lambda (root) (list :confinement (area-confinement root) :compute-backend :seatbelt))
      'type-error))
  (with-root (root :local)
    (let ((c (failure (apply #'make-shell-tools :root root (funcall make-keys root)))))
      (ok (typep c type) label)
      (ok (not (typep c 'a:tool-failure)) "a construction error, not a tool failure"))))

(deftest confined-construction-with-supervisor
  (call-with-state-dir
   (lambda (state)
     (let ((sup (sup:make-supervisor :exit-hook nil :state-directory state)))
       (unwind-protect
            (with-root (root :local)
              (let ((tools (make-shell-tools :root root :supervisor sup
                                             :confinement (area-confinement root)
                                             :compute-backend (sc:make-model-confinement-backend))))
                (ok (shell-tools-confined-p tools))
                (ok (eq :process (shell-tools-executor tools)))
                (ok (null (sup:supervisor-compute-executor sup))
                    "the supervisor needs no compute executor of its own")
                (close-shell-tools tools))
              (ok (failure (make-shell-tools :root root :supervisor sup :executor :pty
                                             :confinement (area-confinement root)
                                             :compute-backend (sc:make-model-confinement-backend)))
                  ":pty under a supervisor stays an error"))
         (quiet-shutdown sup))))))

;;; --- the bash description ------------------------------------------------------

(defun bash-description (tools)
  (llm:llm-tool-description
   (find "bash" (a:list-agent-tools tools) :key #'llm:llm-tool-name :test #'string=)))

(deftest confined-bash-description
  (with-root (root :local)
    (let ((plain (make-shell-tools :root root))
          (tools (make-shell-tools :root root :confinement (area-confinement root)
                                   :compute-backend (sc:make-model-confinement-backend))))
      (unwind-protect
           (let ((d (bash-description tools))
                 (base (bash-description plain)))
             (ok (has base d) "the unconfined text comes first")
             (ok (has "Sandbox:" d))
             (ok (has (native-root root) d) "names the writable root")
             (ok (has "TMPDIR" d))
             (ok (has "network access is off" d))
             (ok (has "unreadable" d) "credential locations")
             (ok (has ".ssh" d))
             (ok (not (has "Sandbox:" base)) "unconfined: no sandbox paragraph")
             (dolist (other (remove "bash" (a:list-agent-tools tools)
                                    :key #'llm:llm-tool-name :test #'string=))
               (ok (not (has "Sandbox:" (llm:llm-tool-description other)))
                   (format nil "~a has no sandbox paragraph" (llm:llm-tool-name other)))))
        (close-shell-tools plain)
        (close-shell-tools tools)))))

(deftest-parametrize confinement-description-policies
    ((label keys present absent)
     :ids ("network-allow" "network-policy" "read-system" "no-deny-read" "two-roots")
     ("network :allow" '(:network :allow) '("network access is allowed") '("network access is off"))
     ("a port policy"
      (list :network (sc:make-sandbox-network-policy
                      :egress (list (sc:make-egress-rule :host "*" :port 443))))
      '("network access is limited") '("network access is off"))
     ("read :system" '(:read :system) '("reads are limited to") '())
     ("no deny-read" '(:deny-read ()) '() '("unreadable"))
     ("two writable roots" :two-roots '() '()))
  (with-root (root :local)
    (let* ((extra (uiop:ensure-directory-pathname
                   (merge-pathnames "second/" (p:path-pathname root))))
           (c (progn
                (ensure-directories-exist extra)
                (if (eq keys :two-roots)
                    (sc:make-confinement :writable (list (native-root root)
                                                         (uiop:native-namestring extra)))
                    (apply #'area-confinement root keys))))
           (d (confinement-description c)))
      (ok (has "Sandbox:" d) label)
      (ok (has (native-root root) d))
      (when (eq keys :two-roots)
        (ok (has (uiop:native-namestring extra) d) "every writable root"))
      (dolist (s present) (ok (has s d) (format nil "~a: says ~s" label s)))
      (dolist (s absent) (ok (not (has s d)) (format nil "~a: does not say ~s" label s))))))

;;; --- the denial note (pure) ------------------------------------------------------

(defun deny-read-entry (c)
  "The native namestring of C's first deny-read entry, without a final /."
  (string-right-trim "/\\" (uiop:native-namestring (first (sc:confinement-deny-read c)))))

(deftest-parametrize confinement-denial-note-cases
    ((label output code reasons)
     :ids ("write-eperm" "write-eacces" "read-only-fs" "sandbox-word" "network-resolve"
           "network-connect" "network-socket-eperm" "deny-read" "write-and-network"
           "normal-failure" "success-with-denial-text" "signal" "empty")
     ("write: Operation not permitted"
      "touch: /etc/x: Operation not permitted" 1 '(:write))
     ("write: Permission denied"
      "sh: /usr/x: Permission denied" 2 '(:write))
     ("write: Read-only file system"
      "cp: /x: Read-only file system" 1 '(:write))
     ("sandbox in stderr"
      "sandbox-exec: deny(1) file-write-create /x" 1 '(:write))
     ("network: resolve"
      "curl: (6) Could not resolve host: example.com" 6 '(:network))
     ("network: failed to connect"
      "curl: (7) Failed to connect to 127.0.0.1 port 9 after 0 ms: Couldn't connect to server" 7 '(:network))
     ("network: socket EPERM"
      "nc: socket: Operation not permitted" 1 '(:network))
     ("deny-read" :deny-read 1 '(:deny-read))
     ("write and network, in order"
      (format nil "curl: (6) Could not resolve host: x~%touch: /etc/y: Operation not permitted") 1
      '(:write :network))
     ("an ordinary failure" "ls: nope: No such file or directory" 1 '())
     ("exit 0" "touch: /etc/x: Operation not permitted" 0 '())
     ("no exit code (signal)" "touch: /etc/x: Operation not permitted" nil '())
     ("no output" "" 1 '()))
  (with-root (root :local)
    (let* ((c (area-confinement root))
           (output (if (eq output :deny-read)
                       (format nil "cat: ~a/id_rsa: Permission denied" (deny-read-entry c))
                       output))
           (note (confinement-denial-note c output code)))
      (if (null reasons)
          (ok (null note) (format nil "~a: no note" label))
          (progn
            (ok (and (stringp note) (uiop:string-prefix-p "[sandbox: " note)) label)
            (ok (has "Do not retry the same command; it will be blocked again.]" note))
            (ok (not (find #\Newline note)) "one line")
            (let ((texts (list (cons :write "writes are allowed only under")
                               (cons :network "network access is off")
                               (cons :deny-read "are unreadable"))))
              (loop for (reason . text) in texts
                    do (if (member reason reasons)
                           (ok (has text note) (format nil "~a: names ~(~a~)" label reason))
                           (ok (not (has text note)) (format nil "~a: not ~(~a~)" label reason))))
              (when (member :write reasons)
                (ok (has (native-root root) note) "names the writable area"))
              (when (equal reasons '(:write :network))
                (ok (< (search "writes are allowed" note) (search "network access is off" note))
                    "reasons in :write :network :deny-read order"))))))))

(deftest confinement-denial-note-network-allowed
  (with-root (root :local)
    (let ((c (area-confinement root :network :allow)))
      (ok (null (confinement-denial-note c "curl: (6) Could not resolve host: example.com" 6))
          "network :allow: a resolve failure is not the sandbox")
      (let ((note (confinement-denial-note c "touch: /etc/x: Operation not permitted" 1)))
        (ok (has "writes are allowed only under" note))
        (ok (not (has "network access is off" note)))))))
