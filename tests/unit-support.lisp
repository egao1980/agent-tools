(in-package #:agent-tools/tests)

;;; H8c helpers. Each test gets a fresh supervisor over a fresh temp state
;;; directory (so units log to rotated segment files) and a UNIT-TOOLS over
;;; it; both are closed afterwards. Expected rows, refs and line numbers are
;;; computed from the supervisor and the log reader (supervisor-protocol), never
;;; from the tool under test.

(defun call-with-state-dir (fn)
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "agent-tools-units-~36R" (random (expt 36 8) (make-random-state t)))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (unwind-protect (funcall fn dir)
      (ignore-errors (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(defun quiet-shutdown (sup)
  (ignore-errors
   (handler-bind ((sup:stop-failed (lambda (c)
                                     (let ((r (find-restart 'sup:abandon c)))
                                       (when r (invoke-restart r))))))
     (sup:shutdown-supervisor sup :grace 0.5 :force-grace 1))))

(defun call-with-units (fn &key (state t) supervisor-keys tool-keys)
  (call-with-state-dir
   (lambda (dir)
     (let* ((sup (apply #'sup:make-supervisor :exit-hook nil
                        (append (and state (list :state-directory dir)) supervisor-keys)))
            (tools nil))
       (unwind-protect
            (progn
              (setf tools (apply #'make-unit-tools :supervisor sup tool-keys))
              (funcall fn tools sup dir))
         (when tools (ignore-errors (close-unit-tools tools)))
         (quiet-shutdown sup))))))

(defmacro with-units ((tools sup &key (state t) supervisor-keys tool-keys) &body body)
  "BODY with SUP a fresh supervisor (a temp state directory DIR unless :STATE NIL;
SUPERVISOR-KEYS) and TOOLS a UNIT-TOOLS over it (TOOL-KEYS), both closed after."
  `(call-with-units (lambda (,tools ,sup dir) (declare (ignorable ,tools ,sup dir)) ,@body)
                    :state ,state :supervisor-keys (list ,@supervisor-keys)
                    :tool-keys (list ,@tool-keys)))

(defun ucall (tools name &rest kv)
  "CALL-UNIT-TOOL with arguments given as a JSON object string."
  (call-unit-tool tools name (apply #'json-args kv)))

(defun wait-for (predicate &key (timeout 10) (step 0.02))
  (loop with deadline = (+ (now) timeout)
        for v = (funcall predicate)
        when (or v (> (now) deadline)) return v
        do (sleep step)))

(defun settle (unit)
  "Wait until UNIT finished and its log has its end record."
  (sup:wait-unit unit :timeout 20)
  (wait-for (lambda () (ignore-errors (sup:log-closed-p unit))) :timeout 10)
  unit)

(defun thread-lines (sup lines &key (kind :shell-job) owner labels stderr-p (wait t))
  "Start a thread unit printing LINES (to *ERROR-OUTPUT* for lines satisfying
STDERR-P), then returning; wait for it and its log unless WAIT is NIL."
  (let ((u (apply #'sup:start-unit sup kind
                  :function (lambda (u)
                              (declare (ignore u))
                              (dolist (l lines)
                                (if (and stderr-p (funcall stderr-p l))
                                    (progn (write-line l *error-output*) (finish-output *error-output*))
                                    (write-line l)))
                              (finish-output)
                              :done)
                  :labels labels
                  (and owner (list :owner owner)))))
    (if wait (settle u) u)))

(defun gated-lines (sup first-lines gate rest-lines &key (kind :shell-job))
  "A thread unit printing FIRST-LINES, then waiting until (CAR GATE), then REST-LINES."
  (sup:start-unit sup kind
                  :function (lambda (u)
                              (declare (ignore u))
                              (dolist (l first-lines) (write-line l))
                              (finish-output)
                              (loop until (car gate) do (sleep 0.01))
                              (dolist (l rest-lines) (write-line l))
                              (finish-output)
                              :done)))

(defun lisp-argv (&rest forms)
  "argv running this SBCL with no init files, evaluating FORMS (strings)."
  (append (list (uiop:native-namestring sb-ext:*runtime-pathname*)
                "--core" (uiop:native-namestring sb-ext:*core-pathname*)
                "--noinform" "--no-sysinit" "--no-userinit" "--non-interactive")
          (loop for f in forms append (list "--eval" f))))

(defun sleeper-argv (seconds) (lisp-argv (format nil "(sleep ~d)" seconds)))

(defun exit-argv (code) (lisp-argv (format nil "(sb-ext:exit :code ~d :abort t)" code)))

(defun echo-argv ()
  (lisp-argv "(loop for l = (read-line *standard-input* nil) while l do (write-line l) (finish-output))"))

;;; Expected values, computed independently of the tools.

(defun rfc3339 (universal-time)
  (multiple-value-bind (s mi h d mo y) (decode-universal-time universal-time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" y mo d h mi s)))

(defun rfc3339-ms (unix-ms)
  (multiple-value-bind (secs ms) (floor unix-ms 1000)
    (let ((base (rfc3339 (+ secs (encode-universal-time 0 0 0 1 1 1970 0)))))
      (format nil "~aZ" (concatenate 'string (subseq base 0 19) (format nil ".~3,'0d" ms))))))

(defun status-word (u)
  (let ((code (sup:unit-exit-code u)))
    (ecase (sup:unit-state u)
      (:running "running") (:starting "starting") (:killed "killed")
      (:exited (if code (format nil "exited ~d" code) "exited"))
      (:failed (if code (format nil "failed ~d" code) "failed")))))

(defun owner-path (u)
  (let ((path (sup:scope-path (sup:unit-owner u))))
    (if path (format nil "~{~a~^/~}" path) "-")))

(defun labels-text (u)
  (let ((l (sup:unit-labels u)))
    (if l
        (format nil "~{~a~^,~}" (loop for (k v) on l by #'cddr
                                      collect (format nil "~(~a~)=~a" k v)))
        "-")))

(defun command-text (u)
  (if (sup:unit-command u) (format nil "~{~a~^ ~}" (sup:unit-command u)) "<thread>"))

(defun lines-text (u)
  (handler-case (princ-to-string (sup:log-last-line u))
    (sup:log-not-found () "-")))

(defun tsv (&rest fields)
  "FIELDS joined by TAB."
  (with-output-to-string (o)
    (loop for (f . more) on fields
          do (princ f o) (when more (write-char #\Tab o)))))

(defun expected-row (u)
  (tsv (sup:unit-id u) (string-downcase (symbol-name (sup:unit-kind u))) (status-word u)
       (owner-path u) (rfc3339 (sup:unit-start-time u)) (lines-text u) (labels-text u)
       (command-text u)))

(defparameter +unit-list-header+
  (tsv "id" "kind" "state" "owner" "started" "lines" "labels" "command"))

(defun log-line (mark line text)
  (format nil "~a~6d~c~a" mark line #\Tab text))

(defun record-of (unit text &key (nth 0))
  "The NTH output record of UNIT's log whose text is TEXT."
  (nth nth (remove text (sup:log-read unit) :key #'sup:log-record-text :test-not #'string=)))

(defun ref-of (key record)
  (format nil "~a:~d:~d" key (sup:log-record-segment record) (sup:log-record-line record)))

(defparameter +build-lines+
  '("cc -Wall -Wextra -O2 -c -o src/lexer.o src/lexer.c"
    "cc -Wall -Wextra -O2 -c -o src/list.o src/list.c"
    "cc -Wall -Wextra -O2 -c -o src/parser.o src/parser.c"
    "src/parser.c:7:12: error: use of undeclared identifier 'tokn'; did you mean 'tok'?"
    "    7 |     return tokn->kind + depth;"
    "      |            ^~~~"
    "1 error generated."
    "make: *** [src/parser.o] Error 1"
    "cc -Wall -Wextra -O2 -c -o src/util.o src/util.c"
    "src/util.c:2:32: warning: unused variable 'y' [-Wunused-variable]"
    "1 warning generated."
    "make: Target `all' not remade because of errors.")
  "A short build log (from supervisor-protocol's recorded corpus).")

(defparameter +trace-lines+
  '("2026-11-21T12:00:01.250Z GET /api/users/1/roles 500 3ms"
    "TypeError: Cannot read properties of undefined (reading 'roles')"
    "    at userRoles (/home/dev/webapp/src/routes.js:5:23)"
    "    at Server.<anonymous> (/home/dev/webapp/src/server.js:13:30)"
    "    at Server.emit (node:events:518:28)"
    "2026-11-21T12:00:01.500Z GET /api/users 200 4ms")
  "A dev-server log with a JS stack on stderr.")

(defun stderr-line-p (l) (not (and (>= (length l) 5) (string= "2026-" l :end2 5))))

(defun result-lines (string) (uiop:split-string string :separator '(#\Newline)))
