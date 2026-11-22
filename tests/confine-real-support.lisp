(in-package #:agent-tools/tests)

;;; R4d-4a against a real kernel sandbox (SBCL on macOS or Linux): Seatbelt
;;; (compute-backend-seatbelt) or Landlock / bubblewrap (compute-backend-landlock).
;;; DEFINE-REAL-CONFINEMENT-TESTS defines the same tests for each backend; each
;;; test skips, with the backend's reason, when the backend cannot enforce the
;;; policy on this machine (e.g. a CI container without Landlock or namespaces,
;;; or a harness that is itself sandboxed).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets))

(defun real-confinement (root)
  "The plan-mode shape: the area writable, read all but the credential
locations, network off."
  (sc:make-confinement :writable (list (native-root root)) :read :all :network :none))

(defun real-backend-refusal (backend confinement)
  "NIL when BACKEND can enforce CONFINEMENT here, else the reason (a string)."
  (multiple-value-bind (ok reason) (sc:confinement-available-p backend)
    (if (not ok)
        (or reason "unavailable")
        (handler-case (progn (sc:check-confinement backend confinement) nil)
          (sc:compute-error (e) (princ-to-string e))))))

(defun call-with-outside-victim (fn)
  "FN on the native namestring of a file holding \"orig\" in a fresh temp
directory outside any test root."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "agent-tools-outside-~36R" (random (expt 36 8) (make-random-state t)))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (unwind-protect
         (let ((victim (merge-pathnames "victim.txt" (truename dir))))
           (with-open-file (s victim :direction :output :if-exists :supersede)
             (write-line "orig" s))
           (funcall fn (uiop:native-namestring victim)))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(defun file-text (native)
  (uiop:read-file-string (uiop:parse-native-namestring native)))

(defun call-with-loopback-server (fn)
  "FN on (port hits): an HTTP/1.0 server on 127.0.0.1 answering \"ok\" to every
request; HITS is a function → how many connections it accepted."
  (let ((server (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (hits (list 0))
        (stop (list nil))
        (thread nil))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address server) t)
           (sb-bsd-sockets:socket-bind server #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen server 8)
           (setf (sb-bsd-sockets:non-blocking-mode server) t)
           (setf thread
                 (bt2:make-thread
                  (lambda ()
                    (loop until (car stop)
                          do (let ((c (handler-case (sb-bsd-sockets:socket-accept server)
                                        (error () nil))))
                               (if (null c)
                                   (sleep 0.02)
                                   (progn
                                     (incf (car hits))
                                     (ignore-errors
                                      (setf (sb-bsd-sockets:non-blocking-mode c) nil)
                                      (let ((s (sb-bsd-sockets:socket-make-stream
                                                c :input t :output t :element-type 'character
                                                  :external-format :latin-1 :timeout 3)))
                                        (loop for line = (read-line s nil "")
                                              until (<= (length (string-right-trim '(#\Return) line)) 0))
                                        (format s "HTTP/1.0 200 OK~c~cContent-Length: 2~c~c~c~cok"
                                                #\Return #\Linefeed #\Return #\Linefeed
                                                #\Return #\Linefeed)
                                        (finish-output s)))
                                     (ignore-errors (sb-bsd-sockets:socket-close c)))))))
                  :name "agent-tools loopback server"))
           (funcall fn (nth-value 1 (sb-bsd-sockets:socket-name server))
                    (lambda () (car hits))))
      (setf (car stop) t)
      (when thread (ignore-errors (bt2:join-thread thread)))
      (ignore-errors (sb-bsd-sockets:socket-close server)))))

(defun curl-command (port)
  (format nil "curl -s -m 5 -o /dev/null http://127.0.0.1:~d/; echo \"curl=$?\"" port))

(defun have-curl-p ()
  (zerop (nth-value 2 (uiop:run-program '("/bin/sh" "-c" "command -v curl")
                                        :ignore-error-status t :output nil))))

(defmacro with-real-shell ((tools root executor backend-form skip-label) &body body)
  "TOOLS: a SHELL-TOOLS over a fresh ROOT confined by REAL-CONFINEMENT through
BACKEND-FORM; skip when the backend cannot enforce it here."
  (let ((b (gensym "B")) (c (gensym "C")) (why (gensym "WHY")))
    `(with-root (,root :local)
       (let* ((,b ,backend-form)
              (,c (real-confinement ,root))
              (,why (real-backend-refusal ,b ,c)))
         (if ,why
             (skip (format nil "~a: ~a" ,skip-label ,why))
             (let ((,tools (make-shell-tools :root ,root :executor ,executor
                                             :confinement ,c :compute-backend ,b)))
               (unwind-protect (progn ,@body)
                 (close-shell-tools ,tools))))))))

(defmacro define-real-confinement-tests (prefix backend-form)
  "Real-sandbox tests named <PREFIX>-…, once per executor, confined by BACKEND-FORM."
  (flet ((name (suffix) (intern (format nil "~a-~a" prefix suffix))))
    `(progn
       (deftest-executors ,(name "write-outside-denied")
         (call-with-outside-victim
          (lambda (victim)
            (with-real-shell (tools root executor ,backend-form ,(string-downcase prefix))
              (let ((r (bash tools (format nil "echo pwned > '~a'; echo inside > inside.txt; cat inside.txt"
                                           victim))))
                (ok (equal (format nil "orig~%") (file-text victim)) "the victim is unchanged")
                (ok (exists-at root "inside.txt") "a write inside the area works")
                (ok (has "inside" r)))
              (let ((r (bash tools (format nil "echo pwned > '~a'" victim))))
                (ok (not (uiop:string-suffix-p r "[exit code 0]")) "the blocked write fails")
                (ok (has "[sandbox: " r) "… and the result names the likely constraint")
                (ok (equal (format nil "orig~%") (file-text victim))))))))
       (deftest-executors ,(name "child-write-outside-denied")
         (call-with-outside-victim
          (lambda (victim)
            (with-real-shell (tools root executor ,backend-form ,(string-downcase prefix))
              (bash tools (format nil "sh -c \"echo pwned > '~a'\" & wait" victim))
              (bash tools (format nil "echo pwned > '~a' &" victim) "run_in_background" t)
              (sleep 0.5)
              (ok (equal (format nil "orig~%") (file-text victim)) "children inherit the sandbox")))))
       (deftest-executors ,(name "network-denied")
         (if (not (have-curl-p))
             (skip "no curl on PATH")
             (call-with-loopback-server
              (lambda (port hits)
                ;; Control: unconfined, the same command reaches the server.
                (with-shell (plain root executor) '()
                  (ok (has "curl=0" (bash plain (curl-command port) "timeout_ms" 20000))
                      "unconfined curl reaches the loopback server"))
                (let ((before (funcall hits)))
                  (with-real-shell (tools root executor ,backend-form ,(string-downcase prefix))
                    (let ((r (bash tools (curl-command port) "timeout_ms" 20000)))
                      (ok (not (has "curl=0" r)) (format nil "confined curl fails: ~a" r))
                      (ok (= before (funcall hits)) "the server saw no connection")))))))))))
