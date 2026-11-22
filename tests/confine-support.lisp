(in-package #:agent-tools/tests)

;;; R4d-4a helpers. The portable tests confine through compute-protocol's
;;; MODEL-CONFINEMENT-BACKEND (not a sandbox: it runs the argv unchanged) or a
;;; RECORDING-BACKEND, a model backend that records every BACKEND-CONFINE-ARGV
;;; call and can rewrite the command line, so a test sees which argv and env
;;; really ran, the start directory, and how often each launch was released
;;; (its cleanup runs once, from RELEASE-CONFINED-LAUNCH).

(defclass recording-backend (sc:model-confinement-backend)
  ((lock :initform (bt2:make-lock :name "recording-backend") :reader rb-lock)
   (launches :initform '() :accessor rb-launches
             :documentation "One RECORDED-LAUNCH per BACKEND-CONFINE-ARGV call, oldest first.")
   (prefix :initarg :prefix :initform nil :reader rb-prefix
           :documentation "A string put in front of the argv's last element (the command line).")
   (replace-command :initarg :replace-command :initform nil :reader rb-replace-command
                    :documentation "A string that replaces the command line altogether.")
   (program :initarg :program :initform nil :reader rb-program
            :documentation "A string that replaces argv[0]."))
  (:documentation "A model backend recording its launches."))

(defstruct recorded-launch
  argv            ; the argv CONFINE-COMMAND gave the backend
  launched-argv   ; the argv the backend returned
  directory       ; the physical start directory
  tmp             ; the private temp directory, or NIL
  (cleanups 0))   ; how many times the backend's cleanup ran

(defun make-recording-backend (&rest keys &key (features sc:+confinement-features+) &allow-other-keys)
  (let ((keys (copy-list keys)))
    (remf keys :features)
    (apply #'make-instance 'recording-backend :features (copy-list features) keys)))

(defmethod sc:backend-confine-argv ((b recording-backend) confinement argv directory tmp)
  (declare (ignore confinement))
  (let* ((last (car (last argv)))
         (command (cond ((rb-replace-command b))
                        ((rb-prefix b) (concatenate 'string (rb-prefix b) last))
                        (t last)))
         (out (append (butlast argv) (list command))))
    (when (rb-program b)
      (setf out (cons (rb-program b) (rest out))))
    (let ((rec (make-recorded-launch :argv (copy-list argv) :launched-argv out
                                     :directory directory :tmp tmp)))
      (bt2:with-lock-held ((rb-lock b))
        (setf (rb-launches b) (append (rb-launches b) (list rec))))
      (values out
              (list (cons "STACK_COMPUTE_CONFINEMENT" "model")
                    (cons "AGENT_TOOLS_BACKEND" "recording"))
              (list (lambda ()
                      (bt2:with-lock-held ((rb-lock b))
                        (incf (recorded-launch-cleanups rec)))))))))

(defun launches (b)
  (bt2:with-lock-held ((rb-lock b)) (copy-list (rb-launches b))))

(defun only-launch (b)
  "The single launch B recorded (NIL when it recorded none or several)."
  (let ((l (launches b)))
    (and (= 1 (length l)) (first l))))

(defun cleanups (rec)
  (recorded-launch-cleanups rec))

(defun released-eventually-p (rec &key (timeout 5))
  "REC's cleanup ran exactly once within TIMEOUT seconds, and never again
shortly after."
  (and (wait-for (lambda () (plusp (cleanups rec))) :timeout timeout)
       (progn (sleep 0.1) (= 1 (cleanups rec)))))

(defun tmp-gone-p (rec &key (timeout 5))
  (let ((tmp (recorded-launch-tmp rec)))
    (or (null tmp)
        (wait-for (lambda () (not (uiop:directory-exists-p tmp))) :timeout timeout))))

(defclass unavailable-backend (sc:model-confinement-backend) ()
  (:documentation "A model backend that cannot confine on this machine."))

(defmethod sc:confinement-available-p ((b unavailable-backend))
  (values nil "agent-tools test: unavailable on purpose"))

(defun area-confinement (root &rest keys)
  "A confinement whose writable area is ROOT (a pathlib PATH), :READ :ALL, the
default deny-read, network off, the default scrub; KEYS override."
  (apply #'sc:make-confinement :writable (list (native-root root)) keys))

(defun native-root (root)
  "ROOT (a local pathlib directory PATH) as a native directory namestring."
  (uiop:native-namestring (uiop:ensure-directory-pathname (p:path-pathname root))))

(defun same-directory-p (a b)
  "A and B (pathnames or native strings) name the same existing directory."
  (let ((ta (uiop:probe-file* (uiop:ensure-directory-pathname a) :truename t))
        (tb (uiop:probe-file* (uiop:ensure-directory-pathname b) :truename t)))
    (and ta tb (equal (namestring ta) (namestring tb)))))

(defmacro with-confined-shell ((tools root executor backend &rest keys) entries &body body)
  "WITH-SHELL with :CONFINEMENT over ROOT (AREA-CONFINEMENT) and :COMPUTE-BACKEND
BACKEND (evaluated once). KEYS may give :CONFINEMENT to override."
  (let ((b (gensym "BACKEND")))
    `(let ((,b ,backend))
       (with-root (,root :local)
         (make-tree ,root ,entries)
         (let ((,tools nil))
           (unwind-protect
                (progn
                  (setf ,tools (make-shell-tools :root ,root :executor ,executor
                                                 ,@keys
                                                 :confinement (area-confinement ,root)
                                                 :compute-backend ,b))
                  ,@body)
             (when ,tools
               (ignore-errors (close-shell-tools ,tools)))))))))

(defun env-command ()
  "A command printing the environment, one NAME=value per line."
  (pick "env" "set"))

(defun env-names (output)
  "The variable names in OUTPUT of ENV-COMMAND (lines NAME=value)."
  (loop for line in (lines output)
        for eq = (position #\= line :start (min 1 (length line)))
        when (and eq (plusp eq)
                  (every (lambda (c) (or (alphanumericp c) (char= c #\_))) (subseq line 0 eq)))
          collect (subseq line 0 eq)))

(defparameter +shell-added-variables+
  '("PWD" "OLDPWD" "SHLVL" "_" "COLUMNS" "LINES")
  "Variables a POSIX shell sets on its own, on top of the environment it got.")

(defun wait-job-finished (tools id &key (timeout 20))
  "job_output with wait_ms until job ID has finished. → the concatenated output."
  (let ((out '()))
    (wait-for (lambda ()
                (push (job-output tools id "wait_ms" 2000) out)
                (not (eq :running (shell-job-status (find-shell-job tools id)))))
              :timeout timeout)
    (push (job-output tools id) out)
    (format nil "~{~a~^~%~}" (reverse out))))

(defun ran-marker-command (root name)
  "A command creating the file NAME in ROOT (proof that something ran)."
  (let ((path (uiop:native-namestring (merge-pathnames name (p:path-pathname root)))))
    (pick (format nil "echo ran > '~a'" path)
          (format nil "echo ran> \"~a\"" path))))

(defun refused (form-thunk)
  "→ the SHELL-CONFINEMENT-REFUSED FORM-THUNK signals, or NIL (any other error
is returned as a list (:other condition))."
  (handler-case (progn (funcall form-thunk) nil)
    (shell-confinement-refused (c) c)
    (error (c) (list :other c))))
