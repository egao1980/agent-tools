(in-package #:agent-tools/tests)

;;; R4d-4a under a supervisor: a confined source starts every command as a
;;; :SHELL-JOB unit with START-UNIT :CONFINEMENT and its own compute executor
;;; over :COMPUTE-BACKEND; the compute executor confines, launches and releases.

(defmacro with-confined-supervised-shell ((tools sup root backend &rest keys) &body body)
  "TOOLS: a confined SHELL-TOOLS (AREA-CONFINEMENT of ROOT, BACKEND) over a fresh
supervisor SUP with its own temp state directory."
  (let ((b (gensym "BACKEND")) (state (gensym "STATE")))
    `(let ((,b ,backend))
       (call-with-state-dir
        (lambda (,state)
          (let ((,sup (sup:make-supervisor :exit-hook nil :state-directory ,state)))
            (unwind-protect
                 (with-root (,root :local)
                   (let ((,tools nil))
                     (unwind-protect
                          (progn
                            (setf ,tools (make-shell-tools :root ,root :supervisor ,sup ,@keys
                                                           :confinement (area-confinement ,root)
                                                           :compute-backend ,b))
                            ,@body)
                       (when ,tools (ignore-errors (close-shell-tools ,tools))))))
              (quiet-shutdown ,sup))))))))

(deftest supervised-confined-unit
  (let ((b (make-recording-backend :prefix (pick "echo wrapped; " "echo wrapped& "))))
    (with-confined-supervised-shell (tools sup root b)
      (let ((r (bash tools "echo hi")))
        (ok (uiop:string-prefix-p "wrapped" r) "the launch argv ran")
        (ok (uiop:string-suffix-p r "[exit code 0]")))
      (let ((u (first (shell-units sup))))
        (ok u "the command ran as a unit")
        (when u
          (ok (sup:unit-confined-p u) "the unit is confined")
          (ok (equal '(:tool "bash") (sup:unit-labels u)) "labels unchanged")
          (ok (typep (sup:unit-executor u) 'sup:compute-executor))
          (ok (eq b (sup:compute-executor-compute-backend (sup:unit-executor u)))
              "the source's backend")))
      (let ((rec (only-launch b)))
        (ok rec)
        (when rec
          (ok (released-eventually-p rec) "the compute executor released it")
          (ok (tmp-gone-p rec)))))))

(deftest supervised-confined-env-and-directory
  (let ((b (make-recording-backend))
        (old (uiop:getenv "AGENT_TOOLS_CANARY")))
    (setf (uiop:getenv "AGENT_TOOLS_CANARY") "leaked")
    (unwind-protect
         (with-confined-supervised-shell (tools sup root b :env '(("AGENT_TOOLS_SOURCE" . "yes")))
           (ensure-directories-exist (merge-pathnames "sub/" (p:path-pathname root)))
           (bash tools (env-command) "run_in_background" t "workdir" "sub")
           (let* ((id (shell-job-id (first (shell-tools-jobs tools))))
                  (out (wait-job-finished tools id))
                  (job (find-shell-job tools id))
                  (names (env-names out)))
             (ok (shell-job-confined-p job))
             (ok (typep (shell-job-launch job) 'sc:confined-launch)
                 "the unit's confined launch")
             (ok (has "STACK_COMPUTE_CONFINEMENT=model" out))
             (ok (has "AGENT_TOOLS_SOURCE=yes" out))
             (ok (has "TERM=dumb" out))
             (ok (not (member "AGENT_TOOLS_CANARY" names :test #'string-equal)) "no leak")
             (let ((rec (only-launch b)))
               (ok (and rec (same-directory-p (recorded-launch-directory rec)
                                              (merge-pathnames "sub/" (p:path-pathname root))))
                   "workdir → :directory"))))
      (setf (uiop:getenv "AGENT_TOOLS_CANARY") (or old "")))))

(deftest supervised-confined-release-on-kill
  (let ((b (make-recording-backend)))
    (with-confined-supervised-shell (tools sup root b)
      (bash tools (sleep-command 30) "run_in_background" t)
      (let ((rec (only-launch b))
            (id (shell-job-id (first (shell-tools-jobs tools)))))
        (ok rec)
        (when rec
          (sleep 0.3)
          (ok (zerop (cleanups rec)) "not released while running")
          (shell tools "job_kill" "job_id" id)
          (ok (released-eventually-p rec) "released once after job_kill"))))))

(deftest supervised-confined-release-on-timeout
  (let ((b (make-recording-backend)))
    (with-confined-supervised-shell (tools sup root b)
      (ok (typep (failure (bash tools (sleep-command 30) "timeout_ms" 300)) 'shell-timeout))
      (let ((rec (only-launch b)))
        (ok (and rec (released-eventually-p rec)) "released once after the timeout")))))

(deftest supervised-confined-refusal
  (let ((b (make-recording-backend :features '(:filesystem :deny-read))))
    (with-confined-supervised-shell (tools sup root b)
      (let ((c (refused (lambda () (bash tools (ran-marker-command root "ran.txt"))))))
        (ok (typep c 'shell-confinement-refused) (format nil "~a" c))
        (when (typep c 'shell-confinement-refused)
          (ok (typep (shell-confinement-refused-cause c) 'sc:confinement-unsupported)
              "the cause is the compute condition, not the unit-start-error")
          (ok (has "was not run" (message c)))))
      (ok (null (shell-units sup)) "no unit registered")
      (ok (null (launches b)))
      (sleep 0.3)
      (ok (not (exists-at root "ran.txt")) "nothing ran")
      (ok (null (shell-tools-jobs tools))))))

(deftest supervised-confined-job-list
  (with-confined-supervised-shell (tools sup root (sc:make-model-confinement-backend))
    (bash tools "echo one" "run_in_background" t)
    (let ((id (shell-job-id (first (shell-tools-jobs tools)))))
      (wait-job-finished tools id)
      (ok (equal (format nil "~a~cexited 0~cconfined~cecho one" id #\Tab #\Tab #\Tab)
                 (shell tools "job_list"))))))

(deftest supervised-unconfined-job-has-no-launch
  (with-supervised-shell (tools sup root)
    (bash tools "echo one" "run_in_background" t)
    (let ((job (first (shell-tools-jobs tools))))
      (wait-job-finished tools (shell-job-id job))
      (ok (null (shell-job-launch job)))
      (ok (not (shell-job-confined-p job))))))
