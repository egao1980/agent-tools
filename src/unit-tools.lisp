(in-package #:agent-tools)

;;; UNIT-TOOLS (H8c): a tool source over a supervisor-protocol supervisor, so
;;; an agent reaches unit logs through search / read / tail instead of raw
;;; dumps, and controls units under permission rules. Brief:
;;; cl-stack/docs/capabilities/agent-tools-units.md.

(defun %h8c-pending (what)
  (error "agent-tools H8c: ~a is not implemented yet" what))

(defparameter +unit-tool-names+
  '("unit_list" "unit_search" "unit_read" "unit_tail" "unit_control")
  "Every unit tool name, in listing order.")

(defparameter +unit-search-kinds+
  '("error" "warning" "stack-trace" "test-failure" "exit")
  "unit_search kinds, the lowercase names of SUPERVISOR-PROTOCOL/SEARCH:+HIT-KINDS+.")

(defparameter +unit-control-actions+ '("stop" "restart" "input")
  "unit_control actions. Each is also the prefix of the call's permission
resource: \"<action>:<unit-id>\".")

(defconstant +max-excerpt-line+ 500
  "Characters of one log line shown by unit_search / unit_read / unit_tail;
longer lines end in \"… [+N chars]\".")

(defclass unit-tools ()
  ((supervisor :initarg :supervisor :reader unit-tools-supervisor
               :documentation "The SUPERVISOR-PROTOCOL:SUPERVISOR whose units the tools see.")
   (index :initarg :index :reader unit-tools-index
          :documentation "The SUPERVISOR-PROTOCOL/SEARCH:LOG-INDEX searched by unit_search.")
   (own-index-p :initarg :own-index-p :initform nil :reader %unit-own-index-p
                :documentation "True when MAKE-UNIT-TOOLS made the index (CLOSE-UNIT-TOOLS closes it).")
   (scope :initarg :scope :reader unit-tools-scope
          :documentation "Units owned by this scope or its descendants are visible.")
   (previous :initarg :previous :reader unit-tools-previous
             :documentation "True: logs of previous supervisors in the same state
directory are visible (read and search only).")
   (needs-approval :initarg :needs-approval :reader unit-tools-needs-approval
                   :documentation "When true, TOOL-NEEDS-APPROVAL-P is T for unit_control.")
   (max-output :initarg :max-output :reader unit-tools-max-output
               :documentation "Characters of one result, everything included.")
   (max-results :initarg :max-results :reader unit-tools-max-results
                :documentation "Rows of unit_list.")
   (default-k :initarg :default-k :reader unit-tools-default-k)
   (max-k :initarg :max-k :reader unit-tools-max-k)
   (context :initarg :context :reader unit-tools-context
            :documentation "unit_search context lines around a hit (default).")
   (max-context :initarg :max-context :reader unit-tools-max-context)
   (max-read-lines :initarg :max-read-lines :reader unit-tools-max-read-lines
                   :documentation "Cap on unit_read limit and unit_tail lines.")
   (max-wait :initarg :max-wait :reader unit-tools-max-wait
             :documentation "Milliseconds; cap on unit_tail wait_ms.")
   (names :initarg :names :reader unit-tools-names
          :documentation "Enabled tool names, a subset of +UNIT-TOOL-NAMES+ in that order.")
   (closed-p :initform nil :reader unit-tools-closed-p)
   (lock :initform (bt2:make-lock :name "unit-tools") :reader %unit-lock))
  (:documentation "Tool source over a supervisor's units and their logs. The
implementation may add slots."))

(defun unit-tools-p (x) (typep x 'unit-tools))

(defun make-unit-tools (&key supervisor index scope (previous t) (needs-approval t)
                             (max-output 30000) (max-results 100) (default-k 10) (max-k 50)
                             (context 2) (max-context 10) (max-read-lines 2000)
                             (max-wait 600000) (tools +unit-tool-names+))
  "→ a UNIT-TOOLS source over SUPERVISOR (required).
INDEX: a SUPERVISOR-PROTOCOL/SEARCH:LOG-INDEX (default: a new one over SUPERVISOR
with :PREVIOUS PREVIOUS, closed by CLOSE-UNIT-TOOLS). SCOPE: a scope of SUPERVISOR
(default its root); only units it owns, directly or below, are visible.
PREVIOUS: also show previous supervisors' logs (read-only). TOOLS: enabled names.
A missing or wrong SUPERVISOR, a scope of another supervisor, an unknown tool
name, DEFAULT-K above MAX-K or CONTEXT above MAX-CONTEXT → ERROR."
  (unless (typep supervisor 'sup:supervisor)
    (error 'type-error :datum supervisor :expected-type 'sup:supervisor))
  (when (and scope (not (and (typep scope 'sup:scope)
                             (eq supervisor (sup:scope-supervisor scope)))))
    (error "make-unit-tools: :scope ~s is not a scope of the supervisor" scope))
  (when (and index (not (typep index 'ss:log-index)))
    (error 'type-error :datum index :expected-type 'ss:log-index))
  (dolist (n (list max-output max-results default-k max-k max-read-lines))
    (check-type n (integer 1)))
  (check-type context (integer 0))
  (check-type max-context (integer 0))
  (check-type max-wait (integer 0))
  (when (> default-k max-k)
    (error "make-unit-tools: :default-k ~d is above :max-k ~d" default-k max-k))
  (when (> context max-context)
    (error "make-unit-tools: :context ~d is above :max-context ~d" context max-context))
  (let ((unknown (remove-if (lambda (n) (member n +unit-tool-names+ :test #'equal)) tools)))
    (when unknown
      (error "make-unit-tools: unknown tool name~p ~{~s~^, ~}; known: ~{~a~^, ~}"
             (length unknown) unknown +unit-tool-names+)))
  (make-instance 'unit-tools
                 :supervisor supervisor
                 :index (or index (ss:make-log-index :supervisor supervisor :previous previous))
                 :own-index-p (null index)
                 :scope (or scope (sup:supervisor-root-scope supervisor))
                 :previous (and previous t)
                 :needs-approval needs-approval
                 :max-output max-output :max-results max-results
                 :default-k default-k :max-k max-k
                 :context context :max-context max-context
                 :max-read-lines max-read-lines :max-wait max-wait
                 :names (remove-if-not (lambda (n) (member n tools :test #'equal))
                                       +unit-tool-names+)))

(defun %unit-enabled-p (tools name)
  (and (stringp name) (member name (unit-tools-names tools) :test #'string=) t))

(defun call-unit-tool (tools name arguments)
  "Run unit tool NAME of TOOLS synchronously on ARGUMENTS (a JSON object string
or a string-keyed hash table). → the result string. Failures signal
AI-AGENT-PROTOCOL:TOOL-FAILURE subtypes (UNIT-NOT-FOUND, TOOL-INPUT-ERROR,
TOOL-UNAVAILABLE); a name that is not enabled → AI-AGENT-PROTOCOL:AGENT-UNKNOWN-TOOL;
any call after CLOSE-UNIT-TOOLS → TOOL-UNAVAILABLE."
  (unless (%unit-enabled-p tools name)
    (error 'agent:agent-unknown-tool
           :name name
           :message (format nil "~a is not enabled on this unit-tools source (enabled: ~{~a~^, ~})"
                            name (unit-tools-names tools))))
  (when (unit-tools-closed-p tools)
    (error 'agent:tool-unavailable
           :message (format nil "~a cannot run: this unit-tools source is closed" name)))
  (%run-unit-tool tools name (%decode-arguments arguments)))

(defun close-unit-tools (tools)
  "Refuse later calls; close the index when MAKE-UNIT-TOOLS made it. Units keep
running. Idempotent. → NIL."
  (let ((close-index (bt2:with-lock-held ((%unit-lock tools))
                       (unless (unit-tools-closed-p tools)
                         (setf (slot-value tools 'closed-p) t)
                         (%unit-own-index-p tools)))))
    (when close-index
      (ss:close-log-index (unit-tools-index tools))))
  nil)
