(in-package #:agent-tools)

;;; H8c tool bodies. The brief's "Tools" section is the specification of every
;;; argument, output line and error; the unit-*-test files lock them. Shared
;;; rules:
;;;
;;; - Results are plain strings, LF-separated, no trailing LF, never longer
;;;   than UNIT-TOOLS-MAX-OUTPUT characters (notices included).
;;; - A log line is shown as (format nil "~a~6d~c~a" mark line #\Tab text):
;;;   MARK ">" for the hit / ref line, else " "; TEXT is the record text with
;;;   ANSI escapes and control characters (except TAB) removed, cut at
;;;   +MAX-EXCERPT-LINE+ characters with "… [+N chars]".
;;; - A gap is the line "[gap: lines A-B not in the log (reason, …)]".
;;; - Status words: running, starting, exited <code>, failed <code> (failed
;;;   alone for a thread), killed; a previous session's log: previous.
;;; - Times: RFC 3339 UTC, "2026-11-21T12:00:01Z" (unit_list, seconds) and
;;;   "2026-11-21T12:00:01.250Z" (unit_search, milliseconds).
;;; - Every result names the refs or the after_line / start_line to go on with.
;;;
;;; Invariants:
;;; - I-vis: a tool reaches a log only through %RESOLVE-LOG (one key) or
;;;   %VISIBLE-KEYS (unit_search); both apply the scope and the PREVIOUS flag.
;;; - I-cap: every result is assembled by reserving its header and footer at
;;;   their longest before lines are admitted (%FIT-COUNT), so MAX-OUTPUT holds
;;;   with every notice; %CAP is only a last guard for absurdly small caps.
;;; - Supervisor conditions are translated to tool conditions with HANDLER-CASE
;;;   at the call that signals them; STOP-FAILED is answered with the
;;;   supervisor's ABANDON restart (HANDLER-BIND), STOP-TIMEOUT is left to its
;;;   default (FORCE).

;;; --- shared text helpers -------------------------------------------------------

(defconstant +unix-epoch-ut+ (encode-universal-time 0 0 0 1 1 1970 0)
  "The Unix epoch as a universal time.")

(defun %cap (string max)
  "STRING cut to MAX characters (a last guard; results are built to fit)."
  (if (> (length string) max) (subseq string 0 max) string))

(defun %fit-count (lines room &key from-end)
  "How many of LINES, taken from the start (or the end with FROM-END), fit in
ROOM characters when each one is counted with one LF."
  (let ((used 0) (n 0))
    (dolist (l (if from-end (reverse lines) lines) n)
      (incf used (1+ (length l)))
      (when (> used room) (return n))
      (incf n))))

(defun %line-text (text)
  "Record TEXT for the model: ANSI escapes and control characters except TAB
removed, cut at +MAX-EXCERPT-LINE+ characters."
  (let* ((s (remove-if (lambda (c) (and (char/= c #\Tab) (%control-char-p c)))
                       (ss:strip-ansi text)))
         (n (length s)))
    (if (> n +max-excerpt-line+)
        (format nil "~a… [+~d chars]" (subseq s 0 +max-excerpt-line+) (- n +max-excerpt-line+))
        s)))

(defun %log-line (mark line text)
  (format nil "~a~6d~c~a" mark line #\Tab (%line-text text)))

(defun %gap-line (gap)
  (format nil "[gap: lines ~d-~d not in the log (~{~(~a~)~^, ~})]"
          (sup:log-gap-first-line gap) (sup:log-gap-last-line gap) (sup:log-gap-reasons gap)))

(defun %merged-lines (records gaps &key mark-line)
  "Display lines of RECORDS and GAPS in line order (each gap before the first
record past it), as a list of (text . last-line-covered). MARK-LINE gets \">\"."
  (let ((out '()))
    (flet ((gap (g) (push (cons (%gap-line g) (sup:log-gap-last-line g)) out)))
      (dolist (r records)
        (loop while (and gaps (< (sup:log-gap-first-line (first gaps)) (sup:log-record-line r)))
              do (gap (pop gaps)))
        (let ((line (sup:log-record-line r)))
          (push (cons (%log-line (if (eql line mark-line) ">" " ") line (sup:log-record-text r))
                      line)
                out)))
      (mapc #'gap gaps))
    (nreverse out)))

(defun %rfc3339 (universal-time &optional ms)
  (multiple-value-bind (s mi h d mo y) (decode-universal-time universal-time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~@[.~3,'0d~]Z" y mo d h mi s ms)))

(defun %rfc3339-ms (unix-ms)
  (multiple-value-bind (secs ms) (floor unix-ms 1000)
    (%rfc3339 (+ secs +unix-epoch-ut+) ms)))

(defun %status-word (unit)
  "unit_list / unit_read: running, starting, exited <code>, failed <code>,
exited / failed (no code: a thread), killed."
  (let ((code (sup:unit-exit-code unit)))
    (ecase (sup:unit-state unit)
      (:running "running") (:starting "starting") (:killed "killed")
      (:exited (if code (format nil "exited ~d" code) "exited"))
      (:failed (if code (format nil "failed ~d" code) "failed")))))

(defun %status-phrase (unit)
  "unit_tail: the status word as a phrase."
  (let ((code (sup:unit-exit-code unit)))
    (ecase (sup:unit-state unit)
      (:running "running") (:starting "starting") (:killed "killed")
      (:exited (if code (format nil "exited with code ~d" code) "exited"))
      (:failed (if code (format nil "failed with code ~d" code) "failed")))))

(defun %abandon-stop (condition)
  "HANDLER-BIND policy for SUP:STOP-FAILED: take the supervisor's ABANDON
restart (the unit is marked :KILLED and swept later)."
  (let ((r (find-restart 'sup:abandon condition)))
    (when r (invoke-restart r))))

(defun %unit-log-or-nil (unit)
  "UNIT's SUP:UNIT-LOG, or NIL when its output is not logged (no state directory)."
  (handler-case (sup:unit-log unit)
    (sup:log-not-found () nil)))

;;; --- arguments -------------------------------------------------------------------

(defun %enum-arg (args key values)
  "String argument KEY, one of VALUES, or NIL when absent."
  (let ((v (%arg args key :string)))
    (when (and v (not (member v values :test #'string=)))
      (%input-error "~a must be one of ~{~a~^, ~}; got ~s" key values v))
    v))

(defun %streams-arg (args)
  "streams: an array of \"stdout\" / \"stderr\" → keywords (default both)."
  (let ((v (%arg args "streams" :array)))
    (if (or (null v) (zerop (length v)))
        (list :stdout :stderr)
        (remove-duplicates
         (map 'list (lambda (s)
                      (cond ((equal s "stdout") :stdout)
                            ((equal s "stderr") :stderr)
                            (t (%input-error "streams: unknown stream ~a; use \"stdout\" or \"stderr\""
                                             (%describe-value s)))))
              v)
         :from-end t))))

(defun %strings-arg (args key)
  "An array of strings KEY → a list, or NIL when absent."
  (let ((v (%arg args key :array)))
    (when v
      (map 'list (lambda (s)
                   (if (stringp s)
                       s
                       (%input-error "~a must be an array of strings, got the entry ~a"
                                     key (%describe-value s))))
           v))))

;;; --- parse-time-argument -----------------------------------------------------------

(defun %unix-ms-now ()
  (* 1000 (- (get-universal-time) +unix-epoch-ut+)))

(defun %digits (s start end)
  "The decimal number S[START, END), or NIL when that is not all digits."
  (and (<= end (length s)) (< start end)
       (every #'digit-char-p (subseq s start end))
       (parse-integer s :start start :end end)))

(defun %duration-ago (s now)
  "\"<n>s|m|h|d\" (n a positive integer) → NOW minus that, else NIL."
  (let* ((n (length s))
         (v (and (>= n 2) (%digits s 0 (1- n))))
         (unit (and v (plusp v) (position (char s (1- n)) "smhd"))))
    (and unit (- now (* v (svref #(1000 60000 3600000 86400000) unit))))))

(defun %days-in-month (year month)
  (if (= month 2)
      (if (and (zerop (mod year 4)) (or (plusp (mod year 100)) (zerop (mod year 400)))) 29 28)
      (svref #(31 28 31 30 31 30 31 31 30 31 30 31) (1- month))))

(defun %rfc3339-unix-ms (s)
  "YYYY-MM-DD(T|t| )hh:mm:ss[.fraction](Z|z|±hh:mm) → Unix ms, else NIL."
  (let ((n (length s)))
    (when (and (>= n 20) (char= (char s 4) #\-) (char= (char s 7) #\-)
               (find (char s 10) "Tt ") (char= (char s 13) #\:) (char= (char s 16) #\:))
      (let ((y (%digits s 0 4)) (mo (%digits s 5 7)) (d (%digits s 8 10))
            (h (%digits s 11 13)) (mi (%digits s 14 16)) (sec (%digits s 17 19))
            (i 19) (ms 0) (offset nil))
        (when (and (< i n) (char= (char s i) #\.))
          (let ((e (or (position-if-not #'digit-char-p s :start (1+ i)) n)))
            (when (= e (1+ i))
              (return-from %rfc3339-unix-ms nil))
            (setf ms (floor (* 1000 (parse-integer s :start (1+ i) :end e)) (expt 10 (- e i 1)))
                  i e)))
        (cond ((and (= i (1- n)) (find (char s i) "Zz"))
               (setf offset 0))
              ((and (= (- n i) 6) (find (char s i) "+-") (char= (char s (+ i 3)) #\:))
               (let ((oh (%digits s (1+ i) (+ i 3))) (om (%digits s (+ i 4) n)))
                 (when (and oh om (< oh 24) (< om 60))
                   (setf offset (* (if (char= (char s i) #\-) -1 1) (+ (* oh 60) om) 60000))))))
        (when (and y mo d h mi sec offset (>= y 1900) (<= 1 mo 12)
                   (<= 1 d (%days-in-month y mo)) (< h 24) (< mi 60) (< sec 60))
          (+ (* 1000 (- (encode-universal-time sec mi h d mo y 0) +unix-epoch-ut+))
             ms (- offset)))))))

(defun parse-time-argument (string &key now)
  "→ Unix epoch milliseconds for a since / until argument: an RFC 3339 time
(\"2026-11-21T12:00:00Z\", fraction and ±hh:mm offset allowed, a space instead of
T allowed) or a duration ago: a positive integer and one unit of s, m, h, d
(\"90s\", \"10m\", \"2h\", \"1d\"), relative to NOW (Unix ms; default the current
time). Anything else → AI-AGENT-PROTOCOL:TOOL-INPUT-ERROR naming the value."
  (or (and (stringp string)
           (or (%duration-ago string (or now (%unix-ms-now)))
               (%rfc3339-unix-ms string)))
      (%input-error "~s is not a time: give an RFC 3339 time (2026-11-21T12:00:00Z) or a duration ago (\"90s\", \"10m\", \"2h\", \"1d\")"
                    string)))

;;; --- visibility ------------------------------------------------------------------

(defstruct (%vlog (:constructor %make-vlog (key unit log previous-p)))
  "What a tool may look at for one key. UNIT: the live SUP:UNIT (NIL for a
previous session's log); LOG: its SUP:UNIT-LOG, or NIL without a log file."
  key unit log previous-p)

(defun %owned-p (unit scope)
  "UNIT's owner is SCOPE or one of its descendants."
  (loop for s = (sup:unit-owner unit) then (sup:scope-parent s)
        while s
        thereis (eq s scope)))

(defun %previous-key-p (tools key)
  "KEY names another supervisor's log (<supervisor-id>--<unit-id>)."
  (let ((cut (search "--" key)))
    (and cut (string/= (subseq key 0 cut)
                       (sup:supervisor-id (unit-tools-supervisor tools))))))

(defun %unit-not-found (arg given)
  (error 'unit-not-found
         :unit given
         :message (format nil "~a ~a is not a unit these tools can see; unit_list shows the units (include_previous true also lists earlier sessions' logs)"
                          arg given)))

(defun %resolve-log (tools key &key (given key) (arg "unit"))
  "KEY (a unit id, or a previous session's key) → a %VLOG, else UNIT-NOT-FOUND
naming GIVEN (the argument ARG as the model wrote it)."
  (let ((u (sup:find-unit (unit-tools-supervisor tools) key)))
    (cond ((and u (%owned-p u (unit-tools-scope tools)))
           (%make-vlog key u (%unit-log-or-nil u) nil))
          ((and (null u) (unit-tools-previous tools) (%previous-key-p tools key))
           (let ((log (ss:index-unit-log (unit-tools-index tools) key)))
             (if log
                 (%make-vlog key nil log t)
                 (%unit-not-found arg given))))
          (t (%unit-not-found arg given)))))

(defun %visible-keys (tools)
  "The index keys these tools may search, in the index's order: the scope's
units, then (with UNIT-TOOLS-PREVIOUS) previous sessions' logs."
  (let ((live (make-hash-table :test #'equal)))
    (dolist (u (sup:find-units (unit-tools-supervisor tools) :owner (unit-tools-scope tools)))
      (setf (gethash (sup:unit-id u) live) t))
    (remove-if-not (lambda (k) (or (gethash k live)
                                   (and (unit-tools-previous tools) (%previous-key-p tools k))))
                   (ss:index-log-keys (unit-tools-index tools)))))

(defun %vlog-status (v)
  (if (%vlog-previous-p v) "previous" (%status-word (%vlog-unit v))))

(defun %vlog-phrase (v)
  (if (%vlog-previous-p v) "from a previous session" (%status-phrase (%vlog-unit v))))

(defun %no-log-body (v room)
  "The in-memory head and tail of a unit without a log file, cleaned, final
newlines dropped, truncated as H2c to fit ROOM characters with its notice; NIL
when empty."
  (let* ((text (string-right-trim '(#\Newline) (clean-shell-output (sup:unit-output (%vlog-unit v)))))
         (notice (length (format nil "[truncated: ~d characters omitted; redirect the output to a file and use read or grep on it]"
                                 (length text))))
         (cut (if (<= (length text) room)
                  text
                  (string-right-trim '(#\Newline)
                                     (truncate-shell-output text (max 0 (- room notice 2)))))))
    (and (plusp (length cut)) cut)))

;;; --- unit_list -------------------------------------------------------------------

(defun %tsv (fields)
  "FIELDS (printed with PRINC) joined by TAB."
  (format nil (concatenate 'string "~{~a~^" (string #\Tab) "~}") fields))

(defparameter +unit-list-header+
  (%tsv '("id" "kind" "state" "owner" "started" "lines" "labels" "command")))

(defparameter +unit-list-states+
  '("live" "finished" "starting" "running" "exited" "failed" "killed"))

(defun %state-selected-p (state filter)
  (cond ((null filter) t)
        ((string= filter "live") (member state '(:starting :running)))
        ((string= filter "finished") (member state '(:exited :failed :killed)))
        (t (string-equal filter (symbol-name state)))))

(defun %label-arg (args)
  "label: an object of string pairs → an alist, or NIL."
  (let ((h (%arg args "label" :object)))
    (when h
      (loop for k being the hash-keys of h using (hash-value v)
            unless (stringp v)
              do (%input-error "label values must be strings; ~a is ~a" k (%describe-value v))
            collect (cons k v)))))

(defun %labels-selected-p (labels filter)
  "Every (key . value) of FILTER is in the plist LABELS (key by name, value by
PRINC-TO-STRING)."
  (every (lambda (pair)
           (loop for (k v) on labels by #'cddr
                 thereis (and (string-equal (car pair) (string k))
                              (string= (cdr pair) (princ-to-string v)))))
         filter))

(defun %owner-column (unit)
  (let ((path (sup:scope-path (sup:unit-owner unit))))
    (if path (format nil "~{~a~^/~}" path) "-")))

(defun %unit-row (unit)
  (let ((labels (sup:unit-labels unit))
        (start (sup:unit-start-time unit)))
    (%tsv
     (list (sup:unit-id unit)
           (string-downcase (symbol-name (sup:unit-kind unit)))
           (%status-word unit)
           (%owner-column unit)
           (if start (%rfc3339 start) "-")
           (handler-case (princ-to-string (sup:log-last-line unit))
             (sup:log-not-found () "-"))
           (if labels
               (format nil "~{~a~^,~}" (loop for (k v) on labels by #'cddr
                                             collect (format nil "~(~a~)=~a" k v)))
               "-")
           (if (sup:unit-command unit)
               (format nil "~{~a~^ ~}" (sup:unit-command unit))
               "<thread>")))))

(defun %previous-row (tools key kind owner label)
  "The unit_list row of the previous session's log KEY, or NIL when a filter
leaves it out."
  (let* ((log (ss:index-unit-log (unit-tools-index tools) key))
         (first-record (and log (first (sup:log-read log :types t :limit 1))))
         (header (and log (first (sup:log-read log :types '(:header) :limit 1))))
         (kind-name (let ((k (and header (getf (sup:log-record-detail header) :kind))))
                      (if k (string-downcase (princ-to-string k)) "-"))))
    (when (and log
               (or (null kind) (string= kind kind-name))
               (or (null owner) (agent:permission-glob-match-p owner "-"))
               (null label))
      (%tsv
       (list key kind-name "previous" "-"
             (let ((wall (and first-record (sup:log-record-wall-time first-record))))
               (if wall (%rfc3339 (+ (floor wall 1000) +unix-epoch-ut+)) "-"))
             (sup:log-last-line log) "-" "-")))))

(defun %tool-unit-list (tools args)
  (let* ((kind (%arg args "kind" :string))
         (state (%enum-arg args "state" +unit-list-states+))
         (owner (%arg args "owner" :string))
         (label (%label-arg args))
         (previous (%arg args "include_previous" :boolean))
         (rows (append
                (loop for u in (sup:find-units (unit-tools-supervisor tools)
                                               :owner (unit-tools-scope tools))
                      when (and (or (null kind)
                                    (string= kind (string-downcase (symbol-name (sup:unit-kind u)))))
                                (%state-selected-p (sup:unit-state u) state)
                                (or (null owner)
                                    (agent:permission-glob-match-p owner (%owner-column u)))
                                (%labels-selected-p (sup:unit-labels u) label))
                        collect (%unit-row u))
                (and previous (null state) (unit-tools-previous tools)
                     (loop for key in (sort (remove-if-not (lambda (k) (%previous-key-p tools k))
                                                           (ss:index-log-keys (unit-tools-index tools)))
                                            #'string<)
                           for row = (%previous-row tools key kind owner label)
                           when row collect row)))))
    (if (null rows)
        "[no units]"
        (let* ((n (length rows))
               (max (unit-tools-max-output tools))
               (limit (min n (unit-tools-max-results tools)))
               (all (%join-lines (cons +unit-list-header+ rows))))
          (if (and (= limit n) (<= (length all) max))
              all
              (flet ((footer (shown)
                       (format nil "[truncated: showing ~d of ~d units; filter by kind, state, owner or label]"
                               shown n)))
                (let* ((room (- max (length +unit-list-header+) 1 (length (footer n))))
                       (shown (min limit (%fit-count rows (max 0 room)))))
                  (%cap (%join-lines (append (list +unit-list-header+)
                                             (subseq rows 0 shown)
                                             (list (footer shown))))
                        max))))))))

;;; --- unit_search -----------------------------------------------------------------

(defun %kinds-arg (args)
  "kinds → (values keywords names) in the order given."
  (let ((names (%strings-arg args "kinds")))
    (dolist (n names)
      (unless (member n +unit-search-kinds+ :test #'string=)
        (%input-error "kinds: unknown kind ~s; use ~{~a~^, ~}" n +unit-search-kinds+)))
    (let ((names (remove-duplicates names :test #'string= :from-end t)))
      (values (mapcar (lambda (n) (find n ss:+hit-kinds+ :test #'string-equal)) names)
              names))))

(defun %hit-header (hit i)
  (let ((extent (ss:hit-extent hit))
        (stream (ss:hit-stream hit))
        (time (ss:hit-time hit))
        (kinds (ss:hit-kinds hit)))
    (format nil "[~d] ~a ~a ~a ~a~@[ (stack trace: lines ~a)~]"
            i (ss:hit-ref hit)
            (if stream (string-downcase (symbol-name stream)) "-")
            (if time (%rfc3339-ms time) "-")
            (if kinds (format nil "~{~(~a~)~^,~}" kinds) "-")
            (and extent (format nil "~d-~d" (car extent) (cdr extent))))))

(defun %hit-block (index hit i context)
  "→ (values block short): the hit's header and excerpt; SHORT is the header and
the hit line alone."
  (let ((header (%hit-header hit i)))
    (multiple-value-bind (records gaps) (ss:hit-excerpt index hit :context context)
      (values (%join-lines (cons header (mapcar #'car (%merged-lines records gaps
                                                                     :mark-line (ss:hit-line hit)))))
              (%join-lines (list header (%log-line ">" (ss:hit-line hit) (ss:hit-text hit))))))))

(defun %search-text (tools hits total context)
  "Hit blocks separated by an empty line, an empty line, the summary; within
MAX-OUTPUT."
  (let* ((index (unit-tools-index tools))
         (max (unit-tools-max-output tools))
         (pairs (loop for h in hits for i from 1
                      collect (multiple-value-list (%hit-block index h i context))))
         (blocks (mapcar #'first pairs)))
    (flet ((summary (shown)
             (if (= shown total)
                 (format nil "[~d result~:p]" total)
                 (format nil "[showing ~d of ~d results; raise k (max ~d), narrow the query, units, kinds or since, or unit_read a ref]"
                         shown total (unit-tools-max-k tools))))
           (assemble (parts summary)
             (format nil "~{~a~%~%~}~a" parts summary)))
      (let ((all (assemble blocks (summary (length blocks)))))
        (if (and (= (length blocks) total) (<= (length all) max))
            all
            (let* ((reserve (+ 2 (length (summary (max 0 (1- total))))))
                   (used 0)
                   (kept (loop for b in blocks
                               for add = (if (zerop used) (length b) (+ 2 (length b)))
                               while (<= (+ used add reserve) max)
                               do (incf used add)
                               collect b)))
              (when (and (null kept) pairs
                         (<= (+ (length (second (first pairs))) reserve) max))
                (setf kept (list (second (first pairs)))))
              (%cap (assemble kept (summary (length kept))) max)))))))

(defun %tool-unit-search (tools args)
  (let ((query (%arg args "query" :string :required t)))
    (multiple-value-bind (kinds kind-names) (%kinds-arg args)
      (when (and (%blank-p query) (null kinds))
        (%input-error "an empty query needs kinds: give words to search for, or kinds (~{~a~^, ~})"
                      +unit-search-kinds+))
      (let* ((patterns (%strings-arg args "units"))
             (streams (%streams-arg args))
             (since (let ((s (%arg args "since" :string))) (and s (parse-time-argument s))))
             (until (let ((s (%arg args "until" :string))) (and s (parse-time-argument s))))
             (k (%ranged-arg args "k" 1 (unit-tools-max-k tools) (unit-tools-default-k tools)))
             (context (%ranged-arg args "context" 0 (unit-tools-max-context tools)
                                   (unit-tools-context tools)))
             (keys (remove-if-not (lambda (key)
                                    (or (null patterns)
                                        (some (lambda (p) (agent:permission-glob-match-p p key))
                                              patterns)))
                                  (%visible-keys tools))))
        (flet ((none ()
                 (%cap (format nil "No results for \"~a\"~@[ with kinds ~{~a~^, ~}~] in ~d unit~:p"
                               query kind-names (length keys))
                       (unit-tools-max-output tools))))
          (if (null keys)
              (none)                    ; :UNITS NIL would mean every log
              (multiple-value-bind (hits total)
                  (handler-case
                      (ss:search-logs (unit-tools-index tools) query
                                      :units keys :since since :until until
                                      :streams streams :kinds kinds :k k)
                    (ss:index-closed (c) (error c))
                    (ss:search-error (c)
                      (%input-error "~a; put words in query or give kinds" c)))
                (if (null hits)
                    (none)
                    (%search-text tools hits total context)))))))))

;;; --- unit_read -------------------------------------------------------------------

(defun %tool-unit-read (tools args)
  (let* ((ref (%arg args "ref" :string))
         (unit (%arg args "unit" :string))
         (streams (%streams-arg args)))
    (when (eq (null ref) (null unit))
      (%input-error "give exactly one of ref (from unit_search) or unit (from unit_list)"))
    (multiple-value-bind (key mark)
        (if ref
            (multiple-value-bind (k segment line) (ss:parse-log-ref ref)
              (declare (ignore segment))
              (unless k
                (%input-error "ref ~s is not a ref \"<unit>:<segment>:<line>\"; copy one from a unit_search result"
                              ref))
              (values k line))
            (values unit nil))
      (let* ((v (%resolve-log tools key :given (or ref unit) :arg (if ref "ref" "unit")))
             (start (%ranged-arg args "start_line" 1 most-positive-fixnum
                                 (if mark (max 1 (- mark 10)) 1)))
             (limit (%ranged-arg args "limit" 1 (unit-tools-max-read-lines tools)
                                 (if mark 40 200)))
             (log (%vlog-log v))
             (status (%vlog-status v))
             (max (unit-tools-max-output tools)))
        (if (null log)
            (let* ((head (format nil "unit ~a (~a): no log file; in-memory head and tail:" key status))
                   (body (%no-log-body v (- max (length head) 1))))
              (%cap (if body (format nil "~a~%~a" head body) head) max))
            (multiple-value-bind (records gaps)
                (sup:log-read log :start start :limit limit :streams streams)
              (let ((last (sup:log-last-line log)))
                (if (and (null records) (null gaps))
                    (%cap (format nil "unit ~a (~a): no output from line ~d; the log ends at line ~d"
                                  key status start last)
                          max)
                    (flet ((header (b) (format nil "unit ~a (~a): lines ~d-~d of ~d" key status start b last))
                           (more (b) (format nil "[more: unit_read unit \"~a\" start_line ~d]" key (1+ b))))
                      (let* ((items (%merged-lines records gaps :mark-line mark))
                             (top (max last start (reduce #'max items :key #'cdr :initial-value 0)))
                             (room (- max (length (header top)) 1 (length (more top))))
                             (shown (%fit-count (mapcar #'car items) (max 0 room)))
                             (b (if (plusp shown) (cdr (nth (1- shown) items)) (1- start)))
                             (more-p (sup:log-read log :start (1+ b) :limit 1 :streams streams)))
                        (%cap (%join-lines (append (list (header (max b start)))
                                                   (mapcar #'car (subseq items 0 shown))
                                                   (and more-p (list (more b)))))
                              max)))))))))))

;;; --- unit_tail -------------------------------------------------------------------

(defun %tool-unit-tail (tools args)
  (let* ((unit (%arg args "unit" :string :required t))
         (lines (%ranged-arg args "lines" 1 (unit-tools-max-read-lines tools) 50))
         (after (%ranged-arg args "after_line" 0 most-positive-fixnum nil))
         (wait-ms (%ranged-arg args "wait_ms" 0 (unit-tools-max-wait tools) 0))
         (streams (%streams-arg args))
         (v (%resolve-log tools unit))
         (log (%vlog-log v))
         (phrase (%vlog-phrase v))
         (max (unit-tools-max-output tools)))
    (flet ((footer (tail) (format nil "[unit ~a ~a; ~a]" unit phrase tail)))
      (if (null log)
          (let* ((foot (footer "no log file"))
                 (body (%no-log-body v (- max (length foot) 1))))
            (%cap (if body (format nil "~a~%~a" body foot) foot) max))
          (multiple-value-bind (records gaps ended)
              (if after
                  (multiple-value-bind (rs gs next ended-p)
                      (sup:log-follow log :from (1+ after) :wait (/ wait-ms 1000d0)
                                          :limit lines :streams streams)
                    (declare (ignore next))
                    (values rs gs ended-p))
                  (multiple-value-bind (rs gs) (sup:log-tail log lines :streams streams)
                    (values rs gs nil)))
            (let* ((items (%merged-lines records gaps))
                   (last (sup:log-last-line log))
                   (continue-max (footer (format nil "continue with after_line ~d"
                                                 (max last (or after 0)))))
                   (room (- max (length continue-max)))
                   (shown (%fit-count (mapcar #'car items) (max 0 room) :from-end (null after)))
                   (kept (if after
                             (subseq items 0 shown)
                             (subseq items (- (length items) shown))))
                   (l (cond (kept (cdr (car (last kept))))
                            (after after)
                            (t last)))
                   (end-p (or ended
                              (and (sup:log-closed-p log)
                                   (= shown (length items))
                                   (null (sup:log-read log :start (1+ l) :limit 1 :streams streams))))))
              (%cap (%join-lines (append (mapcar #'car kept)
                                         (list (footer (if end-p
                                                           "end of output"
                                                           (format nil "continue with after_line ~d" l))))))
                    max)))))))

;;; --- unit_control ----------------------------------------------------------------

(defun %stop-unit (unit &rest keys)
  "SUP:STOP-UNIT; a unit surviving the forced stop is abandoned (:KILLED)."
  (handler-bind ((sup:stop-failed #'%abandon-stop))
    (apply #'sup:stop-unit unit keys)))

(defun %tool-unit-control (tools args)
  (let* ((id (%arg args "unit" :string :required t))
         (action (or (%enum-arg args "action" +unit-control-actions+)
                     (%input-error "action is required: one of ~{~a~^, ~}" +unit-control-actions+)))
         (v (%resolve-log tools id))
         (u (%vlog-unit v)))
    (when (%vlog-previous-p v)
      (%input-error "~a is a log from a previous session; it cannot be controlled (unit_read and unit_tail can read it)"
                    id))
    (flet ((last-line () (let ((log (%vlog-log v))) (if log (sup:log-last-line log) 0))))
      (cond
        ((string= action "stop")
         (if (sup:unit-finished-p u)
             (format nil "~a had already finished: ~a" id (%status-word u))
             (progn (%stop-unit u)
                    (format nil "Stopped ~a: ~a" id (%status-word u)))))
        ((string= action "restart")
         (let ((before (last-line)))
           (handler-case (handler-bind ((sup:stop-failed #'%abandon-stop))
                           (sup:restart-unit u))
             (sup:unit-start-error (c)
               (error 'agent:tool-unavailable
                      :message (format nil "~a could not restart: ~a" id c)))
             (sup:supervisor-error (c)
               (error 'agent:tool-unavailable
                      :message (format nil "~a could not restart: ~a" id c))))
           (format nil "Restarted ~a: running; its output continues after line ~d (unit_tail after_line ~d)"
                   id before before)))
        (t
         (let ((input (or (%arg args "input" :string)
                          (%input-error "input is required for action input: the text to write to ~a's stdin"
                                        id)))
               (eof (%arg args "eof" :boolean))
               (before (last-line)))
           (handler-case (progn (sup:send-unit-input u input)
                                (when eof (sup:close-unit-input u)))
             (sup:unit-not-running ()
               (%input-error "~a is not running (~a); restart it first with action restart"
                             id (%status-word u)))
             (sup:unit-input-closed ()
               (%input-error "~a's stdin is closed; restart it to send input" id))
             (sup:executor-unsupported ()
               (%input-error "~a does not take input (a thread unit); stop or restart it instead" id)))
           (format nil "Sent ~d character~:p to ~a~:[~;, then closed its stdin~]; read the reply with unit_tail after_line ~d"
                   (length input) id eof before)))))))

;;; --- dispatch --------------------------------------------------------------------

(defparameter +unit-tool-handlers+
  '(("unit_list" . %tool-unit-list) ("unit_search" . %tool-unit-search)
    ("unit_read" . %tool-unit-read) ("unit_tail" . %tool-unit-tail)
    ("unit_control" . %tool-unit-control)))

(defun %run-unit-tool (tools name args)
  "Dispatch NAME (unit_list, unit_search, unit_read, unit_tail, unit_control).
An index closed under a running call → TOOL-UNAVAILABLE."
  (handler-case (funcall (cdr (assoc name +unit-tool-handlers+ :test #'string=)) tools args)
    (ss:index-closed ()
      (error 'agent:tool-unavailable
             :message (format nil "~a cannot run: this unit-tools source is closed" name)))))

;;; --- shell tools under a supervisor (MAKE-SHELL-TOOLS :SUPERVISOR) ---------------
;;;
;;; Every bash command is a :SHELL-JOB unit owned by SHELL-TOOLS-SCOPE. A
;;; foreground command gets EOF on stdin at once and is not a job; a background
;;; one (run_in_background, or the RUN-IN-BACKGROUND restart) is a %UNIT-JOB on
;;; %SHELL-JOBS, so the H2c job registry, limit check, status and list texts run
;;; unchanged on it: its SHELL-JOB-STATUS / -EXIT-CODE / -SIGNAL are computed
;;; from the unit. job_output reads the unit log after a per-job line cursor.

(defparameter +unit-drain-wait+ 5
  "Seconds a read of a finished unit waits for its log's end record.")

(defclass %unit-job (shell-job)
  ((cursor :initform 0 :accessor %unit-job-cursor
           :documentation "The last log line job_output returned.")
   (text-cursor :initform 0 :accessor %unit-job-text-cursor
                :documentation "Without a log file: characters of UNIT-OUTPUT returned.")
   (lock :initform (bt2:make-lock :name "unit-job") :reader %unit-job-lock
         :documentation "One job_output read at a time, so reads never share a chunk."))
  (:documentation "A background bash command under a supervisor: ID is the unit
id, UNIT the unit."))

(defmethod shell-job-status ((job %unit-job))
  (ecase (sup:unit-state (shell-job-unit job))
    ((:starting :running) :running)
    ((:exited :failed) :exited)
    (:killed :killed)))

(defmethod shell-job-exit-code ((job %unit-job))
  (and (eq :exited (shell-job-status job)) (sup:unit-exit-code (shell-job-unit job))))

(defmethod shell-job-signal ((job %unit-job))
  (let ((u (shell-job-unit job)))
    (and (eq :exited (shell-job-status job)) (null (sup:unit-exit-code u)) (sup:unit-signal u))))

(defun %stop-unit-quietly (unit)
  "Stop UNIT (its process tree); a no-op on a finished unit."
  (%stop-unit unit :grace 1))

(defun %start-shell-unit (tools command directory)
  "→ a running :SHELL-JOB unit for COMMAND in DIRECTORY. The shell missing or
failing to launch, or the source closed → TOOL-UNAVAILABLE."
  (let ((program (first (shell-tools-shell tools))))
    (when (and (find-if (lambda (c) (find c "/\\")) program)
               (null (uiop:probe-file* (uiop:parse-native-namestring program))))
      (%cannot-start tools "no such file")))
  (or (handler-case
          (sup:start-unit (shell-tools-supervisor tools) :shell-job
                          :command (append (shell-tools-shell tools) (list command))
                          :owner (shell-tools-scope tools)
                          :labels (list :tool "bash")
                          :cwd directory
                          :env (%shell-overrides tools)
                          :restart :never)
        (sup:scope-closed () (%closed-error "bash"))
        (sup:supervisor-closed () (%closed-error "bash"))
        (sup:unit-start-error (c) (%cannot-start tools c))
        (sup:unit-limit-reached (c)
          (error 'agent:tool-unavailable :message (format nil "bash cannot start: ~a" c))))
      (error 'agent:tool-unavailable
             :message "bash cannot start: the supervisor refused the unit")))

(defun %records-octets (records)
  "The output octets of RECORDS, LF after each record a line feed ended."
  (let ((out (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (r records out)
      (loop for b across (sup:log-record-octets r) do (vector-push-extend b out))
      (unless (sup:log-record-continued-p r)
        (vector-push-extend 10 out)))))

(defun %gap-bytes (gaps)
  (reduce #'+ gaps :key (lambda (g) (or (sup:log-gap-bytes g) 0))))

(defun %read-unit-records (log from &key drain)
  "Output records of LOG from line FROM → (values records gap-bytes next-line).
DRAIN (the unit has finished): wait up to +UNIT-DRAIN-WAIT+ seconds for the
log's end record, so every line the pumps still carry is read."
  (if (not drain)
      (multiple-value-bind (records gaps next) (sup:log-read log :start from)
        (values records (%gap-bytes gaps) next))
      (let ((deadline (+ (%now) +unit-drain-wait+))
            (acc '()) (bytes 0))
        (loop
          (multiple-value-bind (records gaps next ended)
              (sup:log-follow log :from from :wait (max 0 (- deadline (%now))))
            (setf acc (nconc acc records) from next)
            (incf bytes (%gap-bytes gaps))
            (when (or ended (null records))
              (return (values acc bytes from))))))))

(defun %truncate-in-unit (text max id)
  "TRUNCATE-SHELL-OUTPUT's head + tail cut of TEXT, with the omitted-characters
line naming unit ID, everything within MAX characters."
  (let ((n (length text)))
    (flet ((notice (omitted)
             (format nil "[truncated: ~d characters omitted; the full output is in unit ~a: use unit_read or unit_search]"
                     omitted id)))
      (if (<= n max)
          text
          (let* ((room (max 0 (- max (length (notice n)) 2)))
                 (head-max (floor room 2))
                 (lf (position #\Newline text :end head-max :from-end t))
                 (head-end (if lf (1+ lf) head-max))
                 (tail-start (- n (- room head-end))))
            (unless (and (> tail-start 0) (char= (char text (1- tail-start)) #\Newline))
              (let ((next (position #\Newline text :start tail-start)))
                (when next (setf tail-start (1+ next)))))
            (%cap (string-right-trim
                   '(#\Newline)
                   (with-output-to-string (s)
                     (write-string text s :end head-end)
                     (when (and (> head-end 0) (char/= (char text (1- head-end)) #\Newline))
                       (write-char #\Newline s))
                     (write-line (notice (- tail-start head-end)) s)
                     (write-string text s :start tail-start)))
                  max))))))

(defun %supervised-output (tools unit &key drain)
  "UNIT's output so far as a foreground bash result shows it."
  (let* ((log (%unit-log-or-nil unit))
         (raw (if log
                  (%records-octets (values (%read-unit-records log 1 :drain drain)))
                  (sup:unit-output unit))))
    (%truncate-in-unit (string-right-trim '(#\Newline) (clean-shell-output raw))
                       (shell-tools-max-output tools) (sup:unit-id unit))))

(defun %unit-exit-line (unit)
  (let ((code (sup:unit-exit-code unit))
        (signal (sup:unit-signal unit)))
    (cond ((and (eq :killed (sup:unit-state unit)) signal)
           (format nil "[killed by signal ~a]" (%signal-name signal)))
          (code (format nil "[exit code ~a]" code))
          (signal (format nil "[killed by signal ~a]" (%signal-name signal)))
          (t "[killed]"))))

(defun %register-unit-job (tools unit command &key check-limit reservation)
  "%REGISTER-JOB for a unit: the job id is the unit id."
  (bt2:with-lock-held ((%shell-lock tools))
    (when (and reservation (car reservation))
      (decf (%shell-reserved tools))
      (setf (car reservation) nil))
    (when (shell-tools-closed-p tools)
      (%closed-error "bash"))
    (when check-limit
      (%check-job-limit tools))
    (let ((job (make-instance '%unit-job :id (sup:unit-id unit) :command command :unit unit)))
      (push job (%shell-jobs tools))
      job)))

(defun %sup-bash-foreground (tools command directory timeout-ms)
  "%BASH-FOREGROUND on a unit (I-restart, I-kill as there)."
  (let ((unit (%start-shell-unit tools command directory))
        (kept nil))
    (unwind-protect
         (progn
           (sup:close-unit-input unit)      ; a stdin reader sees end of file at once
           (loop
             (when (sup:wait-unit unit :timeout (/ timeout-ms 1000d0))
               (when (and (eq :killed (sup:unit-state unit)) (shell-tools-closed-p tools))
                 ;; CLOSE-SHELL-TOOLS closed the scope under this run.
                 (%closed-error "bash"))
               (return (%with-status (%supervised-output tools unit :drain t)
                                     (%unit-exit-line unit))))
             ;; I-restart: signalled here, in the caller's thread, with the
             ;; command still running; the restarts recover in this frame.
             (restart-case
                 (let ((so-far (%supervised-output tools unit)))
                   (error 'shell-timeout :command command :timeout timeout-ms :output so-far
                                         :message (%timeout-message timeout-ms so-far)))
               (continue ()
                 :report (lambda (s) (format s "Wait another ~d ms for the command." timeout-ms))
                 nil)
               (run-in-background ()
                 :report "Keep the command running as a background job; the call returns its job id."
                 (let ((job (%register-unit-job tools unit command :check-limit t)))
                   (setf kept t)
                   (return (%started-text job)))))))
      ;; I-kill: a no-op once the unit finished.
      (unless kept
        (%stop-unit-quietly unit)))))

(defun %sup-bash-background (tools command directory)
  (bt2:with-lock-held ((%shell-lock tools))
    (%check-job-limit tools)
    (incf (%shell-reserved tools)))
  (let ((unit nil) (reservation (list t)) (registered nil))
    (unwind-protect
         (progn
           (setf unit (%start-shell-unit tools command directory))
           (let ((job (%register-unit-job tools unit command :reservation reservation)))
             (setf registered t)
             (%started-text job)))
      (when (car reservation)
        (bt2:with-lock-held ((%shell-lock tools))
          (decf (%shell-reserved tools))))
      (when (and unit (not registered))
        (%stop-unit-quietly unit)))))

(defun %sup-tool-bash (tools args)
  (let* ((command (%arg args "command" :string :required t))
         (timeout (%ranged-arg args "timeout_ms" 1 (shell-tools-max-timeout tools)
                               (shell-tools-default-timeout tools)))
         (workdir (%arg args "workdir" :string))
         (background (%arg args "run_in_background" :boolean)))
    (%arg args "description" :string)       ; accepted, unused (UI text)
    (when (%blank-p command)
      (%input-error "command is empty; give the command line to run"))
    (let ((directory (%workdir-directory tools workdir)))
      (if background
          (%sup-bash-background tools command directory)
          (%sup-bash-foreground tools command directory timeout)))))

(defun %unit-job-take (tools job finished)
  "Output of JOB's unit since the last take → (values text dropped)."
  (let* ((u (shell-job-unit job))
         (log (%unit-log-or-nil u)))
    (multiple-value-bind (text dropped)
        (if log
            (multiple-value-bind (records gap-bytes next)
                (%read-unit-records log (1+ (%unit-job-cursor job)) :drain finished)
              (setf (%unit-job-cursor job) (max (%unit-job-cursor job) (1- next)))
              (values (values (%utf8-decode-lenient (%records-octets records))) gap-bytes))
            (let* ((all (sup:unit-output u))
                   (start (min (%unit-job-text-cursor job) (length all))))
              (setf (%unit-job-text-cursor job) (length all))
              (values (subseq all start) 0)))
      ;; The oldest unread characters beyond max-job-buffer, cut forward to a
      ;; line start, count as dropped (as H2c's buffer).
      (let ((cap (shell-tools-max-job-buffer tools)))
        (if (<= (length text) cap)
            (values text dropped)
            (let* ((cut (- (length text) cap))
                   (lf (position #\Newline text :start cut)))
              (when (and lf (< (1+ lf) (length text)))
                (setf cut (1+ lf)))
              (values (subseq text cut) (+ dropped cut))))))))

(defun %unit-job-report (tools job wait-ms)
  "What job_output returns for JOB after waiting up to WAIT-MS for it. The
final status line only follows a read that began after the unit finished, so
it never comes before output still unread."
  (let ((u (shell-job-unit job)))
    (when (plusp wait-ms)
      (sup:wait-unit u :timeout (/ wait-ms 1000d0)))
    (bt2:with-lock-held ((%unit-job-lock job))
      (let ((finished (sup:unit-finished-p u)))
        (multiple-value-bind (text dropped) (%unit-job-take tools job finished)
          (%with-status (%output-text tools text dropped)
                        (if finished
                            (%status-line job)
                            (format nil "[job ~a running]" (shell-job-id job)))))))))

(defun %sup-tool-job-output (tools args)
  (let* ((id (%arg args "job_id" :string :required t))
         (wait (%ranged-arg args "wait_ms" 0 (shell-tools-max-timeout tools) 0)))
    (%unit-job-report tools (%find-job tools id) wait)))

(defun %sup-tool-job-input (tools args)
  (let* ((id (%arg args "job_id" :string :required t))
         (input (%arg args "input" :string :required t))
         (wait (%ranged-arg args "wait_ms" 0 (shell-tools-max-timeout tools) 0))
         (job (%find-job tools id)))
    (unless (eq (shell-job-status job) :running)
      (%not-running job))
    (handler-case (sup:send-unit-input (shell-job-unit job) input)
      (sup:unit-not-running () (%not-running job))
      (sup:unit-input-closed () (%not-running job)))
    (%unit-job-report tools job wait)))

(defun %sup-tool-job-kill (tools args)
  (let* ((id (%arg args "job_id" :string :required t))
         (job (%find-job tools id)))
    (when (eq (shell-job-status job) :running)
      (%stop-unit-quietly (shell-job-unit job)))
    (%unit-job-report tools job 0)))

(defparameter +supervised-shell-handlers+
  '(("bash" . %sup-tool-bash) ("job_output" . %sup-tool-job-output)
    ("job_input" . %sup-tool-job-input) ("job_kill" . %sup-tool-job-kill)
    ("job_list" . %tool-job-list)))

(defun %call-supervised-shell-tool (tools name args)
  "bash / job_output / job_input / job_kill / job_list when SHELL-TOOLS-SUPERVISOR
is set: every command a :SHELL-JOB unit, the job tools views over unit logs."
  (funcall (cdr (assoc name +supervised-shell-handlers+ :test #'string=)) tools args))

(defun %close-supervised-shell-tools (tools)
  "Close the source's scope (its running units stop) and refuse later calls.
Idempotent. → NIL."
  (when (bt2:with-lock-held ((%shell-lock tools))
          (unless (shell-tools-closed-p tools)
            (setf (slot-value tools 'closed-p) t)))
    (handler-bind ((sup:stop-failed #'%abandon-stop))
      (sup:close-scope (shell-tools-scope tools))))
  nil)
