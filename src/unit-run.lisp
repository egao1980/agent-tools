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

(defun parse-time-argument (string &key now)
  "→ Unix epoch milliseconds for a since / until argument: an RFC 3339 time
(\"2026-11-21T12:00:00Z\", fraction and ±hh:mm offset allowed, a space instead of
T allowed) or a duration ago: a positive integer and one unit of s, m, h, d
(\"90s\", \"10m\", \"2h\", \"1d\"), relative to NOW (Unix ms; default the current
time). Anything else → AI-AGENT-PROTOCOL:TOOL-INPUT-ERROR naming the value."
  (declare (ignore string now))
  (%h8c-pending "parse-time-argument"))

(defun %run-unit-tool (tools name args)
  "Dispatch NAME (unit_list, unit_search, unit_read, unit_tail, unit_control)."
  (declare (ignore tools args))
  (%h8c-pending (format nil "unit tool ~a" name)))

;;; Shell tools under a supervisor (MAKE-SHELL-TOOLS :SUPERVISOR).

(defun %call-supervised-shell-tool (tools name args)
  "bash / job_output / job_input / job_kill / job_list when SHELL-TOOLS-SUPERVISOR
is set: every command a :SHELL-JOB unit, the job tools views over unit logs."
  (declare (ignore tools args))
  (%h8c-pending (format nil "~a under a supervisor" name)))

(defun %close-supervised-shell-tools (tools)
  "Close the source's scope (its running units stop) and refuse later calls.
Idempotent. → NIL."
  (declare (ignore tools))
  (%h8c-pending "close-shell-tools under a supervisor"))
