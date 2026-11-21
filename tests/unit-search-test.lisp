(in-package #:agent-tools/tests)

;;; H8c unit_search: hit blocks with refs, kinds, the summary line, the output
;;; cap, filters and argument errors.

(defun build-and-trace (sup)
  "→ (values build-unit trace-unit): the short build log on stdout, the dev log
with its JS stack on stderr."
  (values (thread-lines sup +build-lines+)
          (thread-lines sup +trace-lines+ :stderr-p #'stderr-line-p)))

(defun hit-header (i ref stream unit-record kinds &optional extent)
  (format nil "[~d] ~a ~a ~a ~a~@[ (stack trace: lines ~{~d-~d~})~]"
          i ref stream (rfc3339-ms (sup:log-record-wall-time unit-record)) kinds extent))

(deftest unit-search-hit-block
  (with-units (tools sup)
    (let* ((build (build-and-trace sup))
           (id (sup:unit-id build))
           (r (record-of build (fourth +build-lines+)))
           (line (sup:log-record-line r))
           (result (ucall tools "unit_search" "query" "undeclared identifier tokn"))
           (lines (result-lines result)))
      (ok (equal (hit-header 1 (ref-of id r) "stdout" r "error") (first lines)) "header line")
      (ok (equal (list (log-line " " (- line 2) (second +build-lines+))
                       (log-line " " (- line 1) (third +build-lines+))
                       (log-line ">" line (fourth +build-lines+))
                       (log-line " " (+ line 1) (fifth +build-lines+))
                       (log-line " " (+ line 2) (sixth +build-lines+)))
                 (subseq lines 1 6))
          "two lines of context on each side, the hit marked")
      (ok (equal '("" "[1 result]") (last lines 2)) "a blank line, then the summary"))))

(deftest unit-search-stack-trace-and-kinds
  (with-units (tools sup)
    (multiple-value-bind (build trace) (build-and-trace sup)
      (declare (ignore build))
      (let* ((id (sup:unit-id trace))
             (head (record-of trace (second +trace-lines+)))
             (last (record-of trace (fifth +trace-lines+)))
             (result (ucall tools "unit_search" "query" "" "kinds" #("stack-trace"))))
        (ok (equal (hit-header 1 (ref-of id head) "stderr" head "error,stack-trace"
                               (list (sup:log-record-line head) (sup:log-record-line last)))
                   (first (result-lines result))))
        (ok (equal "[1 result]" (car (last (result-lines result)))))))))

(deftest unit-search-summary-and-more
  (with-units (tools sup)
    (build-and-trace sup)
    (ok (equal "No results for \"zzzz-nothing\" in 2 units"
               (ucall tools "unit_search" "query" "zzzz-nothing")))
    (ok (equal "No results for \"\" with kinds test-failure in 2 units"
               (ucall tools "unit_search" "query" "" "kinds" #("test-failure"))))
    (let ((result (ucall tools "unit_search" "query" "" "kinds" #("error") "k" 1)))
      (ok (equal "[showing 1 of 4 results; raise k (max 50), narrow the query, units, kinds or since, or unit_read a ref]"
                 (car (last (result-lines result))))
          "build: 2 error lines + make's Error 1; trace: the TypeError")
      (ok (has "[1] " result))
      (ok (not (has "[2] " result))))))

(deftest unit-search-filters
  (with-units (tools sup)
    (multiple-value-bind (build trace) (build-and-trace sup)
      (flet ((refs-units (&rest kv)
               (let ((r (apply #'ucall tools "unit_search" kv)))
                 (loop for l in (result-lines r)
                       when (and (> (length l) 4) (char= #\[ (char l 0)) (digit-char-p (char l 1)) (find #\: l))
                         collect (subseq l (1+ (position #\Space l)) (position #\: l))))))
        (ok (equal (list (sup:unit-id build))
                   (remove-duplicates (refs-units "query" "error" "units" (vector (sup:unit-id build)) "k" 50)
                                      :test #'equal)))
        (ok (equal (list (sup:unit-id trace))
                   (remove-duplicates (refs-units "query" "roles" "streams" #("stderr") "k" 50) :test #'equal)))
        (ok (refs-units "query" "error" "since" "1h") "the last hour holds everything")
        (ok (null (refs-units "query" "error" "until" "2000-01-01T00:00:00Z")))
        (ok (equal "No results for \"error\" in 0 units"
                   (ucall tools "unit_search" "query" "error" "units" #("nope-*"))))))))

(deftest unit-search-output-cap
  (with-units (tools sup :tool-keys (:max-output 600))
    (thread-lines sup (loop for i below 200 collect (format nil "cc -c -o obj/file~3,'0d.o src/file~3,'0d.c error" i i)))
    (let ((result (ucall tools "unit_search" "query" "" "kinds" #("error") "k" 50)))
      (ok (<= (length result) 600) "never over max-output")
      (ok (has "[showing " result))
      (ok (has " of 200 results;" result)))))

(deftest unit-search-long-lines
  (with-units (tools sup)
    (let* ((long (format nil "marker-long ~a" (make-string 1988 :initial-element #\x)))
           (u (thread-lines sup (list long)))
           (line (sup:log-record-line (record-of u long)))
           (result (ucall tools "unit_search" "query" "marker-long")))
      (ok (= 2000 (length long)))
      (ok (member (log-line ">" line (format nil "~a… [+1500 chars]" (subseq long 0 500)))
                  (result-lines result) :test #'equal)
          "a line is cut at 500 characters"))))

(deftest unit-search-argument-errors
  (with-units (tools sup)
    (build-and-trace sup)
    (flet ((input-error (&rest kv)
             (let ((c (failure (apply #'ucall tools "unit_search" kv))))
               (and (typep c 'a:tool-input-error) (message c)))))
      (ok (has "kinds" (input-error "query" "")) "an empty query needs kinds")
      (ok (input-error))
      (ok (has "bogus" (input-error "query" "x" "kinds" #("bogus"))))
      (ok (has "k" (input-error "query" "x" "k" 0)))
      (ok (input-error "query" "x" "k" 51))
      (ok (input-error "query" "x" "context" 11))
      (ok (input-error "query" "x" "streams" #("stdin")))
      (ok (has "soon" (input-error "query" "x" "since" "soon"))))))
