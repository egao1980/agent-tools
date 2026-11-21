(in-package #:agent-tools/tests)

;;; H8c unit_read: by ref, by line range, gaps, the [more] line, the cap, units
;;; without a log, errors.

(defun numbered-texts (n) (loop for i from 1 to n collect (format nil "text line ~3,'0d" i)))

(deftest unit-read-by-ref
  (with-units (tools sup)
    (let* ((u (thread-lines sup (numbered-texts 100)))
           (id (sup:unit-id u))
           (r (record-of u "text line 050"))
           (l (sup:log-record-line r))
           (last (sup:log-last-line u))
           (result (ucall tools "unit_read" "ref" (ref-of id r)))
           (lines (result-lines result)))
      (ok (= 51 l) "line 1 is the segment header")
      (ok (equal (format nil "unit ~a (exited): lines ~d-~d of ~d" id (- l 10) (+ l 29) last)
                 (first lines)))
      (ok (equal (log-line " " (- l 10) "text line 040") (second lines)))
      (ok (member (log-line ">" l "text line 050") lines :test #'equal) "the ref line is marked")
      (ok (= 42 (length lines)) "header, 40 lines, [more]")
      (ok (equal (format nil "[more: unit_read unit \"~a\" start_line ~d]" id (+ l 30))
                 (car (last lines)))))))

(deftest unit-read-by-range
  (with-units (tools sup)
    (let* ((u (thread-lines sup (numbered-texts 20)))
           (id (sup:unit-id u))
           (last (sup:log-last-line u)))
      (ok (equal (format nil "unit ~a (exited): lines 2-4 of ~d~%~a~%~a~%~a~%[more: unit_read unit \"~a\" start_line 5]"
                         id last (log-line " " 2 "text line 001") (log-line " " 3 "text line 002")
                         (log-line " " 4 "text line 003") id)
                 (ucall tools "unit_read" "unit" id "start_line" 2 "limit" 3)))
      (ok (equal (format nil "unit ~a (exited): lines 20-21 of ~d~%~a~%~a"
                         id last (log-line " " 20 "text line 019") (log-line " " 21 "text line 020"))
                 (ucall tools "unit_read" "unit" id "start_line" 20))
          "no [more] line at the end of the output")
      (ok (equal (format nil "unit ~a (exited): no output from line 999; the log ends at line ~d" id last)
                 (ucall tools "unit_read" "unit" id "start_line" 999))))))

(deftest unit-read-shows-gaps
  (with-units (tools sup :supervisor-keys (:log-max-record 256))
    (let* ((u (sup:start-unit sup :shell-job
                              :function (lambda (u) (declare (ignore u))
                                          (dolist (l (numbered-texts 400)) (write-line l)) :done)
                              :log-segment-size 1024 :log-segments 2))
           (id (sup:unit-id (settle u))))
      (multiple-value-bind (records gaps) (sup:log-read u)
        (let ((gap (first gaps)))
          (ok gap "the oldest segments were dropped")
          (let ((lines (result-lines (ucall tools "unit_read" "unit" id "limit" 3))))
            (ok (equal (format nil "[gap: lines ~d-~d not in the log (rotated)]"
                               (sup:log-gap-first-line gap) (sup:log-gap-last-line gap))
                       (second lines))
                "the gap comes first, where the dropped lines were")
            (ok (equal (log-line " " (sup:log-record-line (first records)) (sup:log-record-text (first records)))
                       (third lines)))))))))

(deftest unit-read-cap
  (with-units (tools sup :tool-keys (:max-output 300))
    (let* ((u (thread-lines sup (numbered-texts 100)))
           (result (ucall tools "unit_read" "unit" (sup:unit-id u))))
      (ok (<= (length result) 300))
      (ok (has (format nil "[more: unit_read unit \"~a\" start_line " (sup:unit-id u))
               (car (last (result-lines result))))
          "the [more] line names where to go on"))))

(deftest unit-read-without-log-file
  (with-units (tools sup :state nil)
    (let* ((u (settle (sup:start-unit sup :shell-job :function (lambda (u) (declare (ignore u))
                                                                  (write-line "in memory only") :done))))
           (result (ucall tools "unit_read" "unit" (sup:unit-id u))))
      (ok (equal (format nil "unit ~a (exited): no log file; in-memory head and tail:~%in memory only"
                         (sup:unit-id u))
                 result)))))

(deftest unit-read-errors
  (with-units (tools sup)
    (let ((u (thread-lines sup (numbered-texts 3))))
      (flet ((err (&rest kv) (failure (apply #'ucall tools "unit_read" kv))))
        (ok (typep (err) 'a:tool-input-error) "neither ref nor unit")
        (ok (typep (err "ref" (format nil "~a:1:2" (sup:unit-id u)) "unit" (sup:unit-id u)) 'a:tool-input-error)
            "both")
        (ok (has "ref" (message (err "ref" "not-a-ref"))))
        (ok (typep (err "unit" "shell-job-99") 'unit-not-found))
        (ok (has "unit_list" (message (err "unit" "shell-job-99"))))
        (ok (equal "shell-job-99" (unit-not-found-unit (err "unit" "shell-job-99"))))
        (ok (typep (err "ref" "shell-job-99:1:4") 'unit-not-found))
        (ok (typep (err "unit" (sup:unit-id u) "limit" 2001) 'a:tool-input-error))
        (ok (typep (err "unit" (sup:unit-id u) "start_line" 0) 'a:tool-input-error))))))
