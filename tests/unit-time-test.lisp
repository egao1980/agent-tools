(in-package #:agent-tools/tests)

;;; H8c since / until arguments.

(defparameter +noon-ms+ 1795262400000 "2026-11-21T12:00:00Z in Unix ms.")

(deftest-parametrize time-argument-table
    ((text expected)
     ("30s" (- +noon-ms+ 30000))
     ("10m" (- +noon-ms+ 600000))
     ("2h" (- +noon-ms+ 7200000))
     ("1d" (- +noon-ms+ 86400000))
     ("2026-11-21T12:00:00Z" +noon-ms+)
     ("2026-11-21T12:00:00.250Z" (+ +noon-ms+ 250))
     ("2026-11-21T14:00:00+02:00" +noon-ms+)
     ("2026-11-21T07:30:00-04:30" +noon-ms+)
     ("2026-11-21 12:00:00Z" +noon-ms+))
  (ok (eql expected (parse-time-argument text :now +noon-ms+)) text))

(deftest time-argument-errors
  (dolist (bad '("" "yesterday" "10x" "-5m" "5" "m" "2026-11-21" "2026-13-01T00:00:00Z" "12:00"))
    (let ((c (failure (parse-time-argument bad :now +noon-ms+))))
      (ok (typep c 'a:tool-input-error) bad)
      (ok (has bad (message c)) "the message names the value"))))

(deftest time-argument-default-now
  (let* ((before (* 1000 (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0))))
         (v (parse-time-argument "1m")))
    (ok (<= (- before 60000 2000) v (+ before 2000)) "relative to the current time")))
