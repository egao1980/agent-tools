(in-package #:agent-tools/tests)

;;; CLEAN-SHELL-OUTPUT and TRUNCATE-SHELL-OUTPUT (pure, every OS).

(defun esc (&rest parts)
  "PARTS joined, with :ESC, :BEL, :CR, :LF, :BS and :NUL as those characters."
  (with-output-to-string (s)
    (dolist (p parts)
      (case p
        (:esc (write-char (code-char 27) s))
        (:bel (write-char (code-char 7) s))
        (:cr (write-char #\Return s))
        (:lf (write-char #\Linefeed s))
        (:bs (write-char (code-char 8) s))
        (:nul (write-char (code-char 0) s))
        (t (write-string p s))))))

(defun u8 (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(deftest clean-decodes-octets
  (ok (equal "hi" (clean-shell-output (u8 104 105))))
  (ok (equal (format nil "caf~c~%" (code-char #xE9))
             (clean-shell-output (u8 99 97 102 #xC3 #xA9 10))))
  (ok (equal (format nil "hi~c~%" (code-char #xFFFD))
             (clean-shell-output (u8 104 105 255 10)))
      "an invalid byte becomes U+FFFD")
  (ok (equal "" (clean-shell-output (u8))))
  (ok (equal "" (clean-shell-output ""))))

(deftest clean-line-endings
  (ok (equal (nl "a" "b") (clean-shell-output (crlf "a" "b"))))
  (ok (equal (nl "a" "b") (clean-shell-output (u8 97 13 10 98 13 10))))
  (ok (equal (nl "100%") (clean-shell-output (esc "10%" :cr "50%" :cr "100%" :lf)))
      "a lone CR overwrites the line")
  (ok (equal "b" (clean-shell-output (esc "a" :cr "b"))))
  (ok (equal (nl "x" "done") (clean-shell-output (esc "x" :lf "50%" :cr "done" :lf)))))

(deftest clean-escape-sequences
  (ok (equal "red plain" (clean-shell-output (esc :esc "[31mred" :esc "[0m plain"))))
  (ok (equal "ok" (clean-shell-output (esc :esc "[1;32mok" :esc "[m"))))
  (ok (equal "100%" (clean-shell-output (esc :esc "[2K" :cr "100%"))))
  (ok (equal "x" (clean-shell-output (esc :esc "]0;title" :bel "x"))) "OSC ended by BEL")
  (ok (equal "link" (clean-shell-output
                     (esc :esc "]8;;http://e.x" :esc "\\link" :esc "]8;;" :esc "\\")))
      "OSC ended by ST")
  (ok (equal "ab" (clean-shell-output (esc "a" :esc "(B" "b"))) "ESC with an intermediate")
  (ok (equal "ab" (clean-shell-output (esc "a" :esc "=b"))))
  (ok (equal "a" (clean-shell-output (esc "a" :esc)))))

(deftest clean-control-characters
  (ok (equal (format nil "abcd~ce" #\Tab)
             (clean-shell-output (esc "a" :bs "b" :bel "c" :nul "d" (string #\Tab) "e"))))
  (ok (equal (nl "a" "" "b") (clean-shell-output (nl "a" "" "b")))))

(defparameter +omitted-tail+
  "characters omitted; redirect the output to a file and use read or grep on it]")

(defun omitted (n)
  (format nil "[truncated: ~d ~a" n +omitted-tail+))

(deftest truncate-short-output-unchanged
  (ok (equal "abc" (truncate-shell-output "abc" 3)))
  (ok (equal "" (truncate-shell-output "" 5)))
  (ok (equal (nl "a" "b") (truncate-shell-output (nl "a" "b") 100))))

(deftest truncate-keeps-head-and-tail-lines
  (ok (equal (format nil "aaaa~%~a~%dddd~%" (omitted 10))
             (truncate-shell-output (nl "aaaa" "bbbb" "cccc" "dddd") 10)))
  (ok (equal (format nil "line1~%~a~%line5~%" (omitted 18))
             (truncate-shell-output (nl "line1" "line2" "line3" "line4" "line5") 14)))
  (ok (equal (format nil "ab~%~a~%ij" (omitted 6))
             (truncate-shell-output "abcdefghij" 4))
      "no line breaks: cut inside the line"))

(deftest truncate-bounds-the-kept-text
  (let* ((text (numbered-lines 1000))
         (r (truncate-shell-output text 500))
         (notice (find-if (lambda (l) (has "[truncated: " l)) (lines r)))
         (kept (- (length r) (length notice) 1)))
    (ok (has "[truncated: " r))
    (ok (<= kept 500))
    (ok (string= "line1" r :end2 5))
    (ok (equal "line1000" (last-line r)))))
