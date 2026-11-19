(in-package #:agent-tools)

;;; Output text for the model (H2c, pure): lenient UTF-8 decoding, cleaning
;;; (CR LF, ANSI escapes, progress-bar CRs, control characters) and head +
;;; tail truncation. The job buffers (src/shell-run.lisp) decode with
;;; %UTF8-DECODE-LENIENT as octets arrive and clean with CLEAN-SHELL-OUTPUT
;;; when a chunk is taken; %TAKE-END keeps a take from splitting a CR LF or
;;; an escape sequence.

(defconstant +replacement-char+ (code-char #xFFFD))

(defun %utf8-sequence-length (b0)
  "Bytes a UTF-8 sequence led by B0 takes, or NIL when B0 cannot lead one."
  (cond ((< b0 #x80) 1)
        ((< b0 #xC2) nil)
        ((< b0 #xE0) 2)
        ((< b0 #xF0) 3)
        ((< b0 #xF5) 4)
        (t nil)))

(defun %utf8-decode-lenient (bytes &key (start 0) (end (length bytes)) partial)
  "Decode the octets BYTES[START, END) as UTF-8, with U+FFFD for each byte
that does not belong to a valid sequence. With PARTIAL, a valid but
unfinished sequence at the end is left undecoded.
→ (values string index-after-the-decoded-octets)."
  (let ((out (make-array (- end start) :element-type 'character :fill-pointer 0))
        (i start))
    (loop while (< i end)
          do (let* ((b0 (aref bytes i))
                    (n (%utf8-sequence-length b0)))
               (cond
                 ((eql n 1)
                  (vector-push (code-char b0) out)
                  (incf i))
                 ((null n)
                  (vector-push +replacement-char+ out)
                  (incf i))
                 (t
                  (let ((have 1))
                    (loop while (and (< have n) (< (+ i have) end)
                                     (= (logand (aref bytes (+ i have)) #xC0) #x80))
                          do (incf have))
                    (cond
                      ((and (< have n) (= (+ i have) end) partial)
                       ;; An unfinished sequence at the end: wait for more.
                       (loop-finish))
                      ((< have n)
                       (vector-push +replacement-char+ out)
                       (incf i))
                      (t
                       (let ((code (ecase n
                                     (2 (logior (ash (logand b0 #x1F) 6)
                                                (logand (aref bytes (+ i 1)) #x3F)))
                                     (3 (logior (ash (logand b0 #x0F) 12)
                                                (ash (logand (aref bytes (+ i 1)) #x3F) 6)
                                                (logand (aref bytes (+ i 2)) #x3F)))
                                     (4 (logior (ash (logand b0 #x07) 18)
                                                (ash (logand (aref bytes (+ i 1)) #x3F) 12)
                                                (ash (logand (aref bytes (+ i 2)) #x3F) 6)
                                                (logand (aref bytes (+ i 3)) #x3F))))))
                         (if (or (and (= n 3) (< code #x800))
                                 (and (= n 4) (or (< code #x10000) (> code #x10FFFF)))
                                 (<= #xD800 code #xDFFF))
                             (progn (vector-push +replacement-char+ out)
                                    (incf i))
                             (progn (vector-push (code-char code) out)
                                    (incf i n)))))))))))
    (values (coerce out 'simple-string) i)))

;;; --- escape sequences --------------------------------------------------------

(defun %escape-end (string i end)
  "STRING[I] is ESC. → the index just after its escape sequence, or NIL when
the sequence is not finished before END: CSI ESC [ … final 0x40–0x7E; OSC
ESC ] … BEL or ESC \\; otherwise ESC, intermediates 0x20–0x2F, one final."
  (let ((j (1+ i)))
    (cond
      ((>= j end) nil)
      ((char= (char string j) #\[)
       (loop for k from (1+ j) below end
             when (<= #x40 (char-code (char string k)) #x7E)
               do (return (1+ k))
             finally (return nil)))
      ((char= (char string j) #\])
       (loop for k from (1+ j) below end
             for c = (char string k)
             do (cond ((= (char-code c) 7) (return (1+ k)))
                      ((and (= (char-code c) 27) (< (1+ k) end)
                            (char= (char string (1+ k)) #\\))
                       (return (+ k 2))))
             finally (return nil)))
      (t
       (loop for k from j below end
             unless (<= #x20 (char-code (char string k)) #x2F)
               do (return (1+ k))
             finally (return nil))))))

(defun %take-end (string start end)
  "The end of the part of STRING[START, END) a live job may hand out now:
before an unfinished escape sequence at the end, and before a final CR (it
may be the CR of a CR LF still on its way)."
  (let ((esc (position (code-char 27) string :start start :end end :from-end t)))
    (when (and esc (null (%escape-end string esc end)))
      (setf end esc)))
  (if (and (> end start) (char= (char string (1- end)) #\Return))
      (1- end)
      end))

;;; --- cleaning -------------------------------------------------------------

(defun %control-char-p (c)
  (let ((k (char-code c)))
    (and (or (< k #x20) (= k #x7F) (<= #x80 k #x9F))
         (/= k 9) (/= k 10))))

(defun clean-shell-output (output)
  "→ OUTPUT (an octet vector, decoded as UTF-8 with U+FFFD for each invalid
byte, or a string) as text for the model: CR LF → LF; ANSI escape sequences
removed; in each line only the text after its last remaining CR kept; other
control characters except TAB and LF removed."
  (let* ((text (if (stringp output)
                   output
                   (values (%utf8-decode-lenient output))))
         (n (length text))
         (out (make-string-output-stream))
         (line (make-array 80 :element-type 'character :adjustable t :fill-pointer 0))
         (i 0))
    (flet ((end-line (newline)
             ;; Step 4: only the text after the last CR; step 5: controls.
             (let ((cr (position #\Return line :from-end t)))
               (loop for k from (if cr (1+ cr) 0) below (fill-pointer line)
                     for c = (aref line k)
                     unless (%control-char-p c)
                       do (write-char c out)))
             (when newline (write-char #\Newline out))
             (setf (fill-pointer line) 0)))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond
                   ((= (char-code c) 27)
                    (setf i (or (%escape-end text i n) n)))
                   ((char= c #\Newline)
                    (end-line t)
                    (incf i))
                   ((and (char= c #\Return) (< (1+ i) n) (char= (char text (1+ i)) #\Newline))
                    (end-line t)
                    (incf i 2))
                   (t
                    (vector-push-extend c line)
                    (incf i)))))
      (end-line nil))
    (get-output-stream-string out)))

;;; --- truncation -----------------------------------------------------------

(defun truncate-shell-output (string max)
  "→ STRING when it has at most MAX characters; else its head and tail in
whole lines around a \"[truncated: <n> characters omitted; ...]\" line.
The head is at most ⌊MAX/2⌋ characters cut back to its last LF; the tail is
at most the rest of MAX, starting at a line start (mid-line when the window
holds no line break)."
  (check-type string string)
  (check-type max (integer 0))
  (let ((n (length string)))
    (if (<= n max)
        string
        (let* ((head-max (floor max 2))
               (lf (position #\Newline string :end head-max :from-end t))
               (head-end (if lf (1+ lf) head-max))
               (rest (- max head-end))
               (tail-start (- n rest)))
          (unless (and (> tail-start 0)
                       (char= (char string (1- tail-start)) #\Newline))
            (let ((next (position #\Newline string :start tail-start)))
              (when next (setf tail-start (1+ next)))))
          (let ((omitted (- tail-start head-end)))
            (with-output-to-string (s)
              (write-string string s :end head-end)
              (when (and (> head-end 0)
                         (char/= (char string (1- head-end)) #\Newline))
                (write-char #\Newline s))
              (format s "[truncated: ~d characters omitted; redirect the output to a file and use read or grep on it]~%"
                      omitted)
              (write-string string s :start tail-start)))))))
