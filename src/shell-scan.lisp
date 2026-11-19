(in-package #:agent-tools)

;;; SHELL-COMMAND-RESOURCES (H2c, pure): a conservative scanner from a
;;; command line to permission resources. Three dialects:
;;;
;;; - :POSIX — a single-pass tokenizer (%LEX-*: words with quote removal and
;;;   expansion tracking, operators, redirections with their target word,
;;;   here-doc bodies read at the next newline), then one consumer loop
;;;   (%SCAN-POSIX) over the tokens with the current directory, a stack of
;;;   directories for ( … ) and the cd rule (only && carries a cd).
;;; - :CMD — one simple command or "not modelled".
;;; - :OPAQUE — never modelled.
;;;
;;; Anything not modelled sets the STAR flag; "*" is appended last. The scan
;;; is total: the lexer always advances, nesting is capped, and the entry
;;; point's one HANDLER-CASE turns an internal error into the "not modelled"
;;; answer (the brief makes the function total).

(defparameter +scan-max-depth+ 64
  "Deepest nesting of $( … ), quotes and ${ … } the lexer follows; deeper
counts as not modelled.")

(defparameter +reserved-words+
  '("if" "for" "while" "until" "case" "select" "function" "[["))

(defparameter +indirect-programs+
  '("eval" "exec" "source" "." "command" "builtin" "env" "sudo" "doas" "su" "nohup"
    "nice" "timeout" "time" "xargs" "sh" "bash" "dash" "zsh" "ksh" "fish"))

(defparameter +writes-every-operand+
  '("rm" "rmdir" "unlink" "touch" "mkdir" "tee" "truncate" "mv" "shred"))

(defparameter +writes-last-operand+ '("cp" "ln" "install"))

(defparameter +find-actions+ '("-exec" "-execdir" "-ok" "-okdir" "-delete"))

(defparameter +cmd-starred-programs+
  '("cd" "chdir" "pushd" "popd" "del" "erase" "rd" "rmdir" "md" "mkdir" "ren" "rename"
    "move" "copy" "xcopy" "robocopy" "call" "start" "cmd" "powershell" "pwsh"))

(defun %blank-char-p (c)
  (member c '(#\Space #\Tab #\Newline #\Return)))

(defun %trim-command (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

;;; --- lexer ------------------------------------------------------------------

(defstruct (%lexer (:constructor %make-lexer (string &aux (end (length string)))))
  (string "" :type simple-string)
  (end 0 :type fixnum)
  (pos 0 :type fixnum)
  (star nil)
  (pending '()))                        ; here-docs: (delimiter strip-tabs quoted)

;;; A word token: (:WORD raw text dynamic glob tilde assignment quoted).
(defun %word-raw (w) (nth 1 w))
(defun %word-text (w) (nth 2 w))
(defun %word-dynamic-p (w) (nth 3 w))
(defun %word-glob-p (w) (nth 4 w))
(defun %word-tilde-p (w) (nth 5 w))
(defun %word-assignment-p (w) (nth 6 w))
(defun %word-quoted-p (w) (nth 7 w))
(defun %word-static-p (w)
  (not (or (%word-dynamic-p w) (%word-glob-p w) (%word-tilde-p w))))
(defun %word-display (w)
  (if (%word-dynamic-p w) (%word-raw w) (%word-text w)))

(defun %metachar-p (c)
  (member c '(#\Space #\Tab #\Newline #\; #\& #\| #\( #\) #\< #\>)))

(defun %name-start-char-p (c)
  (or (alpha-char-p c) (char= c #\_)))

(defun %name-char-p (c)
  (or (alphanumericp c) (char= c #\_)))

(defun %expansion-start-p (s i n)
  "S[I] is $. True when it starts an expansion (not a literal $)."
  (and (< (1+ i) n)
       (let ((c (char s (1+ i))))
         (or (%name-start-char-p c) (digit-char-p c)
             (find c "{('\"@*#?$!-")))))

(defun %skip-single (s i n)
  "I is just after an opening '. → index after the closing ', or NIL."
  (let ((q (position #\' s :start i :end n)))
    (and q (1+ q))))

(defun %skip-backtick (s i n)
  (loop while (< i n)
        do (case (char s i)
             (#\\ (incf i 2))
             (#\` (return-from %skip-backtick (1+ i)))
             (t (incf i))))
  nil)

(defun %skip-ansi-c (s i n)
  "I is just after $'."
  (loop while (< i n)
        do (case (char s i)
             (#\\ (incf i 2))
             (#\' (return-from %skip-ansi-c (1+ i)))
             (t (incf i))))
  nil)

(defun %skip-double (s i n depth)
  "I is just after an opening \". → index after the closing \", or NIL."
  (when (> depth +scan-max-depth+) (return-from %skip-double nil))
  (loop while (< i n)
        do (let ((c (char s i)))
             (cond ((char= c #\\) (incf i 2))
                   ((char= c #\") (return-from %skip-double (1+ i)))
                   ((char= c #\`)
                    (setf i (or (%skip-backtick s (1+ i) n) (return-from %skip-double nil))))
                   ((and (char= c #\$) (< (1+ i) n) (char= (char s (1+ i)) #\())
                    (setf i (or (%skip-parens s (+ i 2) n (1+ depth))
                                (return-from %skip-double nil))))
                   ((and (char= c #\$) (< (1+ i) n) (char= (char s (1+ i)) #\{))
                    (setf i (or (%skip-brace s (+ i 2) n (1+ depth))
                                (return-from %skip-double nil))))
                   (t (incf i)))))
  nil)

(defun %skip-parens (s i n depth)
  "I is just after an opening ( (one level open). → index after the
matching ), or NIL."
  (when (> depth +scan-max-depth+) (return-from %skip-parens nil))
  (let ((open 1))
    (loop while (< i n)
          do (let ((c (char s i)))
               (cond ((char= c #\\) (incf i 2))
                     ((char= c #\')
                      (setf i (or (%skip-single s (1+ i) n) (return-from %skip-parens nil))))
                     ((char= c #\")
                      (setf i (or (%skip-double s (1+ i) n (1+ depth))
                                  (return-from %skip-parens nil))))
                     ((char= c #\`)
                      (setf i (or (%skip-backtick s (1+ i) n) (return-from %skip-parens nil))))
                     ((char= c #\() (incf open) (incf i))
                     ((char= c #\))
                      (decf open)
                      (incf i)
                      (when (zerop open) (return-from %skip-parens i)))
                     (t (incf i))))))
  nil)

(defun %skip-brace (s i n depth)
  "I is just after ${. → index after the matching }, or NIL."
  (when (> depth +scan-max-depth+) (return-from %skip-brace nil))
  (let ((open 1))
    (loop while (< i n)
          do (let ((c (char s i)))
               (cond ((char= c #\\) (incf i 2))
                     ((char= c #\')
                      (setf i (or (%skip-single s (1+ i) n) (return-from %skip-brace nil))))
                     ((char= c #\")
                      (setf i (or (%skip-double s (1+ i) n (1+ depth))
                                  (return-from %skip-brace nil))))
                     ((char= c #\{) (incf open) (incf i))
                     ((char= c #\})
                      (decf open)
                      (incf i)
                      (when (zerop open) (return-from %skip-brace i)))
                     (t (incf i))))))
  nil)

(defun %double-quoted-text (s start end out)
  "Append the quote-removed text of the double-quoted S[START, END) (without
its quotes) to OUT. → true when it holds an expansion."
  (let ((dynamic nil) (i start))
    (loop while (< i end)
          do (let ((c (char s i)))
               (cond ((and (char= c #\\) (< (1+ i) end))
                      (let ((d (char s (1+ i))))
                        (cond ((char= d #\Newline))
                              ((find d "$`\"\\") (vector-push-extend d out))
                              (t (vector-push-extend c out) (vector-push-extend d out))))
                      (incf i 2))
                     ((char= c #\`) (setf dynamic t) (vector-push-extend c out) (incf i))
                     ((and (char= c #\$) (%expansion-start-p s i end))
                      (setf dynamic t) (vector-push-extend c out) (incf i))
                     (t (vector-push-extend c out) (incf i)))))
    dynamic))

(defun %lex-word (lx)
  "A word starting at the lexer position (not a metacharacter)."
  (let* ((s (%lexer-string lx)) (n (%lexer-end lx)) (start (%lexer-pos lx)) (i start)
         (text (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
         (dynamic nil) (glob nil) (tilde nil) (quoted nil))
    (flet ((bad ()
             (setf (%lexer-star lx) t dynamic t i n)))
      (loop while (and (< i n) (not (%metachar-p (char s i))))
            do (let ((c (char s i)))
                 (cond
                   ((char= c #\\)
                    (cond ((>= (1+ i) n) (incf i))
                          ((char= (char s (1+ i)) #\Newline) (incf i 2))
                          (t (setf quoted t)
                             (vector-push-extend (char s (1+ i)) text)
                             (incf i 2))))
                   ((char= c #\')
                    (let ((j (%skip-single s (1+ i) n)))
                      (if (null j)
                          (bad)
                          (progn (setf quoted t)
                                 (loop for k from (1+ i) below (1- j)
                                       do (vector-push-extend (char s k) text))
                                 (setf i j)))))
                   ((char= c #\")
                    (let ((j (%skip-double s (1+ i) n 0)))
                      (if (null j)
                          (bad)
                          (progn (setf quoted t)
                                 (when (%double-quoted-text s (1+ i) (1- j) text)
                                   (setf dynamic t))
                                 (setf i j)))))
                   ((char= c #\`)
                    (let ((j (%skip-backtick s (1+ i) n)))
                      (if (null j) (bad) (setf dynamic t i j))))
                   ((and (char= c #\$) (%expansion-start-p s i n))
                    (setf dynamic t)
                    (let* ((d (char s (1+ i)))
                           (j (cond ((char= d #\() (%skip-parens s (+ i 2) n 0))
                                    ((char= d #\{) (%skip-brace s (+ i 2) n 0))
                                    ((char= d #\') (%skip-ansi-c s (+ i 2) n))
                                    ((char= d #\") (%skip-double s (+ i 2) n 0))
                                    ((%name-start-char-p d)
                                     (or (position-if-not #'%name-char-p s :start (1+ i) :end n)
                                         n))
                                    (t (+ i 2)))))
                      (if (null j) (bad) (setf i j))))
                   ((find c "*?[")
                    (setf glob t)
                    (vector-push-extend c text)
                    (incf i))
                   ((and (char= c #\~) (= i start))
                    (setf tilde t)
                    (vector-push-extend c text)
                    (incf i))
                   (t (vector-push-extend c text) (incf i))))))
    (setf (%lexer-pos lx) (max i (1+ start)))
    (let* ((raw (subseq s start (min n (%lexer-pos lx))))
           (eq (position #\= raw))
           (assignment (and eq (plusp eq)
                            (%name-start-char-p (char raw 0))
                            (every #'%name-char-p (subseq raw 0 eq)))))
      (list :word raw (coerce text 'simple-string) dynamic glob tilde assignment quoted))))

(defun %skip-blanks (lx)
  (let ((s (%lexer-string lx)) (n (%lexer-end lx)))
    (loop while (and (< (%lexer-pos lx) n)
                     (member (char s (%lexer-pos lx)) '(#\Space #\Tab)))
          do (incf (%lexer-pos lx)))))

(defun %lex-target (lx)
  "The word after a redirection operator, or NIL (and STAR) when none."
  (%skip-blanks lx)
  (let ((s (%lexer-string lx)) (i (%lexer-pos lx)))
    (if (or (>= i (%lexer-end lx)) (%metachar-p (char s i)))
        (progn (setf (%lexer-star lx) t) nil)
        (%lex-word lx))))

(defun %lex-redirection (lx fd)
  "A redirection at the lexer position (< > or &>). → (:REDIR op fd target)."
  (let* ((s (%lexer-string lx)) (n (%lexer-end lx)) (i (%lexer-pos lx)))
    (flet ((at (k) (and (< (+ i k) n) (char s (+ i k)))))
      (multiple-value-bind (op width)
          (case (char s i)
            (#\< (case (at 1)
                   (#\< (case (at 2)
                          (#\- (values "<<-" 3))
                          (#\< (values "<<<" 3))
                          (t (values "<<" 2))))
                   (#\& (values "<&" 2))
                   (#\> (values "<>" 2))
                   (t (values "<" 1))))
            (#\> (case (at 1)
                   (#\> (values ">>" 2))
                   (#\| (values ">|" 2))
                   (#\& (values ">&" 2))
                   (t (values ">" 1))))
            (t (if (eql (at 2) #\>) (values "&>>" 3) (values "&>" 2))))
        (setf (%lexer-pos lx) (+ i width))
        (cond
          ((member op '("<<" "<<-") :test #'string=)
           (let ((delimiter (%lex-target lx)))
             (when delimiter
               (setf (%lexer-pending lx)
                     (append (%lexer-pending lx)
                             (list (list (%word-text delimiter)
                                         (string= op "<<-")
                                         (or (%word-quoted-p delimiter)
                                             (find-if (lambda (c) (find c "'\"\\"))
                                                      (%word-raw delimiter))))))))
             (list :redir op fd nil)))
          (t (list :redir op fd (%lex-target lx))))))))

(defun %read-here-documents (lx)
  "At the start of a line: consume the bodies of the pending here-docs."
  (let ((s (%lexer-string lx)) (n (%lexer-end lx)))
    (dolist (doc (shiftf (%lexer-pending lx) '()))
      (destructuring-bind (delimiter strip quoted) doc
        (loop
          (when (>= (%lexer-pos lx) n)
            (setf (%lexer-star lx) t)     ; no terminator
            (return-from %read-here-documents))
          (let* ((i (%lexer-pos lx))
                 (eol (or (position #\Newline s :start i) n))
                 (line (subseq s i eol)))
            (setf (%lexer-pos lx) (min n (1+ eol)))
            (when strip
              (setf line (string-left-trim '(#\Tab) line)))
            (when (string= line delimiter)
              (return))
            (when (and (not quoted) (find-if (lambda (c) (find c "$`")) line))
              (setf (%lexer-star lx) t))
            (when (>= eol n)
              (setf (%lexer-star lx) t)
              (return-from %read-here-documents))))))))

(defun %lex (string)
  "→ (values tokens star) for the POSIX command line STRING."
  (let* ((lx (%make-lexer (coerce string 'simple-string)))
         (s (%lexer-string lx)) (n (%lexer-end lx))
         (tokens '()))
    (flet ((op (name width)
             (push (list :op name) tokens)
             (incf (%lexer-pos lx) width)))
      (loop while (< (%lexer-pos lx) n)
            do (let* ((i (%lexer-pos lx))
                      (c (char s i))
                      (next (and (< (1+ i) n) (char s (1+ i)))))
                 (cond
                   ((member c '(#\Space #\Tab #\Return)) (incf (%lexer-pos lx)))
                   ((and (char= c #\\) (eql next #\Newline)) (incf (%lexer-pos lx) 2))
                   ((char= c #\Newline)
                    (op (string #\Newline) 1)
                    (%read-here-documents lx))
                   ((char= c #\#)
                    (setf (%lexer-pos lx) (or (position #\Newline s :start i) n)))
                   ((char= c #\;) (if (eql next #\;) (op ";;" 2) (op ";" 1)))
                   ((char= c #\&)
                    (cond ((eql next #\&) (op "&&" 2))
                          ((eql next #\>) (push (%lex-redirection lx nil) tokens))
                          (t (op "&" 1))))
                   ((char= c #\|)
                    (cond ((eql next #\|) (op "||" 2))
                          ((eql next #\&) (op "|&" 2))
                          (t (op "|" 1))))
                   ((char= c #\() (if (eql next #\() (op "((" 2) (op "(" 1)))
                   ((char= c #\)) (op ")" 1))
                   ((and (member c '(#\< #\>)) (eql next #\())
                    ;; Process substitution: a dynamic word.
                    (let ((j (%skip-parens s (+ i 2) n 0)))
                      (setf (%lexer-star lx) t
                            (%lexer-pos lx) (or j n))
                      (push (list :word (subseq s i (or j n)) "" t nil nil nil nil) tokens)))
                   ((member c '(#\< #\>)) (push (%lex-redirection lx nil) tokens))
                   ((digit-char-p c)
                    (let ((j (or (position-if-not #'digit-char-p s :start i :end n) n)))
                      (if (and (< j n) (member (char s j) '(#\< #\>)))
                          (progn (setf (%lexer-pos lx) j)
                                 (push (%lex-redirection lx (subseq s i j)) tokens))
                          (push (%lex-word lx) tokens))))
                   (t (push (%lex-word lx) tokens))))))
    (when (%lexer-pending lx)
      (setf (%lexer-star lx) t))          ; here-doc with no body at all
    (values (nreverse tokens) (%lexer-star lx))))

;;; --- paths --------------------------------------------------------------------

;;; A directory or path is (absolute-p . segments). Relative segments are
;;; root-relative and may start with "..".

(defun %scan-segments (string)
  (remove "" (uiop:split-string string :separator "/") :test #'string=))

(defun %rel-normalize (segments)
  "Lexical normalization that keeps leading \"..\"."
  (let ((out '()))
    (dolist (s segments (nreverse out))
      (cond ((string= s "."))
            ((string= s "..")
             (if (and out (string/= (first out) ".."))
                 (pop out)
                 (push s out)))
            (t (push s out))))))

(defun %abs-normalize (segments)
  (let ((out '()))
    (dolist (s segments (nreverse out))
      (cond ((string= s "."))
            ((string= s "..") (pop out))
            (t (push s out))))))

(defun %scan-join (base string)
  "STRING (a path word) against the directory BASE. → a path, or :UNKNOWN."
  (cond ((and (plusp (length string)) (char= (char string 0) #\/))
         (cons t (%abs-normalize (%scan-segments string))))
        ((eq base :unknown) :unknown)
        ((car base)
         (cons t (%abs-normalize (append (cdr base) (%scan-segments string)))))
        (t (cons nil (%rel-normalize (append (cdr base) (%scan-segments string)))))))

(defun %format-path (path root)
  "PATH as a resource string: root-relative under ROOT (segments or NIL),
absolute elsewhere; a relative path that climbs out stays relative without
a ROOT."
  (destructuring-bind (absolute-p . segments) path
    (cond
      ((and (not absolute-p) root (equal (first segments) ".."))
       (%format-path (cons t (%abs-normalize (append root segments))) root))
      ((not absolute-p)
       (if segments (format nil "~{~a~^/~}" segments) "."))
      ((and root (%prefix-p root segments))
       (let ((tail (nthcdr (length root) segments)))
         (if tail (format nil "~{~a~^/~}" tail) ".")))
      (t (format nil "/~{~a~^/~}" segments)))))

;;; --- consumer ------------------------------------------------------------------

(defstruct (%scan (:constructor %make-scan (root cwd)))
  root                                  ; segments or NIL
  cwd                                   ; path or :UNKNOWN
  (stack '())
  (out '())                             ; resources, newest first
  (star nil))

(defun %emit (scan resource)
  (unless (member resource (%scan-out scan) :test #'string=)
    (push resource (%scan-out scan))))

(defun %emit-path (scan kind word)
  "Emit KIND:<path> for the path WORD, or STAR when it is not static."
  (if (not (%word-static-p word))
      (setf (%scan-star scan) t)
      (let ((path (%scan-join (%scan-cwd scan) (%word-text word))))
        (cond ((eq path :unknown) (setf (%scan-star scan) t))
              ((equal path '(t "dev" "null")))
              (t (%emit scan (concatenate 'string kind ":"
                                          (%format-path path (%scan-root scan)))))))))

(defun %operands (args)
  "The operand words of ARGS: options (\"-x\") are skipped until \"--\"."
  (let ((out '()) (options t))
    (dolist (a args (nreverse out))
      (let ((text (%word-text a)))
        (cond ((and options (%word-static-p a) (string= text "--")) (setf options nil))
              ((and options (%word-static-p a) (> (length text) 1) (char= (char text 0) #\-)))
              (t (push a out)))))))

(defun %scan-cd (scan args)
  "Resources for cd ARGS. → the new directory, or :UNKNOWN."
  (let ((operands (member-if-not (lambda (a)
                                   (and (%word-static-p a)
                                        (member (%word-text a) '("-L" "-P") :test #'string=)))
                                 args)))
    (when (and operands (%word-static-p (first operands))
               (string= (%word-text (first operands)) "--"))
      (pop operands))
    (let ((target (and (= (length operands) 1)
                       (%word-static-p (first operands))
                       (string/= (%word-text (first operands)) "-")
                       (plusp (length (%word-text (first operands))))
                       (%scan-join (%scan-cwd scan) (%word-text (first operands))))))
      (cond ((or (null target) (eq target :unknown))
             (setf (%scan-star scan) t)
             :unknown)
            (t (%emit scan (concatenate 'string "cd:" (%format-path target (%scan-root scan))))
               target)))))

(defun %scan-command (scan words redirections)
  "Emit the resources of one simple command. → (values cd-p directory):
CD-P when it was a cd, DIRECTORY its target (or :UNKNOWN)."
  (let ((cd-p nil) (new nil))
    (when words
      (%emit scan (format nil "bash:~{~a~^ ~}" (mapcar #'%word-display words)))
      (when (some #'%word-dynamic-p words)
        (setf (%scan-star scan) t))
      (let* ((rest (member-if-not #'%word-assignment-p words))
             (argv0 (first rest))
             (args (rest rest)))
        (when argv0
          (if (%word-dynamic-p argv0)
              (setf (%scan-star scan) t)
              (let ((program (%word-text argv0)))
                (%emit scan (concatenate 'string "program:" program))
                (when (member program +indirect-programs+ :test #'string=)
                  (setf (%scan-star scan) t))
                (when (and (string= program "find")
                           (some (lambda (a) (member (%word-text a) +find-actions+
                                                     :test #'string=))
                                 args))
                  (setf (%scan-star scan) t))
                (cond
                  ((string= program "cd")
                   (setf cd-p t new (%scan-cd scan args)))
                  ((member program +writes-every-operand+ :test #'string=)
                   (dolist (w (%operands args)) (%emit-path scan "write" w)))
                  ((member program +writes-last-operand+ :test #'string=)
                   (let ((last (car (last (%operands args)))))
                     (when last (%emit-path scan "write" last))))))))))
    (dolist (r redirections)
      (destructuring-bind (op fd target) (rest r)
        (declare (ignore fd))
        (cond
          ((member op '("<<" "<<-") :test #'string=))
          ((null target))
          ((string= op "<<<")
           (unless (%word-static-p target) (setf (%scan-star scan) t)))
          ((and (member op '("<&" ">&") :test #'string=)
                (%word-static-p target)
                (or (string= (%word-text target) "-")
                    (every #'digit-char-p (%word-text target)))))
          ((member op '("<" "<&") :test #'string=) (%emit-path scan "read" target))
          ((string= op "<>")
           (%emit-path scan "read" target)
           (%emit-path scan "write" target))
          (t (%emit-path scan "write" target)))))
    (values cd-p new)))

(defun %scan-posix (command root workdir)
  "→ (values resources star) for the POSIX COMMAND."
  (multiple-value-bind (tokens lex-star) (%lex command)
    (let ((scan (%make-scan root workdir))
          (words '()) (redirections '()) (in-pipe nil))
      (when lex-star (setf (%scan-star scan) t))
      (flet ((end-command (op)
               (multiple-value-bind (cd-p new)
                   (%scan-command scan (reverse words) (reverse redirections))
                 (let ((pipe-member (or in-pipe (member op '("|" "|&") :test #'equal))))
                   (when (and cd-p (not pipe-member) (not (equal op "&")))
                     (setf (%scan-cwd scan) (if (equal op "&&") new :unknown)))
                   (setf in-pipe (and (member op '("|" "|&") :test #'equal) t))))
               (setf words '() redirections '())))
        (loop for token in tokens
              do (ecase (first token)
                   (:word
                    (let ((text (%word-text token)))
                      (cond
                        ((and (null words) (not (%word-quoted-p token))
                              (not (%word-dynamic-p token)))
                         (cond ((member text +reserved-words+ :test #'string=)
                                (setf (%scan-star scan) t)
                                (loop-finish))
                               ((member text '("{" "}" "!") :test #'string=))
                               (t (push token words))))
                        (t (push token words)))))
                   (:redir (push token redirections))
                   (:op
                    (let ((op (second token)))
                      (cond
                        ((string= op "((")
                         (setf (%scan-star scan) t)
                         (loop-finish))
                        ((string= op "(")
                         (when (or words redirections)
                           ;; name() — a function definition.
                           (setf (%scan-star scan) t)
                           (loop-finish))
                         (push (%scan-cwd scan) (%scan-stack scan)))
                        ((string= op ")")
                         (end-command ")")
                         (if (%scan-stack scan)
                             (setf (%scan-cwd scan) (pop (%scan-stack scan)))
                             (setf (%scan-star scan) t)))
                        (t (end-command op))))))
              finally (progn
                        (end-command :end)
                        (when (%scan-stack scan) (setf (%scan-star scan) t)))))
      (values (reverse (%scan-out scan)) (%scan-star scan)))))

;;; --- cmd and opaque -----------------------------------------------------------

(defun %scan-cmd (command)
  "→ (values resources star) for one cmd.exe command line (trimmed)."
  (flet ((opaque () (values (list (concatenate 'string "bash:" command)) t)))
    (when (find-if (lambda (c) (find c '(#\% #\! #\Newline #\Return))) command)
      (return-from %scan-cmd (opaque)))
    (let ((words '())
          (word (make-array 8 :element-type 'character :adjustable t :fill-pointer 0))
          (in-word nil) (quoted nil))
      (loop for c across command
            do (cond
                 ((char= c #\") (setf quoted (not quoted) in-word t))
                 ((and (not quoted) (find c "&|<>^()"))
                  (return-from %scan-cmd (opaque)))
                 ((and (not quoted) (member c '(#\Space #\Tab)))
                  (when in-word
                    (push (coerce word 'simple-string) words)
                    (setf (fill-pointer word) 0 in-word nil)))
                 (t (vector-push-extend c word) (setf in-word t))))
      (when quoted (return-from %scan-cmd (opaque)))
      (when in-word (push (coerce word 'simple-string) words))
      (setf words (nreverse words))
      (let* ((argv0 (string-downcase (or (first words) "")))
             (program (let ((dot (and (> (length argv0) 4) (- (length argv0) 4))))
                        (if (and dot (member (subseq argv0 dot) '(".exe" ".com" ".bat" ".cmd")
                                             :test #'string=))
                            (subseq argv0 0 dot)
                            argv0))))
        (values (append (list (format nil "bash:~{~a~^ ~}" words))
                        (and (plusp (length program))
                             (list (concatenate 'string "program:" program))))
                (and (member program +cmd-starred-programs+ :test #'string=) t))))))

(defun %scan-dispatch (command dialect workdir root)
  (let* ((trimmed (%trim-command command))
         (root-segments (and (stringp root) (plusp (length root))
                             (char= (char root 0) #\/)
                             (%abs-normalize (%scan-segments root))))
         (wd (%scan-join '(nil) (if (and (stringp workdir) (plusp (length workdir)))
                                    workdir
                                    ".")))
         (wd-resource (and (not (equal wd '(nil)))
                           (concatenate 'string "cd:" (%format-path wd root-segments)))))
    (multiple-value-bind (resources star)
        (cond ((zerop (length trimmed)) (values '() nil))
              ((eq dialect :cmd) (%scan-cmd trimmed))
              ((eq dialect :posix) (%scan-posix command root-segments wd))
              (t (values (list (concatenate 'string "bash:" trimmed)) t)))
      (when (or resources star)
        (append (and wd-resource (list wd-resource))
                (remove wd-resource resources :test #'equal)
                (and star (list "*")))))))

(defun shell-command-resources (command &key (dialect :posix) (workdir ".") root)
  "→ the permission resources of COMMAND (a command line string) for
AI-AGENT-PROTOCOL rules, in order of appearance, without duplicates:
\"bash:<words>\" and \"program:<argv0>\" per simple command, \"cd:<path>\",
\"read:<path>\", \"write:<path>\" for the paths it changes to, reads or
writes, and \"*\" (last) when the scan met a construct it does not model.
NIL for a blank or comment-only command. Never signals.
WORKDIR: root-relative POSIX directory the command starts in; when it is not
\".\" the first resource is \"cd:<workdir>\". ROOT: the root's absolute POSIX
string, or NIL; with it, absolute paths under the root and relative paths
that leave it are reported root-relative and absolute respectively."
  (if (not (stringp command))
      (list "*")
      ;; The one catch-all: the brief makes this function total, so an
      ;; internal error means "not modelled", never a signal.
      (handler-case (%scan-dispatch command dialect workdir root)
        (error ()
          (let ((trimmed (ignore-errors (%trim-command command))))
            (if (and trimmed (plusp (length trimmed)))
                (list (concatenate 'string "bash:" trimmed) "*")
                (list "*")))))))
