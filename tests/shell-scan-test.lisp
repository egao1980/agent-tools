(in-package #:agent-tools/tests)

;;; SHELL-COMMAND-RESOURCES: the command scanner (pure, every OS). Exact
;;; lists for what the scanner models; "*" last for what it does not.

(defun nl-join (&rest lines)
  (format nil "~{~a~^~%~}" lines))

(deftest scan-simple-commands
  (check-resources
   '(("git status" ("bash:git status" "program:git"))
     ("  git   status  --short " ("bash:git status --short" "program:git"))
     ("ls" ("bash:ls" "program:ls"))
     ("'git' status" ("bash:git status" "program:git"))
     ("echo 'a  b' \"c d\"" ("bash:echo a  b c d" "program:echo"))
     ("echo a\\ b" ("bash:echo a b" "program:echo"))
     ("echo 'no $expansion `here`'" ("bash:echo no $expansion `here`" "program:echo"))
     ("git log # show history" ("bash:git log" "program:git"))
     ("echo a#b" ("bash:echo a#b" "program:echo"))
     ("FOO=1 BAR=2 make" ("bash:FOO=1 BAR=2 make" "program:make"))
     ("FOO=1" ("bash:FOO=1"))
     ("./build.sh --fast" ("bash:./build.sh --fast" "program:./build.sh"))
     ("find . -name '*.lisp'" ("bash:find . -name *.lisp" "program:find")))))

(deftest scan-blank-commands
  (ok (null (scan "")))
  (ok (null (scan "   ")))
  (ok (null (scan "# just a comment")))
  (ok (null (scan (nl-join "" "  " "# c" "")))))

(deftest scan-lists-and-pipelines
  (check-resources
   `(("git status && git diff" ("bash:git status" "program:git" "bash:git diff"))
     ("make; make test" ("bash:make" "program:make" "bash:make test"))
     ("echo a;echo b" ("bash:echo a" "program:echo" "bash:echo b"))
     ("cat a.txt | grep x | wc -l"
      ("bash:cat a.txt" "program:cat" "bash:grep x" "program:grep" "bash:wc -l" "program:wc"))
     ("false || echo failed" ("bash:false" "program:false" "bash:echo failed" "program:echo"))
     ("sleep 1 &" ("bash:sleep 1" "program:sleep"))
     ("make 2>&1 | tee -a build.log"
      ("bash:make" "program:make" "bash:tee -a build.log" "program:tee" "write:build.log"))
     (,(nl-join "git add -A" "git commit -m 'msg'")
      ("bash:git add -A" "program:git" "bash:git commit -m msg"))
     (,(format nil "echo a \\~%  b") ("bash:echo a b" "program:echo"))
     ("( cd sub && make )" ("bash:cd sub" "program:cd" "cd:sub" "bash:make" "program:make"))
     ("{ echo a; echo b; }" ("bash:echo a" "program:echo" "bash:echo b"))
     ("! grep -q x f" ("bash:grep -q x f" "program:grep")))))

(deftest scan-redirections
  (check-resources
   `(("echo hi > out.txt" ("bash:echo hi" "program:echo" "write:out.txt"))
     ("echo hi >> log/a.log" ("bash:echo hi" "program:echo" "write:log/a.log"))
     ("echo hi >| f" ("bash:echo hi" "program:echo" "write:f"))
     ("echo x >out.txt" ("bash:echo x" "program:echo" "write:out.txt"))
     ("echo x>out.txt" ("bash:echo x" "program:echo" "write:out.txt"))
     ("make 2> err.txt" ("bash:make" "program:make" "write:err.txt"))
     ("echo 2>x" ("bash:echo" "program:echo" "write:x"))
     ("echo a2>x" ("bash:echo a2" "program:echo" "write:x"))
     ("make > out.txt 2>&1" ("bash:make" "program:make" "write:out.txt"))
     ("make &> all.txt" ("bash:make" "program:make" "write:all.txt"))
     ("sort < in.txt > out.txt" ("bash:sort" "program:sort" "read:in.txt" "write:out.txt"))
     ("> empty.txt" ("write:empty.txt"))
     ("ls 2>/dev/null" ("bash:ls" "program:ls"))
     ("ls >/dev/null 2>&1" ("bash:ls" "program:ls"))
     ("echo x > 'my file.txt'" ("bash:echo x" "program:echo" "write:my file.txt"))
     ("echo x > /tmp/out" ("bash:echo x" "program:echo" "write:/tmp/out"))
     ("echo x > ../up.txt" ("bash:echo x" "program:echo" "write:../up.txt"))
     ("echo x > ./a/../b.txt" ("bash:echo x" "program:echo" "write:b.txt"))
     ("grep x <<< 'abc'" ("bash:grep x" "program:grep")))))

(deftest scan-here-documents
  (check-resources
   `((,(nl-join "cat <<EOF > notes.md" "hello" "EOF")
      ("bash:cat" "program:cat" "write:notes.md"))
     (,(nl-join "cat <<'EOF'" "$HOME `x` $(y)" "EOF" "echo done")
      ("bash:cat" "program:cat" "bash:echo done" "program:echo"))
     (,(nl-join "cat <<\"EOF\"" "$HOME" "EOF")
      ("bash:cat" "program:cat"))
     (,(nl-join "cat <<-EOF" (format nil "~cbody" #\Tab) (format nil "~cEOF" #\Tab) "ls")
      ("bash:cat" "program:cat" "bash:ls" "program:ls"))
     (,(nl-join "cat <<EOF" "plain text" "EOF")
      ("bash:cat" "program:cat"))
     (,(nl-join "cat <<EOF" "$HOME" "EOF")
      ("bash:cat" "program:cat" "*"))
     (,(nl-join "cat <<EOF" "no end")
      ("bash:cat" "program:cat" "*")))))

(deftest scan-written-operands
  (check-resources
   '(("rm -rf build dist" ("bash:rm -rf build dist" "program:rm" "write:build" "write:dist"))
     ("rm" ("bash:rm" "program:rm"))
     ("mkdir -p a/b" ("bash:mkdir -p a/b" "program:mkdir" "write:a/b"))
     ("touch -- -x" ("bash:touch -- -x" "program:touch" "write:-x"))
     ("cp a.txt b.txt" ("bash:cp a.txt b.txt" "program:cp" "write:b.txt"))
     ("cp -r src/ dst" ("bash:cp -r src/ dst" "program:cp" "write:dst"))
     ("mv a.txt b.txt" ("bash:mv a.txt b.txt" "program:mv" "write:a.txt" "write:b.txt"))
     ("ln -s target link" ("bash:ln -s target link" "program:ln" "write:link"))
     ("tee out.log" ("bash:tee out.log" "program:tee" "write:out.log"))
     ("rmdir old" ("bash:rmdir old" "program:rmdir" "write:old"))
     ("unlink x" ("bash:unlink x" "program:unlink" "write:x"))
     ("rm a b a" ("bash:rm a b a" "program:rm" "write:a" "write:b"))
     ("rm *.o" ("bash:rm *.o" "program:rm" "*"))
     ("rm -f \"$F\"" ("bash:rm -f \"$F\"" "program:rm" "*"))
     ("echo x > ~/x" ("bash:echo x" "program:echo" "*"))
     ("echo x > $OUT" ("bash:echo x" "program:echo" "*"))
     ("echo x > *.txt" ("bash:echo x" "program:echo" "*")))))

(deftest scan-cd
  (check-resources
   '(("cd src && rm a.o"
      ("bash:cd src" "program:cd" "cd:src" "bash:rm a.o" "program:rm" "write:src/a.o"))
     ("cd src; rm a.o"
      ("bash:cd src" "program:cd" "cd:src" "bash:rm a.o" "program:rm" "*"))
     ("cd src || rm a.o"
      ("bash:cd src" "program:cd" "cd:src" "bash:rm a.o" "program:rm" "*"))
     ("cd /tmp && rm x"
      ("bash:cd /tmp" "program:cd" "cd:/tmp" "bash:rm x" "program:rm" "write:/tmp/x"))
     ("cd src && cd .. && touch top"
      ("bash:cd src" "program:cd" "cd:src" "bash:cd .." "cd:." "bash:touch top"
       "program:touch" "write:top"))
     ("cd -P src && touch x"
      ("bash:cd -P src" "program:cd" "cd:src" "bash:touch x" "program:touch" "write:src/x"))
     ("cd ..; ls" ("bash:cd .." "program:cd" "cd:.." "bash:ls" "program:ls"))
     ("cd src; rm /abs/x"
      ("bash:cd src" "program:cd" "cd:src" "bash:rm /abs/x" "program:rm" "write:/abs/x"))
     ("(cd sub && touch a); touch b"
      ("bash:cd sub" "program:cd" "cd:sub" "bash:touch a" "program:touch" "write:sub/a"
       "bash:touch b" "write:b"))
     ("cd a & touch b"
      ("bash:cd a" "program:cd" "cd:a" "bash:touch b" "program:touch" "write:b"))
     ("cd a | touch b"
      ("bash:cd a" "program:cd" "cd:a" "bash:touch b" "program:touch" "write:b"))
     ("cd" ("bash:cd" "program:cd" "*"))
     ("cd -" ("bash:cd -" "program:cd" "*"))
     ("cd a b" ("bash:cd a b" "program:cd" "*"))
     ("cd ~/x" ("bash:cd ~/x" "program:cd" "*"))
     ("cd \"$DIR\" && touch x"
      ("bash:cd \"$DIR\"" "program:cd" "bash:touch x" "program:touch" "*")))))

(deftest scan-workdir-and-root
  (check-resources '(("make > out.txt" ("cd:src" "bash:make" "program:make" "write:src/out.txt"))
                     ("echo > ../top.txt" ("cd:src" "bash:echo" "program:echo" "write:top.txt"))
                     ("echo > ../../up.txt" ("cd:src" "bash:echo" "program:echo" "write:../up.txt")))
                   :workdir "src")
  (check-resources '(("ls" ("bash:ls" "program:ls")))
                   :workdir ".")
  (check-resources '(("echo x > /home/u/proj/a.txt" ("bash:echo x" "program:echo" "write:a.txt"))
                     ("echo x > /home/u/proj" ("bash:echo x" "program:echo" "write:."))
                     ("echo x > ../other/a.txt"
                      ("bash:echo x" "program:echo" "write:/home/u/other/a.txt"))
                     ("echo x > /home/u/project2/a" ("bash:echo x" "program:echo" "write:/home/u/project2/a"))
                     ("cd /home/u/proj/src && touch x"
                      ("bash:cd /home/u/proj/src" "program:cd" "cd:src" "bash:touch x"
                       "program:touch" "write:src/x")))
                   :root "/home/u/proj")
  (check-resources '(("echo x > /home/u/proj/a.txt" ("bash:echo x" "program:echo" "write:a.txt")))
                   :root "/home/u/proj/")
  (check-resources '(("echo x > ../../a.txt" ("cd:lib/sub" "bash:echo x" "program:echo" "write:a.txt"))
                     ("echo x > ../../../a.txt"
                      ("cd:lib/sub" "bash:echo x" "program:echo" "write:/home/u/a.txt")))
                   :root "/home/u/proj" :workdir "lib/sub"))

(deftest scan-dynamic-words
  (check-resources
   '(("echo $(date)" ("bash:echo $(date)" "program:echo" "*"))
     ("echo `date`" ("bash:echo `date`" "program:echo" "*"))
     ("echo $HOME" ("bash:echo $HOME" "program:echo" "*"))
     ("echo ${HOME}/x" ("bash:echo ${HOME}/x" "program:echo" "*"))
     ("echo \"$(rm -rf /)\"" ("bash:echo \"$(rm -rf /)\"" "program:echo" "*"))
     ("echo $(echo \"a)\" ) ok" ("bash:echo $(echo \"a)\" ) ok" "program:echo" "*"))
     ("$CMD arg" ("bash:$CMD arg" "*"))
     ("git log $(rm -rf x) && ls" ("bash:git log $(rm -rf x)" "program:git" "bash:ls" "program:ls" "*")))))

(deftest scan-indirection
  (check-resources
   '(("sudo rm -rf /" ("bash:sudo rm -rf /" "program:sudo" "*"))
     ("xargs rm < list.txt" ("bash:xargs rm" "program:xargs" "read:list.txt" "*"))
     ("sh -c 'rm -rf x'" ("bash:sh -c rm -rf x" "program:sh" "*"))
     ("bash script.sh" ("bash:bash script.sh" "program:bash" "*"))
     ("env FOO=1 make" ("bash:env FOO=1 make" "program:env" "*"))
     ("eval ls" ("bash:eval ls" "program:eval" "*"))
     (". ./env.sh" ("bash:. ./env.sh" "program:." "*"))
     ("source ./env.sh" ("bash:source ./env.sh" "program:source" "*"))
     ("exec > log" ("bash:exec" "program:exec" "write:log" "*"))
     ("time make" ("bash:time make" "program:time" "*"))
     ("nohup ./server &" ("bash:nohup ./server" "program:nohup" "*"))
     ("find . -name '*.o' -delete" ("bash:find . -name *.o -delete" "program:find" "*"))
     ("find . -exec rm {} ;" ("bash:find . -exec rm {}" "program:find" "*")))))

(deftest scan-unmodelled-syntax
  (check-dynamic
   (list "if true; then rm x; fi"
         "for f in *.txt; do echo x; done"
         "while true; do ls; done"
         "until false; do ls; done"
         "case x in a) echo;; esac"
         "f() { rm x; }; f"
         "function f { rm x; }"
         "[[ -f x ]] && rm x"
         "(( i++ ))"
         "echo $((1+2))"
         "diff <(ls a) <(ls b)"
         "tee >(gzip > x.gz)"
         "echo $'a\\nb'"
         "echo 'unterminated"
         "echo \"unterminated"
         "echo $(unclosed"
         "(echo a"
         "echo a)"
         "echo `unclosed")))

(deftest scan-never-signals
  (let ((alphabet "ab $`'\"\\()<>&|;{}#~*?!=
	-/.")
        (x 1980)
        (bad '()))
    (flet ((rnd (n)                     ; a fixed LCG: the same cases every run
             (setf x (mod (+ (* x 1103515245) 12345) 2147483648))
             (mod (floor x 65536) n)))
      (dotimes (i 300)
        (let ((command (coerce (loop repeat (rnd 24)
                                     collect (char alphabet (rnd (length alphabet))))
                               'string)))
        (dolist (dialect '(:posix :cmd :opaque))
          (let ((r (handler-case (shell-command-resources command :dialect dialect)
                     (error () :error))))
            (unless (and (listp r) (every #'stringp r))
              (push (list dialect command) bad)))))))
    (ok (null bad) (format nil "~s" (subseq bad 0 (min 3 (length bad)))))))

(deftest scan-cmd-dialect
  (check-resources
   '(("git status" ("bash:git status" "program:git"))
     ("  dir   /b " ("bash:dir /b" "program:dir"))
     ("GIT.EXE status" ("bash:GIT.EXE status" "program:git"))
     ("npm.cmd test" ("bash:npm.cmd test" "program:npm"))
     ("echo \"a & b\"" ("bash:echo a & b" "program:echo"))
     ("echo hi > out.txt" ("bash:echo hi > out.txt" "*"))
     ("type a & del a" ("bash:type a & del a" "*"))
     ("dir | findstr x" ("bash:dir | findstr x" "*"))
     ("echo %PATH%" ("bash:echo %PATH%" "*"))
     ("echo !X!" ("bash:echo !X!" "*"))
     ("echo ^&" ("bash:echo ^&" "*"))
     ("(dir)" ("bash:(dir)" "*"))
     ("del a.txt" ("bash:del a.txt" "program:del" "*"))
     ("cd sub" ("bash:cd sub" "program:cd" "*"))
     ("copy a b" ("bash:copy a b" "program:copy" "*"))
     ("start notepad" ("bash:start notepad" "program:start" "*"))
     ("powershell -c ls" ("bash:powershell -c ls" "program:powershell" "*"))
     ("" ()))
   :dialect :cmd)
  (check-resources '(("dir" ("cd:src" "bash:dir" "program:dir")))
                   :dialect :cmd :workdir "src")
  (check-dynamic (list (format nil "dir~%del x")) :dialect :cmd))

(deftest scan-opaque-dialect
  (check-resources
   '(("Get-ChildItem -Force" ("bash:Get-ChildItem -Force" "*"))
     ("  ls  " ("bash:ls" "*"))
     ("" ()))
   :dialect :opaque)
  (check-resources '(("ls" ("cd:src" "bash:ls" "*")))
                   :dialect :opaque :workdir "src"))
