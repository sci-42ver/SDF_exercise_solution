;;; search for stack references
;; 0. https://unix.stackexchange.com/questions/156008/is-it-possible-to-use-find-exec-sh-c-safely
;; 0.a. IGNORE TODO why 
;; > Never embed {} in the shell code! That creates a command injection vulnerability.
;; see https://unix.stackexchange.com/questions/262042/how-to-escape-shell-metacharacters-automatically-with-find-command/262072#262072
;; in Stéphane Chazelas's comment.
;; > What if there's a file called $(rm -rf "$HOME").xml for instance? The correct way is to pass those {} as argument to the in-line shell script (-exec sh -c 'use as "$1"...' sh {} \;).
;; Here "$(basename "{}" .xml)" may then execute "$(rm -rf "$HOME").xml" with sh after replacement by find.
;; But with $1, find will pass the file name to $1 which sh will *not do anything further*.
;; See
; ```shell
; $ touch '$(touch Testfile).txt'
; $ find . -maxdepth 1 -name '*touch*' -exec sh -c 'set -x;$(basename "$1" .txt)' sh {} \;
; ++ basename './$(touch Testfile).txt' .txt
; + '$(touch' 'Testfile)'
; sh: line 1: $(touch: command not found
; $ ls Testfile 
; ls: cannot access 'Testfile': No such file or directory
; $ find . -maxdepth 1 -name '*touch*' -exec sh -c 'set -x;$(basename "{}" .txt)' \;
; +++ touch Testfile
; ++ basename ./.txt .txt
; + .txt
; sh: line 1: .txt: command not found
; $ ls Testfile 
; Testfile
; ```
;; 0.b. IGNORE TODO code means command_name in `man sh`?
;; > not in the code argument
;; means we should put "{}" filename inside sh code, e.g. '$(basename "{}" .txt)' etc.
;; i.e. code argument refers to inline script like '$(basename "{}" .txt)'.
;; > the sh inline script (the code argument)
;; 0.c. > Don't write:
;; {} will be replaced by xargs to $(...) which will then be run by sh...
;; 'for file do cmd > "$file"; done' will keep unchanged file args in $@, and then again only one de-reference by $file.
;; > avoiding running one sh per file and the error when list.txt doesn't contain any file.
;; TODO no error for `<list.txt xargs -I{} sh -c 'cmd > {}'` when empty list.txt
;; 1. In summary, it just says {} should not be used in 'sh -c "..."' because it can't
;; > replace {} with the argument quoted in the *right syntax* for the shell.
;; as parallel.
;; But it is fine to use outside "command_string" in `man sh`.

;;; IGNORE stack is not used as stack_... for var's in the Scheme program.
;; so it is fine to use -w for grep.

;;; reference search command
; $ find . -type f -name "ch5*" # these are skipped because SICP chapter 5 haven't learnt.  
; ./exercise_codes/SICP/book-codes/ch5-compiler.scm
; ./exercise_codes/SICP/book-codes/ch5-eceval-compiler.scm
; ./exercise_codes/SICP/book-codes/ch5-eceval-support.scm
; ./exercise_codes/SICP/book-codes/ch5-eceval.scm
; ./exercise_codes/SICP/book-codes/ch5-regsim.scm
; ./exercise_codes/SICP/book-codes/ch5-syntax.scm
; ./exercise_codes/SICP/book-codes/ch5.scm
; # "^[ ]*;" is to except comment 
; ~/SICP_SDF $ grep '%fail-stack' -r . | sed 's/:.*//g' | sort -u
; ./lecs/6.001_fall_2007_recitation/codes/rec20/amb-not-based-on-syntax/amb-not-based-on-syntax.scm
; ./SDF_exercises/common-lib/stack_lib.scm
; $ grep stack ./SICP_SDF/lecs/6.001_fall_2007_recitation/codes/rec20/amb-not-based-on-syntax/amb-not-based-on-syntax.scm | grep -v '%fail-stack'
; # these are useless
;       (error 'amb "Backtracking stack exhausted!")
;       ;; > The top of the fail stack contains the next continuation to be invoked
;         ;; > pops the fail stack and invokes the result
;           ;; > The amb procedure pushes a new continuation on top of the fail stack

; $ find . -type f ! -name "ch5*" -a -name "*.scm" \
; -exec sh -c 'grep -v -E "^[ ]*;" $@ \
; | grep -v -E "(%fail-stack)" | grep "define ([^ ]*stack" --color=always'  'sh' {} \+ | less_n
; # this is enough. I always use snippet in VSCode to output (define (proc ...)) to define proc.
; $ find . -type f ! -name "ch5*" -a -name "*.scm" \
; -exec sh -c 'grep "define ([^ ]*stack" --color=always'  'sh' {} \+ | sed 's/:.*//g' | sort -u
; ./lecs/6.001_spring_2007_recitation/codes/rec16/stack.scm
; ./lecs/6.037_lec_rec/rec/codes/rec5/eval-broken.scm
; ./SDF_exercises/chapter_4/4_17.scm
; ./SDF_exercises/chapter_4/4_9_stack_lib.scm
; ./SDF_exercises/common-lib/stack_lib.scm

;; If just using the last simplified command 
;; (I originally tried former complex commands which can't be done in VSCode due to pipe),
;; VSCode can do that much simple by "define \([^ ]*stack" re-pattern with "ch5*" files to exclude and "*.scm" files to include.

;;; reference
;; similar to lecs/6.001_spring_2007_recitation/codes/rec16/stack.scm
;; also see 
;; lecs/6.037_lec_rec/rec/codes/rec5/eval-broken.scm
;; SDF_exercises/chapter_4/4_9_stack_lib.scm
;; SICP_SDF/SDF_exercises/chapter_4/4_17.scm (bad program... so skipped)

;;; we should mimic perl API, see SDF_exercises/chapter_5/5_7.scm
(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "../common-lib/tagged_lst_lib.scm")
;; similar structure as lecs/6.037_lec_rec/rec/codes/rec5/eval-broken.scm but it doesn't have push...
(define stack-tag 'stack)
(define (make-new-stack)
  (empty-tagged-lst stack-tag))
;; same as SDF_exercises/chapter_4/4_9_stack_lib.scm
;; but with abstraction
(define (push! stack elem)
  ;; 0. different from lecs/6.001_spring_2007_recitation/codes/rec16/stack.scm
  ;; 0.a. infinite room if memory allows.
  (insert-elem-to-data-beginning! elem stack)
  stack
  )
(define (pop! stack)
  (and (empty-stack? stack) (error "empty stack can't be poped"))
  (let ((data (stack-data stack)))
    (set-cdr! stack (cdr data))
    (car data))
  )

;;; borrowed APIs from lecs/6.001_spring_2007_recitation/codes/rec16/stack.scm
(define empty-stack? empty-tagged-lst?)
;; IMHO peek is redundant with pop and duplicately calls empty?, i.e. empty-stack? here.

;;; borrowed APIs from SDF_exercises/chapter_4/4_9_stack_lib.scm
(define (new-stack data)
  (new-tagged-lst stack-tag data))
(define stack-data get-tagged-lst-data)
;; push-stack is skipped due to perl doesn't do that.
;; stack-top is redundant with pop!.

