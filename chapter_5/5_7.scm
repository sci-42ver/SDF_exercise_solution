(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "../software/sdf/manager/load.scm")
(manage 'new 'generic-interpreter)

;;;; > Such an infix parser can be found on the *website for this book*.
;;; https://mitpress.mit.edu/9780262045490/software-design-for-flexibility/
;; doesn't have related things about codes

;;; https://ocw.mit.edu/courses/6-945-adventures-in-advanced-symbolic-programming-spring-2009/cf0915cfce8d7e39de04b9bc1ab851d3_assn04.txt
;; seems to help
;; > Please! Unless you have lots of time to burn, do *not write a complete infix parser* for some entire language, like Python (easy) or Java (hard)!  We just want parsing of *simple arithmetic expressions*.
;; so just **,-,* etc in the following old-example.

;; > It is not hard to write a special form, (INFIX <infix-string>), that takes a character string, parses it as an infix expression with the *usual precedence rules*, and *reduces it to Lisp*.

;; TODO
;; > Write the INFIX special form, install it in the evaluator, and demonstrate that it works.

;; https://live.ocw.mit.edu/courses/6-945-adventures-in-advanced-symbolic-programming-spring-2009/resources/assn04/
;; also doesn't have "an infix parser" by `unzip ~/Downloads/59ff71e985eb4d2ed665daf863932dda_assn04.zip -d ps4; grep 'infix' -r ps4`.

;;; book
;; > This is entirely a small matter of syntax (ha!).

;; > The work is parsing the character string to compile it into the corresponding Lisp expressions
;; same as the above "reduces it to Lisp"

;;;; implementation
;;; what to do
;; 1. need to consider "syntax"s for "simple arithmetic expressions", "precedence rules".
;; 2. mimic "cond->if".

(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "5_7_regexp_lib.scm")
(define (parse exp)
  (assert (string? exp))
  (let* ((split-res (exp-split exp split-lst))
         (partition-res 
          (append-map 
            (lambda (res)
              (exp-partition 
                res 
                partition-separtor-lst 
                primitive-symbol-re-lst)
              )
            split-res
            )
          ))
    (remove empty-str? partition-res))
  )

(define test-exp1 "b**2-4*a*c")
(define test-exp2 "(-b+sqrt(discriminant))/(2*a)")

(define test-book-exp
  "fact := lambda n:
  if n == 0
  then 1
  else n*fact(n-1)"
  )
(parse test-exp1)
(assert (equal? (parse test-exp1) '("b" "**" "2" "-" "4" "*" "a" "*" "c")))
(define expected-parsed-test-exp2 '("(" "-" "b" "+" "sqrt" "(" "discriminant" ")" ")" "/" "(" "2" "*" "a" ")"))
(assert (equal? (parse test-exp2) expected-parsed-test-exp2))
(assert (equal? (parse test-book-exp) '("fact" ":=" "lambda" "n" ":" "if" "n" "==" "0" "then" "1" "else" "n" "*" "fact" "(" "n" "-" "1" ")")))

(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "../common-lib/stack_lib.scm")
(load "../common-lib/pair_lib.scm")
(define new-triple list)
(define (triple? lst) (n:= 3 (length lst)))
(define (new-triple-with-new-3rd triple 3rd)
  (assert (triple? triple))
  (new-triple (first triple) (second triple) 3rd)
  )
(define (combine-parentheses str-lst)
  (assert (every string? str-lst))
  ;; just based on match-parentheses?
  (let ((str-cnt (length str-lst)))
    (let lp 
      ((paren-cnt 0) 
        (idx 0) 
        (paren-to-match (list))
        ;; 0. use 2 stacks to get the pairs `[ pop @stack, $pos ]`s https://stackoverflow.com/a/56239802/21294350
        ;; > push @output, [ pop @stack, $pos ]
        ;; 0. @ https://stackoverflow.com/questions/5553898/what-are-the-differences-between-in-perl-variable-declaration
        ;; 1. pop https://perlmaven.com/manipulating-perl-arrays
        ;; Here sort can be ignored
        ;; 2. @$ https://stackoverflow.com/a/37208206/21294350
        ;; 3. <=> https://www.shlomifish.org/lecture/Perl/Newbies/lecture2/useful_funcs/sort/cmp.html
        ;; sort https://perldoc.perl.org/functions/sort
        ;;;; 4. For details, see perlfunc for use, my, split (also see perlretut), push, pop, sort, say.
        ;; > # sort numerically ascending
        ;; perlop for qw, '', eq, =, ++, <=>, "" (qq//).
        ;; perlintro for $, @, ;, and 
        ;; > You can use parentheses for functions' arguments or omit them according to your personal taste.  They are only required occasionally to clarify issues of precedence.
        ;; perlref for [], ->[], @$_.
        ;;; perlsyn for "for", "if" (see Compound Statements). Notice LIST meaning "any combination of *scalar arguments* or list values".
        ;; elsif is not detailedly descried maybe assumingly functioning like other programming languages.
        ;; See "Statement Modifiers" for "for sort ...".
        ;; 0.a. One stack is at least not straightforward.
        ;; With two, one stack can be pushed and poped which just like +/- paren-cnt here.
        ;; 1. For simplicity, here only consider "(" and ")".
        ;; So just one stack without using dict.
        (left-pos-stack (make-new-stack))
        (pos-pair-stack (make-new-stack))
        )
      (if (n:<= str-cnt idx)
        (if (n:> paren-cnt 0)
          (error "redundant parentheses")
          pos-pair-stack)
        (let ((cur (list-ref str-lst idx)))
          (let ((next-idx (n:+ idx 1)))
            (cond 
              ((left-parenthesis? cur)
                (lp 
                  (n:+ paren-cnt 1) 
                  next-idx 
                  (append paren-to-match (list left-parenthesis))
                  (push! left-pos-stack idx)
                  pos-pair-stack
                  ))
              ((right-parenthesis? cur)
                (let ((paren-cnt* (n:- paren-cnt 1)))
                  (if 
                    (or 
                      (n:< paren-cnt* 0)
                      (not (equal? left-parenthesis (list-ref paren-to-match paren-cnt*))))
                    (error "matched with the wrong right str")
                    )
                  (let ((paired-left-pos (pop! left-pos-stack)))
                    (lp 
                      paren-cnt* 
                      next-idx 
                      (drop-right paren-to-match 1)
                      left-pos-stack
                      (push! pos-pair-stack (new-pair paired-left-pos idx))
                      ))
                  )
                )
              (else
                (lp paren-cnt next-idx paren-to-match left-pos-stack pos-pair-stack)
                )
              ))
          ))
      ))
  )
(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "5_7_list_lib.scm")
(match-parentheses? expected-parsed-test-exp2)
(for-each
  (lambda (pair)
    (write-line 
      (list 
        "left" (list-ref expected-parsed-test-exp2 (get-left pair))
        "right" (list-ref expected-parsed-test-exp2 (get-right pair))
        ))
    )
  (let ((paren-idx-pairs (combine-parentheses expected-parsed-test-exp2)))
    (write-line (list "paren-idx-pairs" paren-idx-pairs))
    (get-data paren-idx-pairs))
  )

(define (infix->polish str-lst)
  (assert (every string? str-lst))

  )

;;;; tests
;;; old-example from 6-945 2009
(define (quadratic a b c)
  (let ((discriminant (infix "b**2-4*a*c")))
    (infix "(-b+sqrt(discriminant))/(2*a)")))

;;; book
;; here some spaces are necessary to make codes work.
; (infix
;   "fact := lambda n:
;   if n == 0
;   then 1
;   else n*fact(n-1)")
; (fact 6) ; The Lisp procedure is now defined
; (infix "fact(5)") ; And it can be used in infix notation.
