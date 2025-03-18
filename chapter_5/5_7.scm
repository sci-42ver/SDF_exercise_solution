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
(define (check-split-lst-and-partition-lst . sre-lsts)
  (for-each 
    (lambda (sre-lst)
      (assert (sre-lst? sre-lst))
      )
    sre-lsts)
  )
(define (parse exp)
  (%parse exp split-lst partition-separtor-lst primitive-symbol-re-lst))
(define (%parse exp split-lst partition-separtor-lst skipped-re-lst)
  (assert (string? exp))
  (check-split-lst-and-partition-lst split-lst partition-separtor-lst skipped-re-lst)
  (let* ((split-res (exp-split exp split-lst))
         (partition-res 
          (append-map 
            (lambda (res)
              (exp-partition 
                res 
                partition-separtor-lst 
                skipped-re-lst)
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
(assert (equal? (parse test-exp1) '("b" "**" "2" "-" "4" "*" "a" "*" "c")))
(define expected-parsed-test-exp2 '("(" "-" "b" "+" "sqrt" "(" "discriminant" ")" ")" "/" "(" "2" "*" "a" ")"))
(assert (equal? (parse test-exp2) expected-parsed-test-exp2))
(assert (equal? (parse test-book-exp) '("fact" ":=" "lambda" "n" ":" "if" "n" "==" "0" "then" "1" "else" "n" "*" "fact" "(" "n" "-" "1" ")")))

(define test-exp4 "b**m-n*a*c")
(assert (equal? (parse test-exp4) '("b" "**" "m" "-" "n" "*" "a" "*" "c")))

;; not used
; (cd "~/SICP_SDF/SDF_exercises/chapter_5")
; (load "5_7_naive_algorithm_for_operator_precedence_parser/5_7_parenthesis_lib.scm")
; (match-parentheses? expected-parsed-test-exp2)

; (for-each
;   (lambda (pair)
;     (write-line 
;       (list 
;         "left" (list-ref expected-parsed-test-exp2 (get-left-idx pair))
;         "right" (list-ref expected-parsed-test-exp2 (get-right-idx pair))
;         ))
;     )
;   (let ((paren-idx-pairs (parenthesis-idx-pair-stack expected-parsed-test-exp2)))
;     (write-line (list "paren-idx-pairs" paren-idx-pairs))
;     (get-data paren-idx-pairs))
;   )

; (combine-non-application-parentheses expected-parsed-test-exp2)

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
