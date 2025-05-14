(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "../software/sdf/manager/load.scm")
(manage 'new 'generic-interpreter)

(write-line "2")

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

; (cd "~/SICP_SDF/SDF_exercises/chapter_5")
; (load "5_7_tokenize_lib.scm")
; (load "5_7_tokenize_tests.scm")

(define (infix->polish str)
  (assert (string? str))
  (ParsePythonDemo str)
  )

(define (infix? exp) (tagged-list? exp 'infix))
(define infix-str cadr)
(define-generic-procedure-handler g:eval
                                  (match-args infix? environment?)
                                  (lambda (expression environment)
                                    (g:eval (infix->polish (infix-str expression)) environment)))
(define == =)

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "ModularPratt.scm")

(init)

;;;; tests
;;; old-example from 6-945 2009
(define (quadratic a b c)
  (let ((discriminant (infix "b**2-4*a*c")))
    (infix "(-b+sqrt(discriminant))/(2*a)")))
;; solution for a*x^2+b*x+c
(equal? 2 (quadratic 1 -3 2))

;;; book
;; here some spaces are necessary to make codes work.
(infix
  "fact := lambda n:
  if n == 0
  then 1
  else n*fact(n-1)")

; (define fact (lambda (n) (if (== n 0) 1 (* n (fact (- n 1))))))
; (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
(equal? 720 (fact 6))
(equal? 120 (infix "fact(5)"))
