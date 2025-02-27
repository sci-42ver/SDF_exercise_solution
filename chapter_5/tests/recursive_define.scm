(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "../software/sdf/manager/load.scm")
(manage 'new 'generic-interpreter)

;; this is one demo for footnote 9 

;; see https://srfi.schemers.org/srfi-219/srfi-219.html
;; 0. > If the Scheme implementation supports attaching declarations such as documentation strings or optimization settings to procedures
;; "documentation strings" see https://stackoverflow.com/a/16106745/21294350
;; 1. 

;;; 0. IMHO better to get both variable and value during the iteration for ~~efficiency~~ simplicity.
;;; 1. TODO https://en.wikipedia.org/wiki/Naming_convention_(programming)#Examples_of_multiple-word_identifier_formats UpperCamelCase ArgLst may be a bit inappropriate here.
(define create-var-ArgLst-pair cons)
(define get-var car)
(define get-ArgLst cdr)
;; See SRFI Specification for the underlying logic here.
;; 0. ArgLst is something like (args2 args1 args).
(define (definition-var-ArgLst-pair defn)
  (define get-args cdr)
  (define get-symbol car)
  (let lp ((ArgLst '()) (possible-symbol (cadr defn)))
    (if (variable? possible-symbol)
      (create-var-ArgLst-pair possible-symbol ArgLst)
      (lp (cons (get-args possible-symbol) ArgLst) (get-symbol possible-symbol))
      ))
  )
(define (definition-variable defn)
  (get-var (definition-var-ArgLst-pair defn))
  )

(define (definition-value defn)
  (let* ((pair (definition-var-ArgLst-pair defn))
         (ArgLst (get-ArgLst pair))
         (body (cddr defn)))
    (if (null? ArgLst)
      (caddr defn)
      (fold-right
        (lambda (elm res)
          (make-lambda elm res)
          )
        (make-begin body)
        ArgLst
        )
      ))
  )

(define (assert-var-ArgLst define-exp expected-var expected-val)
  (assert (equal? (definition-variable define-exp) expected-var))
  (assert (equal? (definition-value define-exp) expected-val))
  )

(define get-exp car)
(define get-expected-var cadr)
(define get-expected-val caddr)
(define (assert-define-proc triple)
  (assert-var-ArgLst 
    (get-exp triple) 
    (get-expected-var triple)
    (get-expected-val triple))
  )

;; Here . arg syntax is not supported by the code base.
(define test-exp1 
  (list 
    '(define (((symbol args2) args1) args) expr end-expr)
    'symbol
    '(lambda (args2) (lambda (args1) (lambda (args) expr end-expr)))
    ))
; (definition-value test-exp1)
(assert-define-proc test-exp1)

(define test-exp2
  (list 
    '(define symbol expr)
    'symbol
    'expr
    ))
(assert-define-proc test-exp2)
