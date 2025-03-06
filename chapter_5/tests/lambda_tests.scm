(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "../software/sdf/manager/load.scm")
(manage 'new 'generic-interpreter)

(define test-exp1 '(lambda ()))
(define test-exp2 '(lambda () a))
(define test-exp3 '(lambda () a b))

(define (lambda->proc->lambda expression)
  (let ((proc 
          (make-compound-procedure
            (lambda-parameters expression)
            (lambda-body expression)
            'environment)))
    (make-lambda
      (procedure-parameters proc)
      (procedure-body proc)
      ) 
    ))
(lambda->proc->lambda test-exp1)
(lambda->proc->lambda test-exp2)
(lambda->proc->lambda test-exp3)

(define (make-lambda parameters body)
  (cons 'lambda
        (cons parameters
              (cond 
                ((begin? body) (begin-actions body))
                ((null? body) body)
                (else (list body))))))
(lambda->proc->lambda test-exp1)
(lambda->proc->lambda test-exp2)
(lambda->proc->lambda test-exp3)

;;; sequence->begin usages related with lambda.
(lambda ())
;Ill-formed syntax: (lambda ())
;; same errors as the above due to being syntactic sugar of lambda.
(let ((var 'test)))
;; i.e.
((lambda (var)) 'test)

(eval '(lambda () ()) (the-environment))

;;; other usages of sequence->begin.
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Conditionals.html#index-cond-1
;; > If the selected clause contains only the predicate and no expressions, cond returns the value of the predicate as the result.
(cond ((number? 1)))
(cond->if '(cond ((number? 1))))

;;; correct implementation
;; Based on the above sequence->begin should manipulate null specifically
(define (sequence->begin seq null-thunk)
  (cond ((null? seq) (null-thunk)) ; modified
        ((null? (cdr seq)) (car seq))
        (else
          (make-begin
            (append-map (lambda (exp)
                          (if (begin? exp)
                            (begin-actions exp)
                            (list exp)))
                        seq)))))
(define (let-body let-exp) 
  (sequence->begin 
    (cddr let-exp) 
    (lambda () (error (list "invalid" let-exp)))))
(define (lambda-body lambda-exp)
  (let ((full-body (cddr lambda-exp)))
    (sequence->begin 
      full-body
      (lambda () (error (list "invalid" lambda-exp)))
      )))
(define (cond-clause-consequent clause)
  (sequence->begin 
    (cdr clause)
    ;; special
    (lambda () (car clause))
    ))

(lambda->proc->lambda test-exp1)
; ("invalid" (lambda ()))
;; not allowed
(cond->if '(cond ((number? 1))))
;Value: (if (number? 1) (number? 1) unspecified)
