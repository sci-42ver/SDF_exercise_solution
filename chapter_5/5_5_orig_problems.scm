(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "../software/sdf/manager/load.scm")
(manage 'new 'generic-interpreter)

;; implement for this exercise
(define-generic-procedure-handler g:apply
                                  (match-args strict-primitive-procedure?
                                              operands?
                                              environment?)
                                  (lambda (procedure operands calling-environment)
                                    (let ((operands* (eval-operands operands calling-environment)))
                                      (write-line (list procedure "is applied to" (map pp operands*)))
                                      (apply-primitive-procedure procedure
                                                                 operands*)
                                      )
                                    ))

;;; test
;; set the init env and go into REPL
(init)
(define (test1)
  (define fib
    (lambda (n)
      (cond 
        ((= n 0) 0)
        ((= n 1) 1)
        ((> n 1) (+ (fib (- n 1)) (fib (- n 2))))
        (else (error (list "wrong arg for fib" n)))
        )
      )
    )
  (equal? (map fib '(10)) '(55)))
(test1)
; (compound-procedure (n) (cond ((= n 0) 0) ((= n 1) 1) ((> n 1) (+ (fib (- n 1)) (fib (- n 2)))) (else (error (list "wrong arg for fib" n)))) <procedure-environment>)
; (10)
; (#[arity-dispatched-procedure 13] "is applied to" (#!unspecific #!unspecific))
; ;The object #[*compound-procedure 14] is not applicable.

;; expected
; #t

(define (test2)
  (define proc1 
    ((lambda ()
       (define y 3)
       (lambda (x) (* x y))
       )))
  (equal? 
    (map
      (lambda (a) (+ a (proc1 3))) ; (* 3 3)
      '(1)
      )
    '(10)
    ))
(test2)
;; expected
; #t
