(define x 42)
(define double
  (lambda () ((lambda () (+ x x))))
  )
(define (proc)
  (define x 3)
  (double))
(proc)
;Value: 84
