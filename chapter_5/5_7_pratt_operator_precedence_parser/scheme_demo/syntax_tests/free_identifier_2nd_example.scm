(define x 42)

(define-syntax double
  (syntax-rules ()
    ((_) ((lambda () (+ x x))) )))

(define (proc)
  (define x 3)
  (double))

(proc)