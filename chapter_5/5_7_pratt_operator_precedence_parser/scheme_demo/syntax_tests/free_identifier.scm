; https://www.gnu.org/software/guile/manual/html_node/Defmacros.html
(define x 42)
(define-macro (double)
  `((lambda () (+ x x)))
  )
(define (proc)
  (define x 3)
  (double))
(proc)
; $1 = 6
