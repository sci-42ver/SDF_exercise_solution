; https://www.gnu.org/software/guile/manual/html_node/Defmacros.html
;; https://stackoverflow.com/a/79520230/21294350
(define x 42) ; free identifier
(define-macro (double)
              `((lambda () (+ x x))) ; should get free identifier due to referential transparency but not
              )
(define (proc)
  (define x 3) ; bound identifier
  (double))
(proc)
; $1 = 6
