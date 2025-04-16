;; modified to show that x is searched in the top-level env
(define-syntax double
  (syntax-rules ()
    ( (_) ((lambda () (+ x x))) ) ))

; equal to
; (define-syntax double
;   (lambda (x)
;     (syntax-case x ()
;       ((_) 
;         #'((lambda () (+ x x)))
;         ))
;     ))

(define x 42)

(set! x 3)
(double)