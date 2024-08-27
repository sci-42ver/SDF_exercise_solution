;; by searching "partial", no sample implementation
(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'automatic-differentiation)
(load "test-lib.scm")

;; from examples.scm
(define (foo x y)
  (square (+ (square x) y)))
(assert-predicate equal? (((partial 0) foo) 'x 'y) '(* (* 2 (+ (square x) y)) (* 2 x)))

;; almost same as `((curry-argument i) . args)` structure.
;; Also see https://www.matthiaspreu.com/posts/lambda-calculus-fundamentals/#currying---application-of-multiple-arguments
;; where we just pass arg one by one.

;; > Draw a diagram of how the data must flow.
;; rest-args and then the inserted arg.
;; > Use currying to fix the arguments that are held constant
;; i.e. the above `arg))`.

;; > producing a one-argument procedure that the ordinary derivative will be applied to.
;; It may mean we apply `((partial i) f)` with f be the "one-argument procedure". But then "args" should be arg.
;; So IMHO here I think of the author's meaning as making "the ordinary derivative" be "a one-argument procedure".
(load "~/SICP_SDF/SDF_exercises/software/sdf/combinators/function-combinators.scm")
(define (((partial i) f) . args)
  (define (the-derivative arg)
    (let ((total-arg-num (+ (length args) 1)))
      (if (not (< i total-arg-num))
          (error "Not enough arguments for PARTIAL" i f args))
      (let* ((dx (make-new-dx))
            (value
              (((apply (curry-argument i) args) f) 
                (d:+ arg (make-infinitesimal dx)))))
        (extract-dx-part value dx))))
  the-derivative)

; ((((partial 0) foo) 'y) 'x)
(assert-predicate equal? ((((partial 0) foo) 'y) 'x) '(* (* 2 (+ (square x) y)) (* 2 x)))