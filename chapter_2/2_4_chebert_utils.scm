(load "../software/sdf/combinators/function-combinators.scm")
;; code base
(define (compose f g)
  (define (the-composition . args)
    (call-with-values (lambda () (apply g args))
                      f))
  ;; Here is to avoid error when using something like `. fs`.
  ;; It is ok since at last we will `(get-arity f)` to correct back.
  (restrict-arity the-composition (procedure-arity g))
  )
;; from chebert
(define (compose-multiple . fs)
  (if (null? fs)
    values
    (let ((gs (reverse fs)))
      ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Folding-of-Lists.html#index-fold_002dleft
      ;; we should not use `fold-left` since it uses one different order `elt acc`.
      ; (fold (lambda (acc elt) (compose acc elt)) (car gs) (cdr gs))
      (fold compose (car gs) (cdr gs))

      ;; fold defaults to be fold-left but with one different connection order.
      ; (fold-left cons* '() '(a b c))
      ; (fold cons* '() '(a b c))
      )))

(define (compose-args f arg-f)
  (compose-multiple f values* arg-f))

(define (values* args) (apply values args))

(define (curry-left*-check-arity f arity . args)
  ((apply (curry-arguments*-check-arity 0) args) f arity))

(define (((curry-arguments*-check-arity position) . fixed-args) f arity)
  (assert (exact-nonnegative-integer? position))
  (display ((lambda args
              (list-insert fixed-args position args)) 'a 'b 'c 'd))
  ; '((a b c d) 2)

  ; (call-with-values (lambda () (apply values '((a b c d) 2)))
  ;   list-remove)

  ; (display (((compose-args f (lambda args
  ;                   (list-insert fixed-args position args))) 'a 'b 'c 'd)))
  ; (display ((compose-args f (lambda args
  ;                   '((a b c d) 2)))))

  ;; Here the args the most inner accepted param.
  (compose-args f (lambda args ; here we should use args instead of `args` to accept multiple args.
                    (assert (= (length args) arity))
                    (list-insert fixed-args position args))))

(define swap-args
  (permute-arguments 1 0))
