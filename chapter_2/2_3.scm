(load "../software/sdf/combinators/function-combinators.scm")

;; Here I didn't follow 2.2 by defining one general `parallel-combine` since it is one routine work.
;; Same for `parallel-combine`.
(define (parallel-combine h f g)
  ; (assert (= (get-arity h) 2)) ; to avoid error when using `list` although we can do as 2.2.
  (compose h (parallel-apply f g)))

;; 1. Here I just mimic spread-apply
;; This may be probably similar to the code base implementation.
(define (parallel-apply f g)
  (let ((n (get-arity f))
        (m (get-arity g)))
    ;; as mbillingr better use the more general `equal?`
    (assert (= n m))
    (define (the-combination . args)
      (assert (= (length args) n))
      ;; mbillingr returns `list` instead of `values` although probably `append` can manipulate both.
      (let-values ((fv (apply f args))
                   ; (gv (apply g (list-tail args n))))
                   (gv (apply g args)))
        (apply values (append fv gv)))) ; This combines 2 values into 1 by first combining them into one list.
    (restrict-arity the-combination n)))

((parallel-combine list
                   (lambda (x y z)
                     (list 'foo x y z))
                   (lambda (u v w)
                     (list 'bar u v w)))
 'a 'b 'c)
'expect-value: '((foo a b c) (bar a b c))
