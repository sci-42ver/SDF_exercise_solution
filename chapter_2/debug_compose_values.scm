(load "../software/sdf/combinators/function-combinators.scm")

(define (orig-spread-apply f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        ;; here is 2 sub-values combined into one values
        (values (apply f (list-head args n))
                (apply g (list-tail args n)))
        )
      (restrict-arity the-combination t))))

; (define (spread-apply f g)
;   (let ((n (get-arity f))
;         (m (get-arity g)))
;     (let ((t (+ n m)))
;       (define (the-combination . args)
;         (assert (= (length args) t))
;         (let-values ((fv (apply f (list-head args n)))
;                      (gv (apply g (list-tail args n))))
;           (apply values (append fv gv))))
;       (restrict-arity the-combination t))))

(define (compose f g)
  (define (the-composition . args)
    (call-with-values (lambda () (apply g args))
                      f))
  (restrict-arity the-composition (get-arity g)))

(define (orig-spread-combine h f g)
  (compose h (orig-spread-apply f g)))

((orig-spread-combine list
                      (lambda (x y) (values x y))
                      (lambda (u v w) (values w v u)))
 'a 'b 'c 'd 'e)

((spread-combine list
                 (lambda (x y) (values x y))
                 (lambda (u v w) (values w v u)))
 'a 'b 'c 'd 'e)
