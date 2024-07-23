(load "../software/sdf/combinators/function-combinators.scm")

;; the basic idea is same but with abstraction and more readable
(define (discard-argument i)
  (assert (exact-nonnegative-integer? i))
  (lambda (f)
    (let ((m (+ (get-arity f) 1)))
      (define (discard-argument-proc . args)
        (assert (= (length args) m))
        (apply values (list-remove args i))
        )
      (restrict-arity discard-argument-proc m) ; This should be done outside `discard-argument-proc`.
      (define the-combination (compose f discard-argument-proc))
      (assert (< i m))
      ;; compose already does `restrict-arity`.
      the-combination)))

(((discard-argument 2)
  (lambda (x y z)
    (list 'foo x y z)))
 'a 'b 'c 'd)
'expect-value: '(foo a b d)

;; tests for 3 assertion.
(((discard-argument 'a)
  (lambda (x y z)
    (list 'foo x y z)))
 'a 'b 'c 'd)

(((discard-argument 2)
  (lambda (x y z)
    (list 'foo x y z)))
 'a 'b 'c)

(((discard-argument 4)
  (lambda (x y z)
    (list 'foo x y z)))
 'a 'b 'c 'd)

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

(define ((discard-argument i) f)
  (assert (exact-nonnegative-integer? i))
  (let ((m (+ (get-arity f) 1)))
    (assert (< i m))
    (restrict-arity
      ;; Here we need to check the most inner func to ensure accepting the correct param number.
      (compose-args f (curry-left*-check-arity list-remove m i))
      m ; same as 2.3, here we don't consider the general case.
      )))

(((discard-argument 2)
  (lambda (x y z)
    (list 'foo x y z)))
 'a 'b 'c 'd)
'expect-value: '(foo a b d)

;; tests for 3 assertion.
(((discard-argument 'a)
  (lambda (x y z)
    (list 'foo x y z)))
 'a 'b 'c 'd)

(((discard-argument 2)
  (lambda (x y z)
    (list 'foo x y z)))
 'a 'b 'c)

(((discard-argument 4)
  (lambda (x y z)
    (list 'foo x y z)))
 'a 'b 'c 'd)