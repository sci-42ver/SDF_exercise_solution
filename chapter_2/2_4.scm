(load "../software/sdf/combinators/function-combinators.scm")

;;; discard-argument
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

(load "2_4_chebert_utils.scm")

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

;; For the rest, I will follow the structure of chebert.

;;; curry-argument
(define ((curry-argument i) . args)
  (lambda (f)
    ;; As the above shows, pass arity until the last caller is routine and trivial.
    (assert (= (length args) (- (get-arity f) 1)))
    ;; As above, we can abstract this lambda. For simplicity, I didn't do that.
    (compose-args f (lambda (x) (list-insert args i x)))
    ))
;; The above is almost same as chebert.

((((curry-argument 2)
   'a 'b 'c)
  (lambda (x y z w)
    (list 'foo x y z w)))
 'd)
'expect-value: '(foo a b d c)

((((curry-argument 2)
   'a 'b)
  (lambda (x y z w)
    (list 'foo x y z w)))
 'd)

;;; permute-arguments
(define (permute-arguments . permspec)
  (let ((permute (make-permutation permspec)))
    (lambda (f)
      ; (define (the-combination . args)
      ;   (apply f (permute args)))
      ;; Here we need lambda to transform multiple args into one list
      (define the-combination (compose-args f (lambda args (permute args))))
      (let ((n (get-arity f)))
        (assert (= n (length permspec)))
        (restrict-arity the-combination n)
        ))))

(((permute-arguments 1 2 0 3)
  (lambda (x y z w)
    (list 'foo x y z w)))
 'a 'b 'c 'd)
'expect-value: '(foo b c a d)

(((permute-arguments 1 2 0)
  (lambda (x y z w)
    (list 'foo x y z w)))
 'a 'b 'c 'd)

(get-arity ((permute-arguments 1 2 0 3)
              (lambda (x y z w)
                (list 'foo x y z w))))