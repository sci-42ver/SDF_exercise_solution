(load "2_4_chebert_utils.scm")

(define (permute-arguments . permspec)
  (let ((permute (make-permutation permspec)))
    (lambda (f)
      ; (define (the-combination . args)
      ;   (apply f (permute args)))
      (let ((n (get-arity f)))
        (assert (= n (length permspec)))
        ;; Here we need lambda to transform multiple args into one list
        (define the-combination (compose-args f (lambda args (permute args))))
        (restrict-arity the-combination n)
        ))))

(((permute-arguments 1 2 0 3)
  (lambda (x y z w)
    (list 'foo x y z w)))
 'a 'b 'c 'd)
'expect-value: '(foo b c a d)