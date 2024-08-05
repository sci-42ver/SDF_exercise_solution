(load "../software/sdf/combinators/function-combinators.scm")

;; warm-up
;; based on 2.1
(define (compose-mod-general f g)
  (let ((nf (procedure-arity f)) ; implied by 2.1 solution by nbardiuk.
        (ng (procedure-arity g)))
    (let ((nf_min (procedure-arity-min nf))
          (ng_min (procedure-arity-min ng))
          (ng_max (procedure-arity-max ng)) ; by MIT_Scheme_Reference only this can be #f
          )
      (assert (<= nf_min 1)) ; point 1
      (define (the-composition . args)
        ;; https://groups.csail.mit.edu/mac/ftpdir/scheme-reports/r5rs-html/r5rs_6.html#IDX71
        ;; > a newly allocated list of the actual arguments

        ;; point 2
        (let ((arg_len (length args)))
          (if (not (equal? ng_max #f))
            (assert (and (<= arg_len ng_max) (>= arg_len ng_min)))
            (assert (>= arg_len ng_min))))
        (f (apply g args)))
      ;; point 3
      (restrict-arity the-composition ng)
      ;; from MIT_Scheme_Reference
      ; (make-primitive-procedure the-composition ng)
      the-composition)))

; ((compose-mod-general (lambda (x) (list 'foo x))
;                       (lambda (x) (list 'bar x)))
;     'z)

; ((compose-mod-general -
;                       (lambda (x) (square x)))
;     3)

; ((compose-mod-general (lambda (x) (+ x (square x)))
;                       -)
;     3)

; ((compose-mod-general (lambda (x) (+ x (square x)))
;                       (lambda (x) (+ x (square x))))
;     3 4)

(define test_general_compose 
  (compose-mod-general (lambda (x) (+ x (square x)))
                       -))
;; MIT_Scheme_Reference is written by Chris Hanson which is also the author of SDF. So probably procedure-arity is based on hash-table.
(procedure-arity test_general_compose)

((compose-mod-general test_general_compose
                      (lambda (x) (+ x (square x))))
 3)
((lambda (x) (+ (- (+ x (square x))) (square (+ x (square x))))) 3)

;; ps01 2.b
;; > YOUR JOB is to reengineer the arity interface (RESTRICT-ARITY and GET-ARITY in function-combinators.scm)
;; Here `RESTRICT-ARITY` just set the table so we don't need to change it.
;; GET-ARITY -> get-arity-general.
(assert (= (cos (+ 3 4)) ((compose-mod-general cos +) 3 4)))

;; > Sketch a plan for how to extend the combinators to use the more general arities.
;; > What choices will you have to make in reformulating spread-combine?
;; change all procedures related with arity.
;; > Note that you may not always be able to use arithmetic on the arities.
;; Since maybe #f

;; borrowed from mbillingr which is similar to nbardiuk's in-arity?.
(define (check-arity arity n-args)
  (assert (>= n-args (procedure-arity-min arity)))
  (if (procedure-arity-max arity)
    (assert (<= n-args (procedure-arity-max arity)))))

;; based on the code base
;; Also see mbillingr.
(define (get-arity-general proc)
  (or (hash-table-ref/default arity-table proc #f) ; take value from table otherwise default (0 . #f)
      (procedure-arity proc)))

(define (simple-spread-combine h f g)
  (check-arity (get-arity-general h) 2) ; point 1
  (let* ((n (get-arity-general f)) 
         (m (get-arity-general g))
         (n_min (procedure-arity-min n))
         (n_max (procedure-arity-max n))
         (m_min (procedure-arity-min m))
         (m_max (procedure-arity-max m))
         (res_min (+ n_min m_min))
         ;; similar to join-arities in nbardiuk
         (plus_arity (lambda (a1_min a1_max a2_min a2_max) 
                       (if (and a1_max a2_max)
                         (make-procedure-arity res_min (+ a1_max a2_max))
                         (make-procedure-arity res_min #f)))))
    (assert (and n_max (= n_min n_max))) ; ensure we can split the parameter list.
    (let ((t (plus_arity n_min n_max m_min m_max)))
      (define (the-combination . args)
        ; (assert (= (length args) t))
        ;; by https://groups.csail.mit.edu/mac/ftpdir/scheme-reports/r5rs-html/r5rs_6.html#IDX71 we only need to check the lower bound.
        (check-arity t (length args)) ; point 2
        (h (apply f (list-head args n_min))
           (apply g (list-tail args n_min))))
      (restrict-arity the-combination t) ; point 3
      )))

((simple-spread-combine
   list
   (lambda (x y) (list 'foo x y))
   (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c 'd 'e)

((simple-spread-combine 
   + 
   (lambda (x y) (* x y)) 
   (lambda (x y) (+ x y)))
 1 2 3 4)

(define test_sc (simple-spread-combine 
                  + 
                  (lambda (x y) (* x y)) 
                  (lambda (x y) (+ x y))))

;; point 1
((simple-spread-combine 
   (lambda (x) x)
   (lambda (x y) (* x y))
   (lambda (x y) (+ x y)))
 1 2 3 4)

;; point 2
((simple-spread-combine 
   + 
   (lambda (x y) (* x y)) 
   (lambda (x y z) (+ x y)))
 1 2 3 4)

;; point 3
(get-arity-general test_sc)
((simple-spread-combine 
   + 
   test_sc
   (lambda (x y) (+ x y)))
 1 2 3 4 5 6)

;; ensure we are able to split param. This will throw error.
((simple-spread-combine 
   + 
   +
   (lambda (x y) (+ x y)))
 1 2 3 4)
