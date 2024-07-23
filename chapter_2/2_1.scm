(load "../software/sdf/combinators/function-combinators.scm")

;;; compose
(define (first-compose f g)
  (lambda args
    (f (apply g args))))

;; following Figure 2.1.
;; IMHO simple-compose already does this.

;; Also see mbillingr compose2 although it have less assertions.
;; chebert compose2 have no assertion by searching "(define (compose". Also for parallel-combine.
(define (compose-mod f g)
  (let ((nf (get-arity f))
        (ng (get-arity g)))
    (assert (= nf 1)) ; point 1
    (define (the-composition . args)
      ;; https://groups.csail.mit.edu/mac/ftpdir/scheme-reports/r5rs-html/r5rs_6.html#IDX71
      ;; > a newly allocated list of the actual arguments

      ;; We can use `check-arity` in mbillingr.
      (assert (= (length args) ng)) ; point 2
      (f (apply g args)))
    (restrict-arity the-composition ng) ; point 3
    the-composition))

;; test for 3 points
;; > they check their components to make sure that the arities are compatible;
;; TODO here I only check f_arg=1 since n is unknown.
;; This is the internal property.
((compose-mod (lambda (x) x) (lambda (x) x)) 1)
((compose-mod (lambda (x y) x) (lambda (x) x)) 1)

;; > the combination they construct checks that it is given the correct number of arguments when it is called;
;; This depends the outer input.
((compose-mod (lambda (x) x) (lambda (x y) x)) 1 2)
((compose-mod (lambda (x) x) (lambda (x y z) x)) 1 2)

;; As nbardiuk (again less assertions, same for parallel-combine), we can also use `get-arity` to test.
(define test_compose (compose-mod (lambda (x) (square x)) (lambda (x y) x)))
((compose-mod (lambda (x) (square x)) test_compose) 2 3)
((compose-mod (lambda (x) (square x)) test_compose) 2 3 4)

;;; parallel-combine
(define (first-parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  the-combination)

;; Figure 2.2
;; changed based on the code base
;; almost same as mbillingr
(define (simple-parallel-combine h f g)
  (let ((n1 (get-arity f))
        (n2 (get-arity g))
        (n3 (get-arity h)))
    ;; point 1 (2 lines)
    (assert (= n1 n2))
    (assert (= n3 2))
    (define (the-combination . args)
      ;; point 2
      (assert (= (length args) n1))
      (h (apply f args) (apply g args)))
    ;; point 3
    (restrict-arity the-combination n1)
    the-combination))

(define 2_ary_list (lambda (x y) (list x y)))

((simple-parallel-combine
   ; list ; not used since it has 0,#f for `procedure-arity`. See MIT_Scheme_Reference
   2_ary_list
   (lambda (x y z) (list 'foo x y z))
   (lambda (u v w) (list 'bar u v w))) 
 'a 'b 'c)

;; test point 1
((simple-parallel-combine
   2_ary_list
   (lambda (A x y z) (list 'foo x y z))
   (lambda (u v w) (list 'bar u v w))) 
 'a 'b 'c)

((simple-parallel-combine
   (lambda (x) x)
   (lambda (x y z) (list 'foo x y z))
   (lambda (u v w) (list 'bar u v w))) 
 'a 'b 'c)

;; test point 2
((simple-parallel-combine
   2_ary_list
   (lambda (x y z) (list 'foo x y z))
   (lambda (u v w) (list 'bar u v w))) 
 'a 'b 'c 'd)

;; test point 3
(define test_pc 
  (simple-parallel-combine
    2_ary_list
    (lambda (x y z) (list 'foo x y z))
    (lambda (u v w) (list 'bar u v w))))
((simple-parallel-combine
   2_ary_list
   test_pc
   (lambda (u v w) (list 'bar u v w))) 
 'a 'b 'c)
;Value: (((foo a b c) (bar a b c)) (bar a b c))

((simple-parallel-combine
   2_ary_list
   test_pc
   (lambda (A u v w) (list 'bar u v w))) 
 'a 'b 'c)
