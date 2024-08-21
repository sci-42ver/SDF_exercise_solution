;; 1. chebert no implementation by searching "bool"
;; 2. compared with 6.945_assignment_solution, I generalize `(-)`.
;; 3. Also see code base common/boolean-arith.scm.
;; By `+-like` etc. here we only need to define binary operation.
;; Its generalized - throws errors.
(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'combining-arithmetics)

;; > Use this to make a boolean arithmetic package that can be *combined* with the arithmetics we have.
;; See `add-arithmetics` where we choose one arithmetic to dispatch by `disjoin` and `operation-union` (constant-union must choose the first which may be inappropriate as the book says)

;; > When an arithmetic is installed, the binary operators +, * , - , and / are generalized to be n-ary operators.
;; > The unary application (- operand) is transformed by the installer into ( negate operand).
;; See `--like`
(define boolean-arithmetic
  (make-arithmetic 'boolean boolean? '()
                   (lambda (name)
                     (case name
                       ((additive-identity) #f)
                       ((multiplicative-identity) #t)
                       (else (default-object))))
                   (lambda (operator)
                     (let ((procedure
                             (case operator
                               ;; notice https://stackoverflow.com/q/78774181/21294350
                               ;; otherwise (lambda args (apply or args)) will cause ";Classifier may not be used as an expression: #[classifier-item 12]".
                               ((+) (lambda args (reduce (lambda (a b) (or a b)) #f args)))
                               ;; Here I allow it to accept multiple arguments as `--like` needs although I don't know whether it is appropriate.
                               ((-) (lambda args (map not args)))
                               ((*) (lambda args (reduce (lambda (a b) (and a b)) #t args)))
                               ((negate) (lambda (arg) (not arg)))
                               (else
                                 (lambda args
                                   (error "Operator undefined in Boolean"
                                          operator))))))
                       (simple-operation operator boolean? procedure))
                     ; (simple-operation operator boolean? (lambda args (map not args)))
                     )))

(install-arithmetic! boolean-arithmetic)

(define (test)
  (assert (not (- #t)))
  (assert (equal? '(#f #t) (- #t #f)))
  (assert (not (* #t #f)))
  (assert (+ #t #f))
  (assert (+ #t #f #t #f)))
(test)

(load "~/SICP_SDF/SDF_exercises/software/sdf/common/boolean-arith.scm")
(test)
