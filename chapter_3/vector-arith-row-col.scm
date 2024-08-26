(load "~/SICP_SDF/SDF_exercises/software/sdf/combining-arithmetics/vector-arith.scm")
;; >  How does this affect the design of the vector package?
;; we must use something like tag to differentiate.

;; Then we change make-vec-operation since IMHO this is one more elegant change to manipulate with the correct data.
;; This is just "stripping off" said in SICP.
(define (vector-data vec)
  (cadr vec))
(define (general-vector? object)
  (and (n:list? object)
       (n:= 2 (length object))
       (or (eq? 'col (car object)) (eq? 'row (car object)))
       (vector? (vector-data object))))
(register-predicate! general-vector? 'general-vector)
(define (make-col-vec data)
  (list 'col data))
(define (row-vector? object)
  (and (general-vector? object)
       (eq? 'row (car object))))
(define (col-vector? object)
  (and (general-vector? object)
       (eq? 'col (car object))))

(define (make-vec-operation operator applicability procedure)
  (list 'operation operator applicability
    (lambda data 
      (apply procedure 
        (map 
          (lambda (datum) 
            (if (general-vector? datum)
              (vector-data datum)
              datum)) 
          data)))))

(define (vector-extender component-arithmetic)
  (let ((component-predicate
         (arithmetic-domain-predicate component-arithmetic))
        (component-proc
         (lambda (operator)
           (operation-procedure
            (arithmetic-operation operator component-arithmetic)))))
    (let ((+ (component-proc '+))
          (- (component-proc '-))
          (* (component-proc '*))
          (negate (component-proc 'negate))
          (sqrt (component-proc 'sqrt)))
      (let ((dot-product (dot-product-maker + *))
            (left-scalar-product (left-scalar-product-maker *))
            (right-scalar-product (right-scalar-product-maker *))
            (magnitude (vector-magnitude-maker + * sqrt)))
        (make-arithmetic 'vector
          (disjoin component-predicate general-vector?)
          (list component-arithmetic)
          (lambda (name component-constant)
            ;; no identity by identity-name->getter
            (default-object))
          (lambda (operator component-operation)
            (case operator
              ((+)
               (make-vec-operation operator
                               (all-args (operator-arity operator)
                                         general-vector?)
                               (vector-element-wise +)))
              ((-)
               (make-vec-operation operator
                               (all-args (operator-arity operator)
                                         general-vector?)
                               (vector-element-wise -)))
              ((*)
               (operation-union
                operator
                (make-vec-operation operator
                                (all-args (operator-arity operator)
                                          general-vector?)
                                dot-product)
                (make-vec-operation operator
                                (match-args component-predicate general-vector?)
                                left-scalar-product)
                (make-vec-operation operator
                                (match-args general-vector? component-predicate)
                                right-scalar-product)))
              ((negate)
               (make-vec-operation operator
                               (all-args (operator-arity operator)
                                         general-vector?)
                               (vector-element-wise negate)))
              ((magnitude)
               (make-vec-operation operator
                               (all-args (operator-arity operator)
                                         general-vector?)
                               magnitude))
              (else
               (make-vec-operation operator
                               (any-arg (operator-arity operator)
                                        general-vector?
                                        component-predicate)
                               (lambda args
                                 (error "Don't know how to "
                                        operator args)))))))))))