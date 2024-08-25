(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'generic-procedures)
(load "~/SICP_SDF/SDF_exercises/software/sdf/combining-arithmetics/vector-arith.scm")

(define (mat-data mat)
  (cadr mat))
;; > be able to know the number of rows and the number of columns in a matrix
(define (mat-row mat)
  (caddr mat))
(define (mat-col mat)
  (cadddr mat))
(define (ensure-same-col mat-data)
  (let ((col-num-vec (vector-map vector-length mat-data)))
    (every 
      (lambda (col-num) (= col-num (car col-num-vec)))
      col-num-vec)))
(define (make-matrix mat-data)
  (assert (ensure-same-col mat-data))
  (list 'matrix mat-data 
    (vector-length mat-data) 
    (vector-length (vector-ref mat-data 0))))
;; mimic operation?
(define (matrix? object)
  (and (n:list? object)
       (n:= 3 (length object))
       (eq? 'matrix (car object))
       ;; > A reasonable representation of a matrix is a Scheme vector in which each element is a Scheme vector representing a row.
       (vector? (mat-data object))
       (every vector? (mat-data object))
       (number? (mat-row object))
       (number? (mat-col object))))

(register-predicate! matrix? 'matrix)
;; just mimic matrix-extender

;; > since matrix multiplication is defined only if the number of columns in the first matrix is equal to the number of rows in the second one.
(define (ensure-row-col-compatibility mat1 mat2)
  (n:= (mat-col mat1) (mat-row mat2)))

(define (scalar? data)
  (or (symbolic? data) (number? data)))

;; misc helper
(define (mat->2-level-list mat)
  (vector->list (vector-map vector->list mat)))
(define (2-level-list->mat lst)
  (list->vector (map list->vector lst)))

(load "vector-arith-row-col.scm")
(define (mat->vec mat)
  (cond 
    ((n:= 1 (mat-row object)) (vector-ref (mat-data object) 0))
    (predicate2 consequent2)))

(define (vec->mat vec)
  (cond 
    ((col-vector? object) (make-matrix (vector-map vector vec)))
    ((row-vector? object) (make-matrix (vector vec)))))

(define (scalar->mat scalar)
  (list->vector
    (map 
      (lambda (idx) 
        (apply 
          vector 
          (append (make-list idx 0) (list scalar) (make-list (- scalar 1 idx) 0)))) 
      (iota scalar))))

;; mimic SICP
(load "sicp-matrix-lib.scm")
(define (transpose-mat m) 
  (2-level-list->mat (transpose (mat->2-level-list mat))))

;; mimic combining-arithmetics/vector-arith.scm
(define (left-row-vector-product-maker + *)
  (define (vector-product v mat)
    (let ((transpose-mat-inst (transpose-mat mat))
          (dot-product (dot-product-maker + *)))
      ;; similar to SICP exercise 2.37 matrix-*-vector
      ;; Here we use `dot-product` from vector-arith.scm.
      (vector-map (lambda (col) (dot-product col v))
                  transpose-mat-inst)))
  vector-product)

(define (matrix-product-maker + *)
  (let ((left-row-vector-product (left-row-vector-product-maker + *)))
    (define (matrix-product mat1 mat2)
      ;; similar to SICP exercise 2.37 matrix-*-matrix
      ;; but here `transpose` is done in left-row-vector-product-maker.
      (vector-map (lambda (row) (left-row-vector-product-maker row mat2)) mat1))
  matrix-product))

(define (left-product-maker transformer + *)
  (let ((matrix-product (matrix-product-maker + *)))
    (define (product v mat)
      (matrix-product (transformer v) mat))
    product))

(define (right-product-maker transformer + *)
  (let ((matrix-product (matrix-product-maker + *)))
    (define (product mat v)
      (matrix-product (transformer v) mat))
    product))



;; > Make sure that your multiplier can multiply a matrix with a scalar or with a vector.
;; Here scalar I only consider symbolic? and number? excluding function?.
;; > For matrices to play well with vectors you probably need to distinguish row vectors and column vectors.
;; add one row/col tag.
;; for simplicity, I transform them to matrix here.
(define (matrix-extender component-arithmetic)
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
      (let ((matrix-product (matrix-product-maker + *))
            (left-scalar-product (left-product-maker scalar->mat + *))
            (right-scalar-product (right-product-maker scalar->mat + *))
            (left-vector-product (left-product-maker vec->mat + *))
            (right-vector-product (right-product-maker vec->mat + *)))
        (make-arithmetic 'matrix
          (disjoin component-predicate matrix?)
          (list component-arithmetic)
          (lambda (name component-constant)
            ;; no identity by identity-name->getter
            (default-object))
          (lambda (operator component-operation)
            (case operator
              ((+)
               (make-operation operator
                               (all-args (operator-arity operator)
                                         matrix?)
                               (matrix-element-wise +)))
              ((-)
               (make-operation operator
                               (all-args (operator-arity operator)
                                         matrix?)
                               (matrix-element-wise -)))
              ((*)
               (operation-union
                operator
                (make-operation operator
                                (all-args (operator-arity operator)
                                          matrix?)
                                matrix-product)
                (make-operation operator
                                (match-args scalar? matrix?)
                                left-scalar-product)
                (make-operation operator
                                (match-args matrix? scalar?)
                                right-scalar-product)
                (make-operation operator
                                (match-args general-vector? matrix?)
                                left-vector-product)
                (make-operation operator
                                (match-args matrix? general-vector?)
                                right-vector-product)))
              ((negate)
               (make-operation operator
                               (all-args (operator-arity operator)
                                         matrix?)
                               (matrix-element-wise negate)))
              (else
               (make-operation operator
                               (any-arg (operator-arity operator)
                                        matrix?
                                        component-predicate)
                               (lambda args
                                 (error "Don't know how to "
                                        operator args)))))))))))

(define (install-specific-generic-arithmetic)
  (let ((g
        (make-generic-arithmetic make-simple-dispatch-store)))
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g
                                (extend-arithmetic matrix-extender 
                                  (extend-arithmetic vector-extender 
                                    (extend-arithmetic symbolic-extender numeric-arithmetic))))
    (install-arithmetic! g)))
(install-specific-generic-arithmetic)