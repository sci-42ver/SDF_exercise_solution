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
  (let ((col-num-lst (vector->list (vector-map vector-length mat-data))))
    (assert
      (every 
        (lambda (col-num) (= col-num (car col-num-lst)))
        col-num-lst))))
(define (make-matrix mat-data)
  (ensure-same-col mat-data)
  (list 'matrix mat-data 
    (vector-length mat-data) 
    (vector-length (vector-ref mat-data 0))))
;; mimic operation?
(define (matrix? object)
  (and (n:list? object)
       (n:= 4 (length object))
       (eq? 'matrix (car object))
       ;; > A reasonable representation of a matrix is a Scheme vector in which each element is a Scheme vector representing a row.
       ;; Here vector? is not redefined, so we can also use mere vector?
       (n:vector? (mat-data object))
       (every n:vector? (vector->list (mat-data object)))
       (number? (mat-row object))
       (number? (mat-col object))))

(register-predicate! matrix? 'matrix)

(define (strict-symbolic? object)
  (and 
    (not (matrix? object))
    (not (general-vector? object))
    (or (symbol? object)
        (pair? object))))
(register-predicate! strict-symbolic? 'strict-symbolic)
(define (strict-symbolic-extender base-arithmetic)
  (make-arithmetic 'strict-symbolic strict-symbolic? (list base-arithmetic)
    (lambda (name base-constant)
      base-constant)
    (let ((base-predicate
           (arithmetic-domain-predicate  base-arithmetic)))
      (lambda (operator base-operation)
        (make-operation operator
                        (any-arg (operator-arity operator)
                                 strict-symbolic?
                                 base-predicate)
                        (lambda args (cons operator args)))))))
;; just mimic matrix-extender

;; > since matrix multiplication is defined only if the number of columns in the first matrix is equal to the number of rows in the second one.
(define (ensure-row-col-compatibility mat1 mat2)
  (assert (n:= (mat-col mat1) (mat-row mat2))))

(define (scalar? data)
  (or (symbolic? data) (number? data)))

;; misc helper
(define (mat-data->2-level-list mat-data)
  (vector->list (vector-map vector->list mat-data)))
(define (2-level-list->mat-data lst)
  (list->vector (map list->vector lst)))

(load "vector-arith-row-col.scm")
; (define (mat->vec mat)
;   (cond 
;     ((n:= 1 (mat-row object)) (vector-ref (mat-data object) 0))
;     (predicate2 consequent2)))

(define (vec->mat vec)
  (let ((vec-data (vector-data vec)))
    (cond 
      ((col-vector? vec) (make-matrix (vector-map vector vec-data)))
      ((row-vector? vec) (make-matrix (vector vec-data))))))

(define (scalar->mat scalar)
  (make-matrix
    (list->vector
      (map 
        (lambda (idx) 
          (apply 
            vector 
            (append (make-list idx 0) (list scalar) (make-list (- scalar 1 idx) 0)))) 
        (iota scalar)))))

;; mimic SICP
(load "sicp-matrix-lib.scm")
(define (transpose-mat-data mat)
  (2-level-list->mat-data (transpose (mat-data->2-level-list mat))))

;; mimic combining-arithmetics/vector-arith.scm
(define (left-row-vector-product-maker + *)
  (define (vector-product v-data mat-data)
    (let ((transpose-mat-inst (transpose-mat-data mat-data))
          (dot-product (dot-product-maker + *)))
      ;; similar to SICP exercise 2.37 matrix-*-vector
      ;; Here we use `dot-product` from vector-arith.scm.
      (vector-map (lambda (col) (dot-product col v-data))
                  transpose-mat-inst)))
  vector-product)

(define (matrix-product-maker + *)
  (let ((left-row-vector-product (left-row-vector-product-maker + *)))
    (define (matrix-product mat1 mat2)
      (ensure-row-col-compatibility mat1 mat2)
      ;; similar to SICP exercise 2.37 matrix-*-matrix
      ;; but here `transpose` is done in left-row-vector-product-maker.
      (let ((mat1-data (mat-data mat1))
            (mat2-data (mat-data mat2)))
        ;; Here we can't call left-product-maker since that will cause infinite dependency loop.
        ;; Since vector-extender doesn't define vec*mat, here we just pass data without tag.
        (vector-map (lambda (row) (left-row-vector-product row mat2-data)) mat1-data)))
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

;; + etc.
(define (ensure-same-dimension mat1 mat2)
  (assert 
    (and
      (n:= (mat-col mat1) (mat-col mat2))
      (n:= (mat-row mat1) (mat-row mat2)))))

(define (matrix-element-wise op)
  (lambda (mat1 mat2) 
    (ensure-same-dimension mat1 mat2)
    (let ((mat1-data (mat-data mat1))
          (mat2-data (mat-data mat2)))
      (vector-map 
        (lambda (row1 row2) (op (make-col-vec row1) (make-col-vec row2))) 
        mat1-data mat2-data))))

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
                ;; Here we don't use make-matrix-operation similar to make-vec-operation since we need to check row, col, etc.
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
                                    (extend-arithmetic strict-symbolic-extender numeric-arithmetic))))
    (install-arithmetic! g)))
(install-specific-generic-arithmetic)

(define test-numeric-mat1 (make-matrix (vector (vector 1 2) (vector 3 4))))
(define test-numeric-mat2 (make-matrix (vector (vector 5 6) (vector 7 8))))
#|
ipython:
# https://numpy.org/doc/stable/reference/generated/numpy.matmul.html
import numpy as np
a = np.array([[1, 2],
              [3, 4]])
b = np.array([[5, 6],
              [7, 8]])
np.matmul(a, b)
|#
(assert (equal? (* test-numeric-mat1 test-numeric-mat2) #(#(19 22) #(43 50))))
