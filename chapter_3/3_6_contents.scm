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

(load "general-symbolic-extender.scm")
;; just mimic matrix-extender

;; > since matrix multiplication is defined only if the number of columns in the first matrix is equal to the number of rows in the second one.
(define (ensure-row-col-compatibility mat1 mat2)
  (assert (n:= (mat-col mat1) (mat-row mat2))))

(define (scalar? data)
  (or (symbolic-1? data) (number? data)))

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

(define (scalar->mat scalar dim)
  (make-matrix
    (list->vector
      (map 
        (lambda (idx) 
          (apply 
            vector 
            (append (make-list idx 0) (list scalar) (make-list (- dim 1 idx) 0)))) 
        (iota dim)))))

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
      (matrix-product mat (transformer v)))
    product))

(define (right-scalar-matrix-product-maker + *)
  (let ((matrix-product (matrix-product-maker + *)))
    (define (product mat scalar)
      (matrix-product mat (scalar->mat scalar (mat-col mat))))
    product))

(define (left-scalar-matrix-product-maker + *)
  (let ((matrix-product (matrix-product-maker + *)))
    (define (product scalar mat)
      (matrix-product (scalar->mat scalar (mat-row mat)) mat))
    product))

;; + etc.
;; mimic ensure-vector-lengths-match and vector-element-wise.
(define (ensure-same-dimension mats)
  (let ((first-mat-col (mat-col (car mats)))
        (first-mat-row (mat-row (car mats))))
    (if (any (lambda (mat)
               (or
                 (not (n:= (mat-col mat)
                           first-mat-col))
                 (not (n:= (mat-row mat)
                           first-mat-row))))
             mats)
      (error "Vector dimension mismatch:" mats))))

(define (matrix-element-wise row-op)
  (lambda mats    ; Note: this takes multiple vectors
    (ensure-same-dimension mats)
    (apply 
      vector-map 
      (lambda rows (apply row-op (map make-row-vec rows))) 
      (map mat-data mats))))

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
            (left-scalar-product (left-scalar-matrix-product-maker + *))
            (right-scalar-product (right-scalar-matrix-product-maker + *))
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
                                                                      (extend-arithmetic 
                                                                        (lambda (base-arithmetic) (symbolic-extender-pred symbolic-1? base-arithmetic))
                                                                        numeric-arithmetic))))
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
print(np.matmul(a, b))

v1= np.array([[1], [2]])
v2= np.array([[1, 2]])
print(np.matmul(a, v1))
print(np.matmul(v2, a))
|#
(load "test-lib.scm")
(assert-predicate equal? (* test-numeric-mat1 test-numeric-mat2) #(#(19 22) #(43 50)))

(define test-numeric-mat3 (make-matrix (vector (vector 5 6 7) (vector 7 8 9))))
;; > since matrix multiplication is defined only ...
; (* test-numeric-mat3 test-numeric-mat1) ; throw errors.

;; > multiply a matrix with a scalar or with a vector.
(define test-vec-data (vector 1 2))
(define test-numeric-vec1 (make-col-vec test-vec-data))
(define test-numeric-vec2 (make-row-vec test-vec-data))
(assert-predicate equal? (* test-numeric-mat1 test-numeric-vec1) #(#(5) #(11)))
(assert-predicate equal? (* test-numeric-vec2 test-numeric-mat1) #(#(7 10)))

(assert-predicate equal? (* test-numeric-mat1 -1) #(#(-1 -2) #(-3 -4)))
(assert-predicate equal? (* -1 test-numeric-mat1) #(#(-1 -2) #(-3 -4)))

;; - should be similar.
(assert-predicate equal? (+ test-numeric-mat1 test-numeric-mat2) #(#(6 8) #(10 12)))
; (negate test-numeric-mat1 test-numeric-mat2) ; As expected, Inapplicable generic procedure ...
(assert-predicate equal? (negate test-numeric-mat1) #(#(-1 -2) #(-3 -4)))

;; b
#|
https://stackoverflow.com/a/46852013

from sympy import *
var('a b c')
A = Matrix([[1, 2],
            [3, 4]])
B = Matrix([[a, 2],
            [3, b]])
A.multiply(B)
C = Matrix([[1],
            [c]])
B.multiply(C)
|#
(define test-symbolic-mat1 (make-matrix (vector (vector 'a 2) (vector 3 'b))))
(define test-symbolic-vec1 (make-col-vec (vector 1 'c)))
(assert-predicate equal? (* test-numeric-mat1 test-symbolic-mat1) #(#((+ (* a 1) 6) (+ 2 (* b 2))) #((+ (* a 3) 12) (+ 6 (* b 4)))))
(assert-predicate equal? (negate test-symbolic-mat1) '#(#((negate a) -2) #(-3 (negate b))))
(assert-predicate equal? (* test-symbolic-mat1 test-symbolic-vec1) '#(#((+ (* 1 a) (* c 2))) #((+ 3 (* c b)))))

;; c I don't know which method will cause this scary space usage. https://en.wikipedia.org/wiki/Invertible_matrix#Methods_of_matrix_inversion

(display "loaded")
(manage 'name-current-environment "3_6")
(manage 'environment-names)
