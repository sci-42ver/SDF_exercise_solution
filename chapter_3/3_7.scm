;; By searching "literal-mat", no sample implementation.

;; > It is also possible to have arithmetic on literal matrices and literal vectors
;; > an algebra of symbolic expressions of vectors and matrices.
;; I assume they are *totally* symbolic as "literal function".
;; https://stackoverflow.com/q/4577007/21294350

;; > Can you make symbolic algebra of these *compound structures*
;; https://mresources.github.io/tutorial/using-mathematica/useful-features/symbolic-algebra.html
;; I only read the 1st section, again "*totally* symbolic" assumption.

;; One way to think about "literal matrices and literal vectors" is that they have no defined dimension.
;; If so, we can just use something like literal-mat tag and let symbolic-extender manipulate with it.
;; This is trivial.

;; TODO ";... aborted" this file can't be totally loaded if not using 3_6_contents.scm.
;; probably due to creating one new env...
; (load "3_6.scm")

(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'generic-procedures)
(load "3_6_contents.scm")

(define (literal-mat sym)
  (list 'literal-matrix sym))
(define (literal-vec sym)
  (list 'literal-vec sym))
;; The above "One way ..." is wrong since "symbolic-1?" is based on "numeric-arithmetic", so it can't catch "matrix?".
;; If we let `symbolic-extender-pred` be based on generic, as the book says, this may be too general.
; (* (literal-mat 'A) test-symbolic-mat1)

(define (literal-matrix? object)
  (and (n:list? object)
       (n:= 2 (length object))
       (eq? 'literal-matrix (car object))
       (symbol? (cadr object))))
(define (literal-vec? object)
  (and (n:list? object)
       (n:= 2 (length object))
       (eq? 'literal-vec (car object))
       (symbol? (cadr object))))
(define (literal-matrix-vec? object)
  (or (literal-matrix? object) (literal-vec? object)))
(register-predicate! literal-matrix-vec? 'literal-matrix-vec)

(define (scalar? data)
  (or (symbolic-2? data) (number? data)))
(define (install-specific-generic-arithmetic)
  (let ((g
          (make-generic-arithmetic make-simple-dispatch-store)))
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g
                                (extend-arithmetic 
                                  (lambda (base-arithmetic) 
                                    (symbolic-extender-pred 
                                      ;; added at last to make literal-matrix-vec be compatible with all other types.
                                      literal-matrix-vec?
                                      base-arithmetic))
                                  ;; here matrix-extender has already domain-predicate including vector.
                                  (extend-arithmetic matrix-extender 
                                                     (extend-arithmetic vector-extender 
                                                                        ;; to avoid capturing literal-mat and literal-vec in scalar-matrix-product.
                                                                        (extend-arithmetic 
                                                                          (lambda (base-arithmetic) (-pred symbolic-2? base-arithmetic))
                                                                          numeric-arithmetic)))))
    (install-arithmetic! g)))
(install-specific-generic-arithmetic)

(define test-literal-matrix (literal-mat 'A))
(define test-literal-vec (literal-vec 'A))
(* test-literal-matrix test-symbolic-mat1)
(* test-literal-vec test-symbolic-mat1)
(* test-literal-vec test-symbolic-vec1)
(* test-literal-matrix test-symbolic-vec1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Another way is that we know dimension, then we can do as sym('A', [2 4]) https://www.mathworks.com/help/symbolic/create-symbolic-matrices.html does.
;; Then all works fine since we do "strict-symbolic-extender" (IMHO this means original symbolic-extender in the review when I almost finish chapter 3.) first before vector-extender etc.

;; literal-vec is similar. I won't mimic to create that.
(define (literal-mat sym rows cols)
  (make-matrix 
    (vector-map 
      (lambda (row) 
        (vector-map 
          ;; https://stackoverflow.com/a/8608607/21294350
          (lambda (col) 
            (string->symbol
              (string-append 
                (symbol->string sym)
                (number->string row) 
                (number->string col))))
          (list->vector (iota cols))))
      (list->vector (iota rows)))))

;; assume all symbols -> literal-matrix
(define (literal-matrix? object)
  (and (matrix? object)
       (every 
         (lambda (row) 
           (every symbol? row))
         (mat-data->2-level-list object))))

;; See section 3.3. We can use `generate-uninterned-symbol` to avoid conflicts.
(define test-literal-matrix (literal-mat 'A 2 2))
#|
from sympy import *
var('a b c')
B = Matrix([[a, 2],
            [3, b]])
C = Matrix([[1],
            [c]])
B.multiply(C)

# https://docs.sympy.org/latest/modules/matrices/expressions.html
A = MatrixSymbol('A', 2, 2)
MatA=Matrix(A)
print(MatA.multiply(B))
MatA.multiply(C)
|#
(load "../common-lib/test-lib.scm")
(assert-predicate equal? (* test-literal-matrix test-symbolic-mat1) #(#((+ (* a a00) (* 3 a01)) (+ (* 2 a00) (* b a01))) #((+ (* a a10) (* 3 a11)) (+ (* 2 a10) (* b a11)))))
(assert-predicate equal? (* test-literal-matrix test-symbolic-vec1) #(#((+ (* 1 a00) (* c a01))) #((+ (* 1 a10) (* c a11)))))

;; > Caution: This is quite hard.
;; Maybe my above understanding doesn't get what the author wants to say.
