;; no sample implementation by searching "gradient".
;; covector definition https://math.stackexchange.com/questions/4872003/am-i-correct-in-terms-of-understanding-gradient-and-covector#comment10387345_4872003
;; its definition depends one vector definition https://en.wikipedia.org/wiki/Gradient#Cartesian_coordinates or too long https://math.stackexchange.com/a/240629/1059606

(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'automatic-differentiation)

;; covector -> row vector
(load "vector-arith-row-col.scm")
(load "test-lib.scm")

;; a
;; similar to general-derivative.
;; By abstraction, here I won't check the detailed implementation of `derivative` and ``partial.
(define (gradient g)
  (define (the-derivative-covector . args)
    (let* ((n (length args))
           (vec-data 
              (if (= n 1)
                (vector ((derivative g) (car args)))
                (list->vector
                  (map (lambda (i)
                          (apply ((partial i) g) args))
                        (iota n))))))
      (make-row-vec vec-data)))
  the-derivative-covector)

(define (test-1)
  ;; > the covector of partials.
  (assert-predicate equal? ((gradient (lambda (x y) (+ x y))) 1 1) '(row #(1 1)))
  (assert-predicate equal? ((gradient (lambda (x) (* 2 x))) 1) '(row #(2))))
(test-1)

;; 1. > the product of a vector and a covector should be their inner product
;; same as dot product value.
;; 2. notice to use "extend-arithmetic" to keep some generic base arithmetic which is prioritized.
(install-arithmetic! (extend-arithmetic vector-extender full-arithmetic))
(assert (= 2 (* (make-col-vec (vector 1 1)) (make-row-vec (vector 1 1)))))

;; b
;; See https://math.stackexchange.com/questions/4872003/am-i-correct-in-terms-of-understanding-gradient-and-covector#comment10387437_4872003 for why we define such a way.
;; 
(define (gradient g)
  (define (derivative-vec-data . args)
    (let* ((n (length args)))
      (if (= n 1)
        (vector ((derivative g) (car args)))
        (list->vector
          (map (lambda (i)
                  (apply ((partial i) g) args))
                (iota n))))))
  (define (the-derivative-covector . args)
    (let* ((n (length args)))
      (if (= n 1)
        (let* ((arg (car args))
               (vec-data 
                  (if (general-vector? arg)
                    (apply derivative-vec-data (vector->list (vector-data arg)))
                    (derivative-vec-data arg))))
          (cond 
            ((row-vector? arg) (make-col-vec vec-data))
            (else (make-row-vec vec-data))))
        (make-row-vec
          (list->vector
            (map (lambda (i)
                    (apply ((partial i) g) args))
                  (iota n)))))))
  the-derivative-covector)

(test-1)
(assert-predicate equal? ((gradient (lambda (x y) (+ x y))) (make-row-vec (vector 1 1))) (make-col-vec (vector 1 1)))
(assert-predicate equal? ((gradient (lambda (x y) (+ x y))) (make-col-vec (vector 1 1))) (make-row-vec (vector 1 1)))