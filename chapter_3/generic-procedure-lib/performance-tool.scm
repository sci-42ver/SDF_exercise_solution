;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; preface
(register-predicate! differential? 'differential)
(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))
;; See `predicates-match?` and `apply-predicate`.
;; So the above < is not counted.
; (with-predicate-counts (lambda () (fib 20)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; exercise preface
(load "~/SICP_SDF/SDF_exercises/software/sdf/common/stormer2.scm")
(define (test-stormer-counts)
  (define (F t x) (- x))
  (define numeric-s0
    (make-initial-history 0 .01 (sin 0) (sin -.01) (sin -.02)))
  (with-predicate-counts
    (lambda ()
      (x 0 ((evolver F 'h stormer-2) numeric-s0 1)))))

(define full-generic-arithmetic
  (let ((g (make-generic-arithmetic make-simple-dispatch-store)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g
                                (symbolic-extender numeric-arithmetic))
    g))

(define trie-full-generic-arithmetic
  (let ((g (make-generic-arithmetic make-trie-dispatch-store)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g
                                (symbolic-extender numeric-arithmetic))
    g))

(define (<-fib-test)
  (with-predicate-counts (lambda () (fib 1))))

(define (<+-_fib-test)
  (with-predicate-counts (lambda () (fib 2))))

(define (book-fib-test)
  (with-predicate-counts (lambda () (fib 20))))