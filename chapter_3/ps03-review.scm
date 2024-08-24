(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'generic-procedures)
(load "~/SICP_SDF/SDF_exercises/software/sdf/combining-arithmetics/vector-arith.scm")

; (define (n:vector . args) (apply vector args))
;; avoid hang
(define n:vector vector)

(register-predicate! list? 'list)

(define vector (simple-generic-procedure 'vector 2 vector)) ; changed to be compatible with the new code base.
(define-generic-procedure-handler vector
                                  (all-args 2 (disjoin number? symbol? list?))
                                  ;; This is why hang.
                                  (lambda (a b) (n:vector a b)))

(define-generic-procedure-handler vector
                                  (all-args 2 function?)
                                  (lambda (a b) (lambda (x) (vector (a x) (b x)))))

;; from 3_4.scm to have vector-extender.
(define (install-specific-generic-arithmetic)
  (let ((g
        (make-generic-arithmetic make-simple-dispatch-store)))
    ;; it has numeric-arithmetic, (function-extender g), (extend-arithmetic ...).
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g
                                (extend-arithmetic symbolic-extender (extend-arithmetic vector-extender numeric-arithmetic)))
    (extend-generic-arithmetic! g vector-extender)
    (install-arithmetic! g)))
(install-specific-generic-arithmetic)

(* (vector 2 3) (vector 1 2))