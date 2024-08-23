;; By searching `vec-before-func` in this repo, nbardiuk, mbillingr, chebert, 6.945_assignment_solution and code base (only nbardiuk repo is not included in this repo) all don't have this implementation.

;; IGNORE:
;; ((magnitude unit-circle) 'a)
;; ((magnitude (vector sin cos)) 'a)
;; 1. arguments are func? and vector?
;; Here extend-arithmetic prioritizes base.
;; for vec-before-func: so the former will call `(unit-circle 'a)` and the latter will call `(sqrt (dot-product v v))` (IMHO this is fine).
;; for func-before-vec: 

(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'combining-arithmetics)
(load "~/SICP_SDF/SDF_exercises/software/sdf/combining-arithmetics/vector-arith.scm")

;; This has combined-arithmetic, (vector-extender combined-arithmetic), (function-extender (vector-extender combined-arithmetic))
(define vec-before-func
  (extend-arithmetic
    function-extender
    (extend-arithmetic vector-extender combined-arithmetic)))
(define func-before-vec
  (extend-arithmetic
    vector-extender
    (extend-arithmetic function-extender combined-arithmetic)))

(install-arithmetic! vec-before-func)
(define (unit-circle x)
  (vector (sin x) (cos x)))
;; works by avoiding the 2nd error.
((magnitude unit-circle) 'a)
;; errors are thrown due to `(extend-arithmetic vector-extender combined-arithmetic)` doesn't support + for func.
; ((magnitude (vector sin cos)) 'a)

(install-arithmetic! func-before-vec)
;; function-extender will call combined-arithmetic for magnitude which doesn't support vector.
; ((magnitude unit-circle) 'a)
;; works by avoiding the 1st error.
((magnitude (vector sin cos)) 'a)

;; > Is it possible to make an arithmetic for which both evaluate correctly?
;; We can combine the above 2 by choosing appropriate arithmetics from the above union of 5 (6-1) arithmetics.

(define vec-func-combined-arithmetic
  (add-arithmetics
    (function-extender (vector-extender combined-arithmetic)) ; only manipulates function?
    (vector-extender (function-extender combined-arithmetic)) ; only manipulates vector? and scalar product.
    combined-arithmetic ; only manipulates symbolic (here we only use this) and numeric.
    ))
(install-arithmetic! vec-func-combined-arithmetic)
;; All these calls are dispatched by operation-applicability.
;; call `magnitude func` from (function-extender (vector-extender combined-arithmetic))
;; -> (sin x) from combined-arithmetic (notice this is called when `(apply thing args)`, so it needs `combined-arithmetic`. This is different from `apply-operation`.)
;; -> call `magnitude (vector ...)` (i.e. `codomain-operation`) from (vector-extender combined-arithmetic) (notice all these are in env of `(function-extender (vector-extender combined-arithmetic))`).
;; -> call `dot-product` and then `(vector-map * v1 v2)` from combined-arithmetic (notice this is from `(arithmetic-operation operator component-arithmetic)` in `(vector-extender combined-arithmetic)`.)

;; interesting here `(cos x)` is first called since commenting `combined-arithmetic` out will throw error ";Inapplicable operation: cos (a)".
((magnitude unit-circle) 'a)
;; magnitude from (vector-extender (function-extender combined-arithmetic))
;; -> (sqrt (+ (* sin sin) (* cos cos))) where (* sin sin) etc. is lambda func: `(vector-sum (vector-map * v1 v2))` from (function-extender combined-arithmetic)
;; -> then recursively pass 'a by `(lambda args ...)` in function-extender.
((magnitude (vector sin cos)) 'a)

;; > `operation-union` may overload if not carefully used
(define overload-arithmetic (add-arithmetics vec-before-func func-before-vec))
(install-arithmetic! overload-arithmetic)

((magnitude unit-circle) 'a)
;; will call `(vector-extender combined-arithmetic)`
; ((magnitude (vector sin cos)) 'a)

;; > Now we can use complex mixed expressions, because the functions are defined over the generic arithmetic:
(define func-symbolic-numeric
  (extend-arithmetic function-extender combined-arithmetic))
(install-arithmetic! func-symbolic-numeric)
;; here (+ 3 'a) works by combined-arithmetic
;; then (+ 'c cos) works by function-extender
;; then (cos (+ 3 'a)) (also for sin) by combined-arithmetic
;; then (* 'b ...) combined-arithmetic
(* 'b ((+ 'c cos sin) (+ 3 'a)))

(((+ (lambda (x) (lambda (y) (cons x y)))
     (lambda (x) (lambda (y) (cons y x))))
  3)
 4)
; (+ (lambda (y) (cons 3 y)) (lambda (y) (cons y 3)))
