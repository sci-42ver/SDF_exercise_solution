;; by searching "vector.*cos sin\) 3\)" in this repo, only 6.945_assignment_solution has this implementation.
;; See 6.945_assignment_solution 3.1
(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'combining-arithmetics)
(load "~/SICP_SDF/SDF_exercises/software/sdf/combining-arithmetics/vector-arith.scm")

; (pp (pe))

;; a
(define (function-extender codomain-arithmetic)
  (letrec ((codomain-predicate
             (arithmetic-domain-predicate codomain-arithmetic)))
    (make-arithmetic 'function
                     (disjoin codomain-predicate function?)
                     (list codomain-arithmetic)
                     (lambda (name codomain-constant)
                       codomain-constant)
                     (lambda (operator codomain-operation)
                       (make-operation operator
                                       (any-arg (operator-arity operator)
                                                function?
                                                codomain-predicate)
                                       ;; https://stackoverflow.com/a/7719140/21294350
                                       ;; > giving x a reference to itself so now x can refer to itself
                                       ;; here fact-gen is given one func arg, we can think of `(fact-gen fact-gen)` is `((lambda (fact-gen) ...) fact-gen)` which returns the original func `(lambda (n) ...)`.
                                       ;; Also see CS61A related notes "Y combinator" which shares one similar idea by passing *func* in args.
                                       ;; This corresponds to the book
                                       ;; > self-referential structures
                                       ;; > self reference is cumbersome to arrange
                                       ((lambda (x) (x x))
                                        (lambda (func)
                                          (lambda things
                                            (lambda args
                                              ;; > (+ 1 (cos a))
                                              ;; works since "codomain-operation" allows '(cos a) as the symbolic.
                                              ; (apply-operation codomain-operation
                                              ;; 1. TODO here we need to get the final procedure, but we have the error ";This form allowed only at top level: (the-environment) #[syntactic-environment internal]".
                                              ;; This seems impossible https://stackoverflow.com/q/6584818/21294350
                                              ; (apply (environment-lookup (the-environment) operator)
                                              ;; 2. If define this nested lambda as proc and then call proc
                                              (let ((transformed-args 
                                                      (map (lambda (thing)
                                                             (if (function? thing)
                                                               ;; small changes
                                                               (apply thing args)
                                                               thing))
                                                           things)))
                                                (if (any function? transformed-args)
                                                  ;; notice in the original version, actually it calls `pairwise` to get one `(lambda args ...)` for the former 2 args
                                                  ;; then it calls `(pairwise binary ((lambda args ...) (lambda (x) ...)))` which indeed does same as `(lambda things ...)` for 3 args.
                                                  ;; then it consumes 'a arg to have (* 3 (lambda (y) (+ 'a y)) (lambda (y) (vector y 'a))).
                                                  ;; since they are all done by binary which consumes `(lambda things (lambda args ...))` only, this also works for here.
                                                  ;; The only difference is how we interpret * in (* 3 (lambda (y) (+ 'a y)) (lambda (y) (vector y 'a))). 

                                                  ;; call (* 3 (lambda (y) (+ 'a y))) where * is `(lambda things ...)`
                                                  ;; so this returns `(lambda args ...)` which then uses the `(lambda (y) ...)`,
                                                  ;; so then we call `codomain-operation`.

                                                  ;; based on the 1st paragraph, recursive call `(func func)` here will only consume arg `4` in the following example.
                                                  (apply (func func) transformed-args)
                                                  (apply-operation codomain-operation transformed-args))))))))))))
(define func-vec-symbolic-numeric
  (extend-arithmetic function-extender (extend-arithmetic vector-extender combined-arithmetic)))
(install-arithmetic! func-vec-symbolic-numeric)

(define (test-1)
  (((* 3
       (lambda (x) (lambda (y) (+ x y)))
       (lambda (x) (lambda (y) (vector y x))))
    'a)
   4))

(test-1)
;; since we call vector after symbolic which allows scalar-product with symbolic.
;; #((* (* 3 (+ a 4)) 4) (* (* 3 (+ a 4)) a))

(define func-symbolic-vec-numeric
  (extend-arithmetic function-extender (extend-arithmetic symbolic-extender (extend-arithmetic vector-extender numeric-arithmetic))))
(install-arithmetic! func-symbolic-vec-numeric)

(test-1)
;Value: (* (* 3 (+ a 4)) #(4 a))

;; b
;; I won't re-dig into the calling order. Here I just ensures the 2 problems in 3.3 are avoided.
;; 1. + here obviously supports func due to generic calling any-object?.
;; 2. magnitude called in function-extender is again generic. We only need to ensure it has vector implementation.

(pp (pe))
(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(pp (pe))
(manage 'new 'generic-procedures)
(pp (pe))
(manage 'help)
(load "~/SICP_SDF/SDF_exercises/software/sdf/combining-arithmetics/vector-arith.scm")

(define (test-1)
  (((* 3
       (lambda (x) (lambda (y) (+ x y)))
       (lambda (x) (lambda (y) (vector y x))))
    'a)
   4))
(define (install-specific-generic-arithmetic)
  (let ((g
          (make-generic-arithmetic make-simple-dispatch-store)))
    ;; it has numeric-arithmetic, (function-extender g), (extend-arithmetic ...).
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    ;; notice here (symbolic func) will be manipulated with `function-extender`.
    ;; IGNORE: Here vector-symbolic is to make (* sin sin) etc. in `(magnitude (vector sin cos))` work.
    (add-to-generic-arithmetic! g
                                (extend-arithmetic symbolic-extender (extend-arithmetic vector-extender numeric-arithmetic)))
    ;; the following sin etc. are generic proc after install-specific-generic-arithmetic.
    (extend-generic-arithmetic! g vector-extender)
    (install-arithmetic! g)))
(install-specific-generic-arithmetic)
(test-1)

(define (test-3-3)
  (define (unit-circle x)
    (vector (sin x) (cos x)))
  ((magnitude unit-circle) 'a)
  ((magnitude (vector sin cos)) 'a))
(test-3-3)

;; c
;; here we just make vector function similar to + in function-extender.

;; since `arithmetic-operators-for` is based on bases which at last is `operator-names`,
;; and `make-arithmetic` doesn't have one option to add operator (also for `add-arithmetics` which at last calls `make-arithmetic` always)
;; we need to change base case `operator-names` to have `vector`.

;; See 6.945_assignment_solution. We are better to define n:vector.
;; not use `(define (n:vector . args) (apply vector args))` since we will redefine vector.
(define n:vector vector)

(set! %arithmetic-operator-alist (cons '(vector (domain domain) domain) %arithmetic-operator-alist))
;; reload `numeric-arithmetic` to ensure (vector num num) is supported.
(load "numeric-arith-mod.scm")
(install-specific-generic-arithmetic)
;; use numeric-arithmetic to get (vector (cos 3) (sin 3)).
;; then again numeric-arithmetic to get the final result.
((vector cos sin) 3)
((vector cos 2) 3)

;; 6.945_assignment_solution
;; all tests in 3.1 (c) are included here.
(vector 1 2)

;; different from 6.945_assignment_solution since here symbolic-extender will just cons operator.
((vector sin cos) 'a)
; (vector (sin a) (cos a))

(define cos-mult
  (lambda (a)
    (lambda (x)
      (cos (* a x)))))

(define sin-mult
  (lambda (a)
    (lambda (x)
      (sin (* a x)))))

;; different from 6.945_assignment_solution due the the last reason.
(((vector cos-mult sin-mult) 2) 'a)
;; manipulated by function-extender
; (vector (cos (* 2 a)) (sin (* 2 a)))

;; just use n:vector due to using numeric-arithmetic
(* (vector 2 3) (vector 1 2))

;; similar to cos-mult part if using vector since `(vector (cos a) (sin a))` is one symbol.
((* (vector cos sin) (vector cos sin)) 'a)
; (* (vector (cos a) (sin a)) (vector (cos a) (sin a)))
;; if using n:vector, then calls vector-extender first instead of function-extender.
;; then it calls (* cos cos) etc. which is one lambda func by function-extender.
;; so it then calls ((+ (* cos cos) (* sin sin)) 'a) where 'a is passed by `(apply thing args)` (e.g. (* cos cos) -> cos) to cos and sin.
((* (n:vector cos sin) (n:vector cos sin)) 'a)
; (+ (* (cos a) (cos a)) (* (sin a) (sin a)))

;; just do the same as 3.3. 
((magnitude (n:vector cos sin)) 'a)

;; avoid
;; > This only supports fixed-length vectors
;; This is not the case since the book generic + supports multiple args.
;; > because we had to define "vector-new" as a generic procedure taking a certain number of arguments
(load "general-vector-lib.scm")

(install-specific-generic-arithmetic-2)

((vector cos sin sqrt) 3)
;; `(+-like operator identity-name)` makes multiple arguments able to manipulate.
((+ cos sin sqrt) 3)
