(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'combining-arithmetics)
(load "~/SICP_SDF/SDF_exercises/software/sdf/combining-arithmetics/vector-arith.scm")

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

(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'generic-procedures)
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
    (add-to-generic-arithmetic! g
                                (extend-arithmetic symbolic-extender (extend-arithmetic vector-extender numeric-arithmetic)))
    (install-arithmetic! g)))
(install-specific-generic-arithmetic)
(test-1)

;; c
;; here we just make vector function similar to + in function-extender.

;; since `arithmetic-operators-for` is based on bases which at last is `operator-names`,
;; and `make-arithmetic` doesn't have one option to add operator (also for `add-arithmetics` which at last calls `make-arithmetic` always)
;; we need to change base case `operator-names` to have `vector`.

(set! %arithmetic-operator-alist (cons '(vector (domain domain) domain) %arithmetic-operator-alist))
;; reload `numeric-arithmetic` to ensure (vector num num) is supported.
(load "~/SICP_SDF/SDF_exercises/software/sdf/common/numeric-arith.scm")
(install-specific-generic-arithmetic)
;; use numeric-arithmetic to get (vector (cos 3) (sin 3)).
;; then again numeric-arithmetic to get the final result.
((vector cos sin) 3)