;; > procedural arguments
;; https://en.wikipedia.org/wiki/Procedural_parameter

;; > if we redefine map for our interpreter it does work:
;; same as SICP p144 except without nil abstraction.

;; > Why does it not work to use the underlying procedures that take procedural arguments, such as map? Explain.
;; see SDF_exercises/README.md

;;; > Outline a strategy to fix the problem and implement your solution.
;; What to be done is just let the lambda object here can be executed expectedly like strict-compound-procedure.
;; 0. IMHO just let the underlying scheme eval to return one compatible lambda object.
;; 0.a. Notice the env arg needs to contain all necessary bindings.
;; See (match-args strict-compound-procedure? operands? environment?)
;; 0.a.0. length compatibility can be done by the underlying Scheme, e.g. the following will throw errors.
; (map (lambda (x y) (* x x)) '(1 2 3))
;; 0.a.1. IGNORE: Then we use the underlying eval instead of g:eval.
;; We need to pass the correct env to eval just like make-compound-procedure does.
;; 0.a.1.a. IMHO the env trivially should contain those can't be accessed by lookup-scheme-value.
;; So it is based on (the-environment).

(map (lambda (x) (* x x)) '(1 2 3))
(define y 3)
(map (lambda (x) (* x y)) '(1 2 3))
;; 0.a.1.b. So it should also contain those defined in this evaluator, i.e. the-global-environment.
;; Done by behavior1 in SDF_exercises/chapter_5/5_5_env_behaviors.scm
;; 0.a.1.c. lookup-variable-value behavior same as the evaluator:
;; newer in the-global-environment => older ... => (the-environment)
;; 0.a.1.d. ";; behavior3" implies the evaluator inherence here, i.e. changes are independent of the underlying Scheme.

;; IGNORE won't work
;; tests for the 2nd => based on "shadow" in SDF_exercises/chapter_5/5_5_env_behaviors.scm
(define test-overload-env (extend-top-level-environment (the-environment) '(*) `(,+)))
;; notice to use quote to avoid applicative-order evaluation.
(eval '(assert (= (+ 3 3) ((lambda (x) (* x x)) 3))) test-overload-env)

;; tests for the 1st =>
(define test-env-with-overloads-inside (extend-top-level-environment (the-environment) '(a a) `(2 3)))
;; notice here the 1st new binding is used.
(eval '(assert (= (* 3 2) ((lambda (x) (* x a)) 3))) test-env-with-overloads-inside)
