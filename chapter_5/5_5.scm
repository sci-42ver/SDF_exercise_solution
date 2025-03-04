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

;;; implementation
(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(eq? (the-environment) user-initial-environment)
(load "../software/sdf/manager/load.scm")
(eq? (the-environment) user-initial-environment)
;; many procedures inside (define-command '(new-environment . flavors) ...) are not doc'ed. So skipped.
(manage 'new 'generic-interpreter)
(eq? (the-environment) user-initial-environment)
;; will overload the original one. See (set-cdr! p handler) in SDF_exercises/software/sdf/common/generic-procedures.scm
(define-generic-procedure-handler g:apply
  (match-args strict-primitive-procedure?
              operands?
              environment?)
  (lambda (procedure operands calling-environment)
    (apply-primitive-procedure procedure
      (eval-operands-and-keep-underlying-procedure-arg operands calling-environment))))

(define (new-bindings names vals) (cons names vals))
(define set-names set-car!)
(define set-vals set-cdr!)
(define get-names car)
(define get-vals cdr)

(define (all-bindings environment)
	;; similar to lookup-variable-value
	(let lp ((env environment) (bindings (new-bindings '() '())))
		(if (eq? env the-empty-environment)
			bindings
			(begin
				;; 0. cons is inappropriate to combine 2 lists into one.
				;; 1. binding precedence: see the above.
				;; 2. bindings is a local var, so a bit inappropriate to set!.
				;; Although it is fine here because this local var is *passed along*.
				; (set-car! bindings (append (car bindings) (environment-variables env)))
				; (set-cdr! bindings (append (cdr bindings) (environment-variables env)))
				; (lp  bindings)
				(lp 
					(environment-parent env) 
					(new-bindings
						;; The inner frames are prioritized.
						(append (car bindings) (environment-variables env))
						(append (cdr bindings) (environment-values env))
						))
				)
			)
		)
	)
(write-line (list "outside of the interpreter" (eq? base-env user-initial-environment)))
(define (eval* expression environment)
	(let ((bindings (all-bindings environment)))
		(write-line (list "eval* bindings" bindings))
		(eval 
			expression 
			(extend-top-level-environment
				;; this will throw errors: 
				;This form allowed only at top level: (the-environment) #[syntactic-environment internal]
				;; i.e. at least not allowed inside one lambda
				; (the-environment)
				;; base env used by lookup-scheme-value.
				base-env
				(get-names bindings)
				(get-vals bindings))))
	)
(define (eval-operands-and-keep-underlying-procedure-arg operands calling-environment)
	(map (lambda (operand)
         (cond 
				 	((lambda? operand) (eval* operand calling-environment))
					(else (g:advance (g:eval operand calling-environment)))))
       operands))

;;; > Note: This is subtle to get right, so don't spend infinite time trying to make it work perfectly.
;; TODO IMHO the above is correct.

;;; tests
; (trace extend-top-level-environment) ; no use
; (trace eval*)
(init)
;; normal
(equal? '(1 4 9) (map (lambda (x) (* x x)) '(1 2 3)))

;; using outside var
(define y 3)
(equal? '(3 6 9) (map (lambda (x) (* x y)) '(1 2 3)))

;; overload var definitions
(equal? 
	'(4 8 12)
	;; here map will have 2 bindings for a.
	;; So the a for map is 4.
	((lambda (a) ((lambda (a) (map (lambda (x) (* x a)) '(1 2 3))) (+ a a))) 2)
	)
