;; extend-top-level-environment normal behavior (behavior1)
;; IGNORE won't work
(define test-normal-env (extend-top-level-environment (the-environment) '(a) '(2)))
(eval '(assert (= (* 3 2) ((lambda (x) (* x a)) 3))) test-normal-env)

;; https://stackoverflow.com/a/7111493/21294350
; (define (extend-top-level-environment* base names values)
; 	(let ((bound-var-names (environment-bound-names base))
; 				(new-env (make-top-level-environment)))
; 		(for-each
; 			(lambda (name) (link-variables new-env name base name))
; 			bound-var-names
; 			)
; 		(for-each
; 			(lambda (sym val) (environment-define new-env sym val))
; 			names
; 			values
; 			)
; 		new-env
; 		))

;; behavior2
(define a 3)
(define test-normal-env (extend-top-level-environment (the-environment) '(a) '(2)))
(environment-lookup test-normal-env 'a)
;; overloaded to 2 as https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Environment-Concepts.html.
;; > The new bindings shadow the old ones
;; just as SICP implementation and implementation here do.

;; behavior3
(environment-lookup (the-environment) 'a)
;; > Note that “extending an environment” does not modify the environment
;; so still 3.
