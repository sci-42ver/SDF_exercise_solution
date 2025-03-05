; (define model (make-working-env-model))
; (define base-env (model 'get-environment))
(define base-env (make-top-level-environment))
(define base-lambda (lambda (x) (+ x 10)))
(define extended-env (extend-top-level-environment base-env '(a b f) `(1 2 ,base-lambda)))

(define lambda1 (eval '(lambda (a) (* a b (f a))) extended-env))

(set! base-lambda (lambda (x) (+ x 20)))

;; Here it uses base-lambda value but not its reference.
(lambda1 1) 
(* 1 2 (+ 1 20))

;;
; (define f (lambda (x) (+ x 10)))

;; all can access procedure-environment
(procedure-environment base-lambda)
(procedure-environment lambda1)
