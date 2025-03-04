;; > In this exercise we change the interpreter to accept a single symbol as the formal parameter list
;; > Change g:apply to work with the new compound procedures.

;; > This can be done by rewriting the existing strict-compound-procedure? handler of g:apply (page 246)
;; i.e. the handler for strict-compound-procedure?.
;; IMHO just changing extend-environment is enough because here we just care about bindings for procedure-parameters.
(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'generic-interpreter 'combining-arithmetics)
(define (strict-compound-procedure? object)
  (and (compound-procedure? object)
       (let ((params (procedure-parameters object)))
         (or (and (list? params) (every symbol? params))
             (variable? params)
             ))
       ))
;; overload handler as add-handler! and get-handler implies
(define-generic-procedure-handler g:apply
                                  (match-args strict-compound-procedure? operands? environment?)
                                  (lambda (procedure operands calling-environment)
                                    (let ((params (procedure-parameters procedure)))
                                      (cond 
                                        ((list? params)
                                         (if (not (n:= (length params)
                                                       (length operands)))
                                           (error "Wrong number of operands supplied"))
                                         )
                                        (else 'pass-check)
                                        )
                                      ;; > We also can extract the body of the procedure, which
                                      (g:eval (procedure-body procedure)
                                              ;; > we will pass to eval with an environment that includes the formal parameter bindings.
                                              (extend-environment
                                                params
                                                (eval-operands operands calling-environment)
                                                ;; > built on the environment packaged with the procedure
                                                (procedure-environment procedure))))
                                    ))
;; for modularity, here I checked for variables type both in extend-environment and the above  handler. Maybe there is one better alternative ~~TODO~~ (See "to add a new handler").
(define (extend-environment variables values base-environment)
  (cond 
    ((list? variables)
     (if (not (fix:= (length variables) (length values)))
       (if (fix:< (length variables) (length values))
         (error "Too many arguments supplied" variables values)
         (error "Too few arguments supplied" variables values)))
     (vector variables values base-environment)
     )
    ((variable? variables) 
     (vector (list variables) (list values) base-environment)
     )
    (else (error "Incompatible arguments supplied" variables values))
    )
  )

(init)
;; (lookup-scheme-value 'assert) => ;Variable reference to a syntactic keyword: assert
(n:= 6 ((lambda args (apply + args)) 1 2 3))
(n:= 3 ((lambda (arg1 arg2) (+ arg1 arg2)) 1 2))

;; > but it is both easier and clearer to specialize that handler for the case where the procedure- parameters is a list, and to add a new handler for the case where the procedure-parameters is a symbol.
;; I will try both for comparison. Anyway the underlying ideas are same.
(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'generic-interpreter 'combining-arithmetics)
(define (compound-procedure-with-symbol-arg? object)
  (and (compound-procedure? object)
       ;; Compared with the above, this is checked only once.
       (variable? (procedure-parameters object))
       ))
(define-generic-procedure-handler g:apply
                                  (match-args compound-procedure-with-symbol-arg? operands? environment?)
                                  (lambda (procedure operands calling-environment)
                                    (let ((params (procedure-parameters procedure)))
                                      ;; > We also can extract the body of the procedure, which
                                      (g:eval (procedure-body procedure)
                                              ;; > we will pass to eval with an environment that includes the formal parameter bindings.
                                              (extend-environment
                                                (list params)
                                                (list (eval-operands operands calling-environment))
                                                ;; > built on the environment packaged with the procedure
                                                (procedure-environment procedure))))
                                    ))

;; same tests as the above
(init)
(n:= 6 ((lambda args (apply + args)) 1 2 3))
(n:= 3 ((lambda (arg1 arg2) (+ arg1 arg2)) 1 2))

