(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "5_5_preparation.scm")
(load "../software/sdf/manager/load.scm")
;; many procedures inside (define-command '(new-environment . flavors) ...) are not doc'ed. So skipped.
(manage 'new 'generic-interpreter)

;;; implementation
;; will overload the original one. See (set-cdr! p handler) in SDF_exercises/software/sdf/common/generic-procedures.scm
(define-generic-procedure-handler 
  g:apply
  (match-args strict-primitive-procedure?
              operands?
              environment?)
  (lambda (procedure operands calling-environment)
    (apply-primitive-procedure 
      procedure
      ;; modified
      ; (eval-operands operands calling-environment)
      ;; 0. not use eval-operands-and-keep-underlying-procedure-arg because we may have one var whose val is lambda procedure.
      ;; 1. Here proc-arg may be still something like map, so we need to dig into operands to  transform all strict-compound-procedure's.
      (let ((strict-compound-procedure-vars (empty-vars-with-binding-to-strict-compound-procedure)))
        (tree-map-with-strict-compound-procedure-as-elem
          strict-compound-procedure->underlying-procedure
          (eval-operands* 
            operands 
            calling-environment
            strict-compound-procedure-vars)))
      )))
(define (empty-vars-with-binding-to-strict-compound-procedure) (list 'vars-with-binding-to-strict-compound-procedure))
(define (empty-vars-with-binding-to-strict-compound-procedure? obj) (tagged-list? obj 'vars-with-binding-to-strict-compound-procedure))
(define (eval-operands* operands calling-environment vars-with-binding-to-strict-compound-procedure)
  (assert (empty-vars-with-binding-to-strict-compound-procedure? vars-with-binding-to-strict-compound-procedure))
  (map (lambda (operand)
         (let ((val (g:advance (g:eval operand calling-environment))))
           (if (strict-compound-procedure? val)
             (set-cdr! vars-with-binding-to-strict-compound-procedure (cons operand (cdr vars-with-binding-to-strict-compound-procedure))))
           val
           )
         )
       operands))
;; IGNORE TODO no use
;; see SDF_exercises/chapter_5/tests/trace_apply.scm
; (trace apply-primitive-procedure)

(define (make-lambda-corrected parameters body)
  (cons 'lambda
        (cons parameters
              (cond 
                ((begin? body) (begin-actions body))
                ;; see SDF_exercises/chapter_5/tests/lambda_tests.scm
                ;; for simplicity, here we skip cond manipulation correction.
                ((null? body) (error "invalid lambda"))
                (else (list body))))))

(cd "~/SICP_SDF/SDF_exercises")
(load "common-lib/tree_lib.scm")
(define (tree-map-with-strict-compound-procedure-as-elem proc tree)
  (define (elem? obj)
    ;; here exp may work due to using parent env bindings.
    ; (or (strict-compound-procedure? exp)
    ;   ;; we won't descend deeper than eval-operands.
    ;   (not (list? exp))
    ;   )
    (or (strict-compound-procedure? obj)
        (not (list? obj))
        )
    )
  (tree-map proc tree elem?)
  )
; (trace tree-map-with-strict-compound-procedure-as-elem)
; (trace tree-map)

;; transform if possible
(define (strict-compound-procedure->underlying-procedure exp)
  (cond 
    ((strict-compound-procedure? exp)
     (eval*
       ;; What to pass here?
       ;; just the original lambda-exp and lexical env.
       ;; 0. see g:eval (match-args lambda? environment?)
       ;; 0.a. How lambda object is ~~passed around~~ eval'ed?
       ;; make-compound-procedure
       ;; 0.b. Here lambda-parameters is just same as the original parameters part in (make-lambda parameters body).
       ;; 0.b.0. lambda-body is different due to with one begin wrapper *possibly*.
       ;; 3 cases: 
       ;; null: (list body) -> (list '()) redundantly
       ;; len=1: (cons params (list exp)) -> `(,params exp) fine.
       ;; len>1: begin will be dropped, so go back to the original one but with internal begin removed.
       ;; 0.b.1. env is just passed around intact.
       (make-lambda-corrected
         (procedure-parameters exp)
         (procedure-body exp)
         )
       (vector-copy (procedure-environment exp)))
     )
    (else exp))
  )

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
; (write-line (list "outside of the interpreter" (eq? base-env user-initial-environment)))
; #f
(define (eval* expression environment)
  (let ((bindings (all-bindings environment)))
    (write-line (list "eval*" expression "bindings" bindings))
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


;;;; Where strict-compound-procedure may exist
;; 0. operator must be primitive
;; 1. procedural-arg may be strict-compound-procedure
;; 1.a. Then there may be other strict-compound-procedure's inside body and env
;; 2. point 1 can be used for all other operands which may contain procedural-arg.

;; Here point 1 and 2 are manipulated by tree-map-with-strict-compound-procedure-as-elem.
;; For point 1.a., we just need to pass the correct binding to env, then body can be manipulated implicitly.


;;; tests
; (define test-compound-procedure (make-compound-procedure '() '() 'env))
; test-compound-procedure
; (pp test-compound-procedure)
; (compound-procedure () () <procedure-environment>)
; (define outside-pp pp)

; (trace extend-top-level-environment) ; no use
; (trace eval*)
(init)
;;; test1 normal
;; dot . inside parameter list is unsupported.
; (cd "~/SICP_SDF/SDF_exercises/chapter_5")
; (load "../common-lib/test_lib.scm")
; (trace-wrapper
;   (lambda () (equal? '(1 4 9) (map (lambda (x) (* x x)) '(1 2 3))))
;   strict-compound-procedure->underlying-procedure
;   eval*
;   )

;; for-each works.
; (for-each 
;   (lambda (proc) (trace proc)) 
;   (list
;     strict-compound-procedure->underlying-procedure
;     eval*
;     ))
;; assert is not one primitive, so not use it.
(equal? '(1 4 9) (map (lambda (x) (* x x)) '(1 2 3)))
; (for-each 
;   (lambda (proc) (untrace proc))
;   (list
;     ;; TODO these are all defined in (model 'get-environment). See SDF_exercises/software/sdf/manager/software-manager.scm. Unsure due to force-top-level-repl! etc.
;     strict-compound-procedure->underlying-procedure
;     eval*
;     ))

;;; test2 using outside var
(define y 3)
(equal? '(3 6 9) (map (lambda (x) (* x y)) '(1 2 3)))

;;; test3 overload var definitions and also "using outside var"
(equal? 
  (map (lambda (num) (* num y)) '(4 8 12))
  ;; here map will have 2 bindings for a.
  ;; So the a for map is 4.
  ((lambda (a) ((lambda (a) (map (lambda (x) (* x a y)) '(1 2 3))) (+ a a))) 2)
  )

;;; test4 proc arg existed deep inside operands
;; and using compound porc "var"
(define y 3)
;; should use the above definition instead of the next inside the following (map map ,,,)
(define proc1 (lambda (x) (* x y)))
; (trace procedure-printable-representation)
; (trace lookup-variable-value)
; (eq? outside-pp pp) ; #t
;; Based on the above implementation, here proc1 => strict-compound-procedure => underlying proc, so here we don't call procedure-printable-representation.
; (pp proc1)
; ("eval*" (lambda (x) (* x y)) "bindings" ((proc1 y) #[*compound-procedure 14] 3))
; (lambda (x)
;   (* x y))
; #!unspecific
(define y 4)
; (pp proc1)
; ("eval*" (lambda (x) (* x y)) "bindings" ((proc1 y) #[*compound-procedure 14] 4))

(define proc2 (lambda (x) (+ x y)))
; (trace eval*)
(equal?
  '((4 8 12) (5 6 7))
  (map 
    map
    (list proc1 proc2)
    (list '(1 2 3) '(1 2 3))))
;; see SDF_exercises/chapter_5/5_5_lexical_scope_behaviors.scm
;; here proc1's y is modified later because it is "A *top-level* definition".
;; See define-variable! which may call set!.

;;; test5 ensure env-arg is stored to implement lexical scope.
(define proc1 
  ((lambda ()
     (define y 3)
     ;; this should use the extended env by (lambda () ...) with the y binding shadowing the outside one.
     (lambda (x) (* x y))
     )))
;; still use the "top-level" y.
(define proc2 (lambda (x) (* x y)))
(equal?
  '((3 6 9) (4 8 12))
  (map 
    map
    (list proc1 proc2)
    (list '(1 2 3) '(1 2 3))))

;;; lambda-body using strict-compound-procedure
(map
  (lambda (a) (+ a (proc1 3)))
  '(1 2 3)
  )
;The object #[*compound-procedure 15] is not applicable.
;; inside debugger
; (pp #[*compound-procedure 15])
; (compound-procedure (x) (* x y) <procedure-environment>)
; (procedure-environment #[*compound-procedure 15])
; Value: #((y) (3) #((proc2 proc1 y) (#[*compound-procedure 14] #[*compound-procedure 15] 4) (*the-empty-environment*)))
;; So (strict-compound-procedure->underlying-procedure proc1) should also transform strict-compound-procedure's inside the env if that binding may be used inside proc1.
;; Just like the above (lambda (a) (+ a (proc1 3))) needs change proc1 binding in env.
;; So the recursive loop occurs.

;; No #f in the above tests, so passed.

