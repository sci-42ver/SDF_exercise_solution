(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'unification)

;; IGNORE: Here I won't consider the case where one local procedure changes the global variable binding.
;; Otherwise, we need to track the program execution order at runtime which is even more complex when with stream or lazy evaluation as SICP says.
;; TODO This is said in one QA chat of https://stackoverflow.com/users/21294350/an5drama.
;; So only allow set! etc to change local bindings.

;; Here I also won't consider the type depending on 
;; IGNORE: running order and 
;; whether lazy.
;; So just think about sequential program as the original type-inference system does.
(define y 4)
(+ y 3) ; application before should work
(set! y #t)
; (+ y 3) ; should fail
((lambda (y) (+ y 3)) 4) ; should work
;; expected behavior:
;; 0. assignment will influence the local var type *after* its application.
;; 0.a. IGNORE: Won't influence outside the local env based on assumption.
;; 0.a.0. If considering assignment of global var, .
;; 1. Here set-car! or set-cdr! etc won't change the type, so skipped.
;; 1.a. After all, this is one incomplete implementation since set-car! may change the car part data type.
;; I won't dig into that since there are many set-foo! in MIT_Scheme_Reference doc.
(define (test x)
  (write-line y)
  (define (inner-test)
    (define y 5)
    (set! y 6)
    y
    )
  (set! y 3)
  (write-line (inner-test))
  (write-line y) ; should be 3
  )
(test 'ignored)

;; similar to helpers for if-expr etc.
(define (set!-expr? object)
  (and (list? object)
       (= (length object) 3)
       (eq? (car object) 'set!)
       ;  (match:element-var? (set!-var object))
       (let ((var (set!-var object)))
         (or (symbol? var) (texpr? var)))
       ))

(define set!-var cadr)
(define set!-val caddr)

;; similar to SICP chapter 4.
(define (make-set!-expr var val)
  (list 'set! var val))

(define annotate-expr-with-type
  (simple-generic-procedure 'annotate-expr-with-type 3 #f))
(define-generic-procedure-handler annotate-expr-with-type
                                  (match-args symbol? any-object? type-expression?)
                                  (lambda (expr env type)
                                    (make-texpr (reset-var-type-if-possible expr env type) expr)))
;; to make all later search for name type use the new target-type.
(define restore-env-queue (list 'restore-env-queue))
(define (enqueue queue data assertion?)
  (assert (assertion? data))
  (set-cdr! (last-pair queue) (list data))
  )

(define define-name-stack (list 'define-name-stack))
(define stack-contents cdr)
(define (stack-empty? stack)
  (null? (stack-contents stack)))
(define (pop-stack stack)
  ;; simplification of https://stackoverflow.com/a/77711423/21294350
  (if (stack-empty? stack)
    (error "can't pop")
    (let loop ((cur-stack stack))
      (if (null? (cddr cur-stack))
        (set-cdr! cur-stack '())
        (loop (cdr cur-stack)))
      )
    )
  ; (set-cdr! (last-pair stack) '())
  )
(define (get-stack-top stack)
  (and (stack-empty? stack)
       (last stack))
  )
(define push-stack enqueue)

;; See exercise_codes/SICP/4/4_34_revc.scm
(define lambda-reset-env-schedule-table (make-hash-table))
(define (get-reset-proc name)
  (let ((proc (hash-table-ref/default lambda-reset-env-schedule-table name #f)))
    (and 
      proc
      (assert 
        (and (procedure? proc)
             (equal? (procedure-arity proc) (cons 0 0))
             ))
      )
    proc
    )
  )
(define (set!-reset-proc name proc)
  (hash-table-set! lambda-reset-env-schedule-table name proc)
  )

(define (reset-var-type-if-possible name env target-type)
  (cdr (let loop ((env env))
         (let ((search-result (assq name (car env))))
           (if search-result
             (let ((orig-type (cdr search-result)))
               (set-cdr! search-result target-type)
               (let ((current-define-name (get-stack-top define-name-stack)))
                 (if current-define-name
                   (let ((proc (get-reset-proc current-define-name)))
                     (if proc
                       (set!-reset-proc current-define-name (lambda () (proc) (set-cdr! search-result target-type)))
                       (set!-reset-proc current-define-name (lambda () (set-cdr! search-result target-type)))
                       )
                     (write-line (list "reset-var-type-if-possible set!-reset-proc" proc current-define-name))    
                     ;; Here due to env, when lambda is applied, search-result will automatically point to the above one.
                     (enqueue restore-env-queue (lambda () (set-cdr! search-result orig-type)) procedure?)
                     )
                   ;; not in define or lambda
                   ;; So no need for reset-proc and 
                   'ignored)
                 )
               search-result
               )
             (if (pair? (cdr env))
               (loop (cdr env))
               (error "should init var-type first for set!")
               ;  (let ((tcell (make-type-cell name)))
               ;    (set-car! env (cons tcell (car env)))
               ;    tcell)
               ))
           ))))
(define-generic-procedure-handler annotate-expr
                                  (match-args set!-expr? any-object?)
                                  (lambda (expr env)
                                    (write-line "call annotate-expr for set!-expr?")
                                    (let* ((val-expr (set!-val expr))
                                           (val-annotated (annotate-expr val-expr env))
                                           ;  (val-type (get-var-type val-expr env))
                                           (val-type (texpr-type val-annotated))

                                           (var-expr (set!-var expr))
                                           (old-var-type (get-var-type var-expr env))
                                           )
                                      (make-texpr old-var-type ; Here I assume to return old-var-type same as MIT/GNU Scheme.
                                                  (make-set!-expr
                                                    (annotate-expr-with-type var-expr env val-type)
                                                    val-annotated
                                                    ))
                                      )))

;; constraints are all contained in env setting in annotate-expr.
(define-generic-procedure-handler program-constraints-1
                                  (match-args type-expression? set!-expr?)
                                  (lambda (type expr)
                                    (write-line "call program-constraints for set!-expr?")
                                    '()
                                    ))

;; coderef: simplify-combination
(define-generic-procedure-handler simplify-annotated-program-1
                                  (match-args type-expression? set!-expr?)
                                  (lambda (type expr)
                                    (write-line "call simplify-annotated-program-1 for set!-expr?")
                                    (make-set!-expr
                                      (simplify-annotated-program (set!-var expr))
                                      (simplify-annotated-program (set!-val expr)))))

;; test1
; (set!-expr? '(set! y #t))
; (pp (noisy-infer-program-types '(+ 3 2)))
(pp 
  (noisy-infer-program-types 
    '(begin
       (define y 4)
       (+ y 3) ; application before should work
       (set! y #t)
       (+ y 3) ; should fail
       ((lambda (y) (+ y 3)) 4)
       )))
;; Here the 2nd y has boolean-type, so error.
; (t
;  (? type:9)
;  (begin
;   (t (? y:2) (define y (t (numeric-type) 4)))
;   (t (? type:3) ((t (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) +) (t (? y:2) y) (t (numeric-type) 3)))
;   (t (? y:2) (set! (t (boolean-type) y) (t (boolean-type) #t)))
;   (t (? type:4) ((t (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) +) (t (boolean-type) y) (t (numeric-type) 3)))
;   (t
;    (? type:8)
;    ((t (type:procedure ((? y:5)) (? type:7)) (lambda (y) (t (? type:6) ((t (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) +) (t (? y:5) y) (t (numeric-type) 3)))))
;     (t (numeric-type) 4)))))
(pp 
  (noisy-infer-program-types 
    '(begin
       (define y 4)
       (+ y 3) ; application before should work
       (set! y #t)
       ; (+ y 3) ; should fail
       ((lambda (y) (+ y 3)) 4)
       )))

;; test2 preparation
;; IGNORE: Notice here it is fine to decide type inside lambda when
;; Here set! should not be done until that lambda is called. But that needs delay for annotate-expr.
;; Then we need to change the overall structure.
;; Here I doesn't use delay but just uses thunk with auto env to do restoration when necessary.

(define restore-env-queue-contents cdr)
(define (reset-restore-env-queue)
  (set! restore-env-queue (list 'restore-env-queue))
  )
(define (run-restore-env-queue)
  (for-each (lambda (proc) (proc)) (restore-env-queue-contents restore-env-queue))
  (reset-restore-env-queue)
  )
;; TODO this assumes each application of procedure will set! for the *same* data.
;; But that won't be true when used in different nested levels.
;; To make that work, we need to change the program structure *further* so that combination-expr needs to do annotate-expr for lambda *again*.
;; That is also related with 4.18.
;; So that is not "elegantly extended".
(define-generic-procedure-handler annotate-expr
                                  (match-args define-expr? any-object?)
                                  (lambda (expr env)
                                    ;; different from SICP, here not consider (define (proc ...) ...).
                                    (let ((name (define-name expr))
                                          (val (define-value expr)))
                                      (let ((type (define-var-type name env))
                                            (val-whether-lambda (lambda-expr? val)))
                                        ;; modified
                                        (if val-whether-lambda
                                          (push-stack define-name-stack name symbol?)
                                          'ignored)
                                        (let ((res (make-texpr type
                                                               (make-define-expr name
                                                                                 (annotate-expr
                                                                                   val
                                                                                   env)))))
                                          (if val-whether-lambda
                                            (begin
                                              (run-restore-env-queue)
                                              (pop-stack define-name-stack)
                                              )
                                            'ignored)
                                          res
                                          )
                                        ))))
(define-generic-procedure-handler annotate-expr
                                  (match-args combination-expr? any-object?)
                                  (lambda (expr env)
                                    (let* ((proc-name (combination-operator expr))
                                           (whether-use-lambda-proc (lambda-expr? proc-name))
                                           )
                                      ;; proc-name may be lambda, then we don't need to restore set!.
                                      (if whether-use-lambda-proc
                                        (push-stack define-name-stack proc-name lambda-expr?)
                                        ;; has been manipulated by define.
                                        'do-nothing-for-symbol-proc-name
                                        )
                                      (let ((res (make-texpr (type-variable)
                                                             (make-combination-expr
                                                               (annotate-expr proc-name env)
                                                               (map (lambda (operand)
                                                                      (annotate-expr operand env))
                                                                    (combination-operands expr))))))
                                        (if whether-use-lambda-proc
                                          (pop-stack define-name-stack)
                                          'ignored)
                                        (if (not whether-use-lambda-proc)
                                          (let ((proc (get-reset-proc proc-name)))
                                            (write-line (list "call combination-expr? annotate-expr with symbol proc name" proc-name proc))
                                            (and 
                                              proc
                                              (proc)))
                                          'lambda-not-do-restoration)
                                        res
                                        )
                                      )
                                    ))
;; test2
;; nested define.
(pp 
  (noisy-infer-program-types 
    '(begin
       (define y 4)
       (define test 
         (lambda (x) 
           (begin
             (+ y 4)
             (define inner-test
               (lambda () 
                 (begin
                   (define y 5)
                   (+ y 4)
                   (set! y #t)
                   y
                   )
                 )
               )
             (inner-test)
             (+ y 4) ; should work
             (set! y #t)
             y
             )
           )
         )
       (+ y 4) ; This will fail when not using delay or its alternative...
       (test ignored)
       )))
;; 0. Here the local y assignments in inner-test won't influence the outside due to new frame.
;; So `y:21` and `y:16`.
;; 1. set! same as the above will influence the latter type inferences.
;; So (t (boolean-type) y) at last.
;; 2. (= (? y:16) (numeric-type)) and (= (? y:21) (numeric-type)) won't be influenced by set!.

; (t
;  (? type:32)
;  (begin
;   (t (? y:16) (define y (t (numeric-type) 4)))
;   (t
;    (? test:17)
;    (define test
;      (t
;       (type:procedure ((? x:18)) (? type:28))
;       (lambda (x)
;         (t
;          (? type:27)
;          (begin
;           (t (? type:19) ((t (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) +) (t (? y:16) y) (t (numeric-type) 4)))
;           (t
;            (? inner-test:20)
;            (define inner-test
;              (t
;               (type:procedure () (? type:24))
;               (lambda ()
;                 (t
;                  (? type:23)
;                  (begin (t (? y:21) (define y (t (numeric-type) 5)))
;                         (t (? type:22) ((t (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) +) (t (? y:21) y) (t (numeric-type) 4)))
;                         (t (? y:21) (set! (t (boolean-type) y) (t (boolean-type) #t)))
;                         (t (boolean-type) y)))))))
;           (t (? type:25) ((t (? inner-test:20) inner-test)))
;           (t (? type:26) ((t (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) +) (t (? y:16) y) (t (numeric-type) 4)))
;           (t (? y:16) (set! (t (boolean-type) y) (t (boolean-type) #t)))
;           (t (boolean-type) y)))))))
;   (t (? type:29) ((t (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) +) (t (boolean-type) y) (t (numeric-type) 4)))
;   (t (? type:31) ((t (? test:17) test) (t (? ignored:30) ignored)))))

;; test3
;; Here y is implicitly not passed by reference when used for the construction of other data types.
;; So set-car! actually can't change types if we think list<int> is same as list<double> etc (use cpp syntax here for parametric types. See 4.15).
(define y 3)
(set-car! (cons y 4) #t)
y

;; test4
;; multiple set! inside lambda.
(pp 
  (noisy-infer-program-types 
    '(begin
       (define y 4)
       (define z 4)
       (define test 
         (lambda (x) 
           (begin
             (+ y 4)
             (define inner-test
               (lambda () 
                 (begin
                   (define y 5)
                   (+ y 4)
                   (set! y #t)
                   y
                   )
                 )
               )
             (inner-test)
             (+ y 4)
             (set! y #t)
             (set! z #t)
             )
           )
         )
       (+ y 4)
       (test ignored)
       ;; Here both should be boolean.
       y
       z
       )))

;; test5
;; set! inside lambda.
(pp 
  (noisy-infer-program-types 
    '(begin
       (define y 4)
       (define z 4)
       ((lambda (x) 
          (begin
            (+ y 4)
            (define inner-test
              (lambda () 
                (begin
                  (define y 5)
                  (+ y 4)
                  (set! y #t)
                  y
                  )
                )
              )
            (inner-test)
            (+ y 4) ; should work
            (set! y #t)
            (set! z #t)
            )
          )
        ignored
        )
       ;; Here both should be boolean.
       y
       z
       )))

; (pp 
;   (noisy-infer-program-types 
;     '(begin
;       (define y 4)
;       (define z 4)
;       ((lambda (x) 
;           (begin
;             (+ y 4)
;             (define inner-test
;               (lambda () 
;                 (begin
;                   (define y 5)
;                   (+ y 4)
;                   (set! y #t)
;                   y
;                   )
;                 )
;               )
;             (inner-test)
;             (+ y 4) ; should work
;             (set! y #t)
;             (set! z #t)
;             )
;           )
;         ignored
;         )
;       (+ y 4) ; fail
;       ;; Here both should be boolean.
;       y
;       z
;       )))
