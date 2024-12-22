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
(+ y 3) ; should fail
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
(define (reset-var-type-if-possible name env target-type)
  (cdr (let loop ((env env))
         (let ((search-result (assq name (car env))))
          (if search-result
            (begin
              (set-cdr! search-result target-type)
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
(set!-expr? '(set! y #t))
(pp (noisy-infer-program-types '(+ 3 2)))
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
(pp 
  (noisy-infer-program-types 
    '(begin
      (define y 4)
      (+ y 3) ; application before should work
      (set! y #t)
      ((lambda (y) (+ y 3)) 4)
      (+ y 3) ; still fail since `lambda (y)` should not influence outside bindings.
      )))
