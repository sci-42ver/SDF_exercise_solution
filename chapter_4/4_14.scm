(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'unification)

;; > The specific problem shown in the critique above is not very hard to fix
;; The straightforward way is to ignore specifying types for combination-expr just as 
;; > the type of a procedure should not depend on its use
;; shows

;; > How can we handle procedures passed as arguments and returned as values?
;; > Remember
;; > that there may be free variables in a procedure that are lexically
;; > bound where the procedure was defined.
;; 0. "returned as values": I assume that is defined locally since it is weird to return one global one.
;; 0.a. We can redefine one global one by define which won't influence the global one.
;; 1. Here I won't 
;; Here maybe define won't have one new lexical context, so define is not allowed to be nested here.
;; Also see exercise_codes/SICP/4/misc-debug/definition_may_not_as_expression.scm
(define x
  (begin
    (define y (lambda (x) #f))
    (y 'ignore)
    ))
(define x
  (begin
    1
    ))
(begin
  (define y (lambda (x) #f))
  (y 'ignore)
  )

;;; Fix for the original problems
;; 0. Just ignore these constrains for arg.
;; 0.a. Same as https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html
;; > Its type would be a -> a. We have to write that using the type variable a because we can’t tell what type x has from just reading the definition of the function.
(define-generic-procedure-handler program-constraints-1
                                  (match-args type-expression? combination-expr?)
                                  (lambda (type expr)
                                    ;; Here (texpr-type (combination-operator expr)) may be already procedure-type.
                                    (cons (constrain (texpr-type (combination-operator expr))
                                                     (procedure-type 
                                                       ;; modified
                                                       ;; just creates new type-variable to imply they are unknown.
                                                       ;; Since they must be unknown, I won't put them in env for further constrain manipulation.
                                                       ;; Anyway, the types of combination-operands have been considered in env.
                                                       (map type-variable
                                                            (combination-operands expr))
                                                       ;; codomain
                                                       type))
                                          (append (program-constraints (combination-operator expr))
                                                  (append-map program-constraints
                                                              (combination-operands expr))))))

(pp (noisy-infer-program-types
      '(begin (define id (lambda (x) x))
              (id 2)
              (id #t))))
;; we can only know id returns itself.
;; So (type:procedure ((? type:6)) (? type:6))
; (t
;  (? type:6)
;  (begin (t (type:procedure ((? type:6)) (? type:6)) (define id (t (type:procedure ((? type:6)) (? type:6)) (lambda (x) (t (? type:6) x)))))
;         (t (? type:6) ((t (type:procedure ((? type:6)) (? type:6)) id) (t (numeric-type) 2)))
;         (t (? type:6) ((t (type:procedure ((? type:6)) (? type:6)) id) (t (boolean-type) #t)))))

;;; a bit general

(define-generic-procedure-handler program-constraints-1
                                  (match-args type-expression? combination-expr?)
                                  (lambda (type expr)
                                    ;; Here (texpr-type (combination-operator expr)) may be already procedure-type.
                                    (cons (constrain (texpr-type (combination-operator expr))
                                                     (procedure-type 
                                                       ;; IGNORE: Here we assume all non-proc types as one whole, so they can be matched.
                                                       ;; For proc, we
                                                       (map type-variable
                                                            (combination-operands expr))
                                                       ;; codomain
                                                       type))
                                          (append (program-constraints (combination-operator expr))
                                                  (append-map program-constraints
                                                              (combination-operands expr))))))

;; Since domain is got from operands. (see lambda-expr? and combination-expr?)
;; their numbers should be assumed same.
; (define (domains-compatible? domain1 domain2)
;   (assert (n:= (length domain1) (length domain2)))
;   (cond 
;     ((and (null? domain1) (null? domain2)) #t)
;     (else
;       (let ((op1 (car domain1))
;             (op2 (car domain2)))
;         (cond 
;           ((or ) 'to-check-later)
;           (predicate2 consequent2)))
;       )))

;; Better to change all APIs for unify to do these tracking.
;; IGNORE: For simplicity, I just use global vars here.
;; This will be messy when procedure-type exists in domain of another procedure-type.
;; So this can't work.

; (define checking-procedure-type-matching #f)
; (define checking-procedure-type-matching-stage 0)
; (define restore-proc (lambda () 'do-nothing))

; (define (unify:constant-terms terms1 terms2)
;   (let ((first1 (car terms1)) (rest1 (cdr terms1))
;         (first2 (car terms2)) (rest2 (cdr terms2)))
;     (define (unify-constants dict succeed fail)
;       (if (eqv? first1 first2)
;           (begin
;             (if (eq? first1 'type:procedure)
;               (set! checking-procedure-type-matching-stage 1))
;             (succeed dict fail rest1 rest2)
;             )
;           (fail)))
;     unify-constants))

; (define (unify:list-terms terms1 terms2)
;   (let ((first1 (car terms1)) (rest1 (cdr terms1))
;         (first2 (car terms2)) (rest2 (cdr terms2)))
;     (define (unify-lists dict succeed fail)
;       ;; TODO it is better to do this checking when we have got the most reduced forms for both.
;       ;; IGNORE: i.e. in maybe-substitute
;         ;; But that needs passing arg to show whether we are comparing 2 procedure-type domains
;         ;; and then we do the final comparison when both are constant/type:procedure's.
;         ;; I won't do this routine work to change APIs.
;         ;; Anyway the basic ideas are same.
;       ;; The above domains-compatible? can be only done once since the next match will have stripped procedure-type tags.
;       ;; 
;       (if (and (procedure-type? first1) (procedure-type? first2))
;         ;; here do explicit compatibility checking.
;         (set! checking-procedure-type-matching #t))
;       ((unify:dispatch first1 first2)
;        dict
;        ;; Here we only check domain compatibility as the following "So again we should check domain feasibility." shows.
;        (let ((restore-proc
;               (if (n:= checking-procedure-type-matching-stage 1)
;                 (begin
;                   (set! checking-procedure-type-matching-stage 2)
;                   (lambda () (set! checking-procedure-type-matching #f))
;                   )
;                 (lambda () 'do-nothing)
;                 )))
;         (lambda (dict* fail* null1 null2)
;          (assert (null? null1))
;          (assert (null? null2))
;          (restore-proc)
;          (succeed dict* fail* rest1 rest2)))
;        fail))
;     unify-lists))

(define test1 
  '(begin
     (define demo
       (lambda (x)
         (begin
           (define inner
             ;; "lexical" context is ensured by annotate-expr for lambda-expr?.
             ; (lambda (y) (+ (x y) y))
             (lambda (y) (x y))
             )
           inner)
         )
       )
     ;; 0. based on abstraction, we only need to know domain and codomains for passed procedure arg.
     ;; 1. Notice here (x y) implies the type of the proc returned by demo depends on lexical context.
     ;; So the passed arg of the outer demo also depends on that.
     ;; Maybe this is what the author tries to convey by
     ;; > make it as general as you can.
     ;; See "passed around expectedly" context.
     ;; 1.a. IGNORE: Anyway I don't know what is general in "the general case is complicated".
     ;; Maybe there are cases beyond "procedures passed as arguments and returned as values".
     (demo (demo (lambda (x) (* x x))))
     ;; 0. If there are branches in demo, then passing 1 may be allowed.
     ;; e.g. (or x inner) where x is allowed to be #f or (or (not x) inner) where x has no valid vals.
     ;; Maybe this is what the author means for "the general case is complicated".
     ;; 1. If we use original program-constraints-1
     ;; Otherwise the following can't be connected
     ; (= (? num:22) (numeric-type))
     ; (= (? demo:9) (type:procedure ((? |(t (? num:22) num):25|)) (? type:23)))
     ;; 1.a. If we know the arg must be proc, then we should deny other types.
     ;; But when constructing constrains, we can *only* know num is one var here since we can't get former constraints in program-constraints-1.
     ;; So we can't do denial there.
     ;; 1.a.0. IMHO denial should be done when unify which is just the next step of the above constraint construction.
     ;; But then we should change the general unify behavior to allow match like (notice this is after some reduction like (? num:22) -> (numeric-type)):
     ; (type:procedure (numeric-type) (? type:23))
     ; (type:procedure (boolean-type) (? type:23))
     ;; while not allow:
     ; (type:procedure (numeric-type) (? type:23))
     ; (type:procedure (type:procedure ...) (? type:23))
     ;; So change unify to be more specific...
     (define num 1)
     (demo num)
     ;; 1.a.1. Here we have no restriction on codomain since that should be *only decided* by the procedure itself.
     ;; If there is one error when inference, that is due to *wrong usage* of the returned value instead of that the procedure returned one wrong value.
     ;; So again we should check domain feasibility.
     (define codomain-known-proc (lambda (x) #f))
     (define demo2
       (lambda (x)
         (begin
           (define inner
             (lambda (y) (< (x y) 1))
             )
           inner)
         )
       )
     (demo2 codomain-known-proc)
     )
  )
; (lambda (x) (define inner (lambda (y) (+ ... y))) inner)
(pp (noisy-infer-program-types test1))
;; Notice here 2 (? x:2)'s.
;; So lexical context is captured expectedly.
; (t
;  (? type:14)
;  (begin
;   (t
;    (? demo:1)
;    (define demo
;      (t
;       (type:procedure ((? x:2)) (? type:8))
;       (lambda (x)
;         (t (? type:7) (begin (t (? inner:3) (define inner (t (type:procedure ((? y:4)) (? type:6)) (lambda (y) (t (? type:5) ((t (? x:2) x) (t (? y:4) y))))))) (t (? inner:3) inner)))))))

;; based on unify:gdispatch ordering, thing-element-var is matched first.
;; So (? type:13) is bound to (? type:14).
; (= (? type:14) (? type:13))
; (= (? x:10) (type:procedure ((? |(t (? y:12) y):27|)) (? type:13)))

;; Here
;; 1. x type won't be influenced by application of y.
;; So "(? |(t (? y:12) y):29|)".
;; 2. Here (? type:14) is passed around expectedly.
;; The output of annotate-program has only one (? type:14).
; (begin
;  (define demo
;    (lambda (x)
;      (declare-type x (type:procedure ((? |(t (? y:12) y):29|)) (? type:14)))
;      (define inner
;        (lambda (y)
;          (declare-type y (? y:12))
;          (x y)))
;      (declare-type inner (type:procedure ((? y:12)) (? type:14)))
;      inner))
;  (declare-type demo (type:procedure ((type:procedure ((? |(t (? y:12) y):29|)) (? type:14))) (type:procedure ((? y:12)) (? type:14))))
;  (demo (demo (lambda (x) (declare-type x (? x:17)) (* x x))))
;  (demo 1))

;;; "free variables" created by define instead of by arg passing.
(define test2
  '(begin
     (define demo
       (lambda (x)
         (begin
           (define z 2)
           (define inner
             (lambda (y) (x z y))
             )
           inner)
         )
       )
     (demo (demo (lambda (x y) (* x y))))
     ))
(pp (noisy-infer-program-types test2))

(define test3
  '(begin
     (define demo
       (lambda (x)
         (begin
           (define z 2)
           (define inner
             (lambda (y) z)
             )
           inner)
         )
       )
     (demo (demo (lambda (x y) (* x y))))
     ))
(pp (noisy-infer-program-types test3))
;; local z can be recognized.
;  (begin (t (? z:72) (define z (t (numeric-type) 2)))
;         (t (? inner:73) (define inner (t (type:procedure ((? y:74)) (? type:75)) (lambda (y) (t (? z:72) z)))))
;         (t (? inner:73) inner)))))))

(define test4
  '(begin
     (define demo
       (lambda (x)
         (begin
           (define z (lambda () 2))
           (define inner
             (lambda (y) (z))
             )
           inner)
         )
       )
     (demo (demo (lambda (x y) (* x y))))
     ))
(pp (noisy-infer-program-types test4))
