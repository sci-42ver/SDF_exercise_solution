(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'unification)

;; Here the straightforward way is that we just replace (numeric-type) with something like (numeric-or-string-type)
;; But how does this symbol match with string or (numeric-type).
(pp (noisy-infer-program-types '(+ 2 3)))
;; this
; (= (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) (type:procedure ((numeric-type) (numeric-type)) (? type:1)))
;; will be 
; (= (type:procedure ((numeric-or-string-type) (numeric-or-string-type)) (numeric-or-string-type)) (type:procedure ((numeric-type) (numeric-type)) (? type:1)))
;; Then if using string, the rhs *must* be ((string-type) ...).
;; So this just means one lhs can match *2 possible rhs's*.
;; But there is no such thing in unify.scm.
;; > Does this extension require us to modify the unifier? If so, explain why it is necessary.
;; So yes. Reasons are shown above.

;; > What must be done to extend the system to support union types?
;; 0. IMHO just use one (?:union ...) similar to denotation (?:choice ...) in 4.6.
;; Then the above 1-to-2 or 1-to-n matching can be done.
;; 1. Here union types can work also for "if".
;; Then rhs may also have union types.
;; Then we need to ensure lhs>=rhs (*not bidirectional*).
;; But the problems is that we may have 2 sides both with var's, so how to know which is lhs?
(define (lambda-test)
  (pp (noisy-infer-program-types 
        '(begin
           (define demo
             (lambda (x y)
               (if y
                 ;; IGNORE: TODO one problem here is that if we constrain for (+ x 2) first, then x is bound to ?:union.
                 ;; IGNORE: But ?:union will be forbidden by (* x x).
                 ;; Anyway this is expected since (* x x) doesn't support string.
                 ;; Here the allowed type for x actually depends on y... This is beyond what this exercise tries to implement.
                 (* x x)
                 ;; use this when with union, so returned type is (?:union (numeric-type) (boolean-type))
                 ; (< x 2)
                 (+ x 2)
                 )
               ))
           (demo (demo 3 #t) #t)
           ))))
(lambda-test)
;; This is the constraint got by definition.
; (= (? demo:2) (type:procedure ((? x:3) (? y:4)) (? type:8)))
; ...
;; This is the constraint got by application.
; (= (? demo:2) (type:procedure ((? type:9) (boolean-type)) (? type:10)))
; (= (? demo:2) (type:procedure ((numeric-type) (boolean-type)) (? type:9)))
;; Here (? type:8) can be union, similar for (? type:9).
;; So here we should forbid since (? x:3)<(? type:9).
;; But here it is better to have no assumption for constraint order.
;; 1.a. 
;; However, SDF_exercises/software/sdf/unification/type-resolver.scm
;; only considers boolean?, number?, symbol?, if-expr?, lambda-expr?, combination-expr?, define-expr?, begin-expr?.
;; Among these, only lambda-expr? can construct new procedure.
;; So we can assume when application, procedure constraints can know (? x:3) etc if possible (not for identity procedure etc).
;; So we will have lhs with definition type:procedure and rhs with application derived type:procedure expectedly.
;; 1.a.0. Anyway, as 4.14 shows, application should not constrain procedure type.
;; Then we just define union type for procedure argument, that won't be forbidden later at all.
;; Emm... 4.14 is a bit hard, but this is related with 4.14 someway. So this may be as author says
;; > This is not easy.
;; 1.a.1. So here I assume lhs>=rhs should be always met and unify always has lhs as the 1st term.
;; As 1.a.0. says, this may fail for some tricky cases.
;; What's more, I assume that lhs has all possible types which can be defined available, i.e. (? x:3) (? y:4) etc in the above.
;; 1.a.1.a. Notice here I won't implement the deduction from application types to defined types, so that (id 1) and (id #f) implies id can accept numeric-or-boolean-type
;; As 4.14 says this is weird and should be forbidden.
;; 1.b.
;; full trace of the above demo:
; (= (? demo:2) (type:procedure ((? x:3) (? y:4)) (? type:8)))
;; will be
; (= (? demo:2) (type:procedure ((numeric-type) (boolean-type)) (numeric-boolean-type)))
;; Then (? type:9) is bound to (numeric-type) and (? type:10)->(numeric-boolean-type).
;; Then (= (? demo:2) (type:procedure ((numeric-type) (boolean-type)) (? type:9)))
;; will unify ((? type:8) (? type:9)) which has (? type:9) as var.
;; Then ((numeric-type) (? type:8)) and then ((numeric-boolean-type) (numeric-type)) which again has the expected lhs.
;; But "(? type:9) is bound to (numeric-type)" unexpectedly.
;; Anyway this is the same problem as 4.14 where deriving type based on application may have errors.

;; Implementation based on 1.a.1.
;; 0. The above assumption about lhs is always at the left part of unify is wrong due to unify may reorder *all later terms* when both terms have var as the first.
;; IMHO we can add one tag for procedure definition by lambda or make-top-level-env-frame.
;; Then we can always recognize that expected lhs.
;; 0.a. lambda may have var which has no binding in union-term (like identity procedure), then it will always succeed for type constraint.
;; IGNORE (support has been added): TODO add support for lambda with union types.
(load "4_16_union_lib.scm")
(load "4_16_type-expression_reload.scm")
;; IGNORE: TODO weird assoc can't recognize match-args created obj.
;; This may be due to cons always creates one new  data structure.
(assoc (match-args (car-satisfies union-term?)
                   (car-satisfies list-term?))
       (list 
         (cons 
           (match-args (car-satisfies union-term?)
                       (car-satisfies list-term?))
           'ignored
           )))
;Value: #f
(assoc 1 (list (cons 1 'ignored)))

; (define-generic-procedure-handler unify:gdispatch
;   (match-args (car-satisfies union-term?)
;               (car-satisfies list-term?))
;   unify:left-with-union-term)
(load "4_16_type_lib.scm")

(define (make-top-level-env-frame)
  (let ((binary-numerical
          (let ((v (numeric-type)))
            (procedure-type (list v v) v)))
        (binary-comparator
          (let ((v (numeric-type)))
            ;; See `(define-parametric-type-operator operator)` -> (lambda operands ...)
            (procedure-type (list v v) (boolean-type))))
        ;; added
        (binary-numerical-string
          (let ((v (add-procedure-definition-tag (union-term (numeric-type) (string-type)))))
            (procedure-type (list v v) v)))
        )
    (list (cons '+ binary-numerical-string)
          (cons '- binary-numerical)
          (cons '* binary-numerical)
          (cons '/ binary-numerical)
          (cons '= binary-comparator)
          (cons '< binary-comparator)
          (cons '> binary-comparator)
          (cons '<= binary-comparator)
          (cons '>= binary-comparator))))
; (trace unify)
(trace unify:list-terms)
(pp (noisy-infer-program-types '(begin (+ 2 3) (+ "1" "2"))))
; (t
;  (procedure-definition ?:union (numeric-type) (string-type))
;  (begin
;   (t
;    (procedure-definition ?:union (numeric-type) (string-type))
;    ((t
;      (type:procedure ((procedure-definition ?:union (numeric-type) (string-type)) (procedure-definition ?:union (numeric-type) (string-type)))
;                      (procedure-definition ?:union (numeric-type) (string-type)))
;      +)
;     (t (numeric-type) 2)
;     (t (numeric-type) 3)))
;   (t
;    (procedure-definition ?:union (numeric-type) (string-type))
;    ((t
;      (type:procedure ((procedure-definition ?:union (numeric-type) (string-type)) (procedure-definition ?:union (numeric-type) (string-type)))
;                      (procedure-definition ?:union (numeric-type) (string-type)))
;      +)
;     (t (string-type) "1")
;     (t (string-type) "2")))))

;; IGNORE: See the 2nd TODO in SDF_exercises/chapter_4/4_16_union_lib.scm.
(define (lambda-test-with-union)
  (pp (noisy-infer-program-types 
        '(begin
           (define demo
             (lambda (x y)
               (if y
                 (* x x)
                 ;; use this when with union, so returned type is (?:union (numeric-type) (boolean-type))
                 (< x 2)
                 )
               ))
           (demo (demo 3 #t) #t)
           ))))
;; Skip problems in this test. See above.
; (lambda-test)
(lambda-test-with-union)
;; capture ?:union from if appropriately.
; (t
;  (procedure-definition ?:union (numeric-type) (boolean-type))
;  (begin
;   (t
;    (type:procedure ((numeric-type) (boolean-type)) (procedure-definition ?:union (numeric-type) (boolean-type)))
;    (define demo
;      (t
;       (type:procedure ((numeric-type) (boolean-type)) (procedure-definition ?:union (numeric-type) (boolean-type)))
;       (lambda (x y)
;         (t
;          (procedure-definition ?:union (numeric-type) (boolean-type))
;          (if (t (boolean-type) y)
;              (t (numeric-type) ((t (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) *) (t (numeric-type) x) (t (numeric-type) x)))
;              (t (boolean-type) ((t (type:procedure ((numeric-type) (numeric-type)) (boolean-type)) <) (t (numeric-type) x) (t (numeric-type) 2)))))))))
;   (t
;    (procedure-definition ?:union (numeric-type) (boolean-type))
;    ((t (type:procedure ((numeric-type) (boolean-type)) (procedure-definition ?:union (numeric-type) (boolean-type))) demo)
;     (t (numeric-type) ((t (type:procedure ((numeric-type) (boolean-type)) (procedure-definition ?:union (numeric-type) (boolean-type))) demo) (t (numeric-type) 3) (t (boolean-type) #t)))
;     (t (boolean-type) #t)))))

;; IGNORE: TODO use demo returned type for one procedure accepting something like string to throw errors.
; (define (lambda-test-with-union-throw-error)
;   (pp (noisy-infer-program-types 
;     '(begin
;       (define demo
;         (lambda (x y)
;           (if y
;             (* x x)
;             (< x 2)
;             )
;           ))
;       (demo 2 #t)
;       ))))
; (lambda-test-with-union-throw-error)

;; throw errors.
(pp (noisy-infer-program-types '(+ "2" #t)))
