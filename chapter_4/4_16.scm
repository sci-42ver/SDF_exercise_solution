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
(pp (noisy-infer-program-types 
  '(begin
    (define demo
      (lambda (x y)
        (if y
          (* x x)
          ;; use this when with union, so returned type is (?:union (numeric-type) (boolean-type))
          ; (< x 2)
          (+ x 2)
          )
        ))
    (demo (demo 3 #t) #t)
    )))
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

