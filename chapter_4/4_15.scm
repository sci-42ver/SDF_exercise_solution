(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'unification)

;; > Parametric type
;; https://docs.julialang.org/en/v1/manual/types/#Parametric-Types
;; > This declaration defines a new parametric type, Point{T}

;; 0. > map procedure operates on lists of objects of any type.
;; i.e. List{T}
;; So a bit different from 4.14 id with T.
;; 1. Here it is a bit too general with T1{T2}. Anyway, the normal usage is not that way.
;; 1.a. Here I write a lot of codes based on the new proc list-type.
;; TODO we can abstract with one X-type, so it may be easy to add new thing like seq-type...
;; 1.b. Notice the original code base has already one parametric-type? but only for procedure.
;; 2. > Does this extension require us to modify the unifier?
;; No.
;; > If not, explain why it is not necessary.
;; Since we have one (? type:...) to denote T.
;; Then just use one tag which is taught in SICP to show List{T}.
;; Or straightforward reason is that "the original code base has already one parametric-type?".
;; 3. > What must be done to extend the system to support parametric types?
;; For list, just one new list-type and generic procedures and helpers based on that.
;; For others, see point 1.

;; This exercise doesn't require us to "work with" variable-length argument
;; So I will only implement one unary-map
(define unary-map
  (lambda (proc items)
    (if (null? items)
      '()
      (cons (proc (car items))
            (unary-map proc (cdr items))))))

;;; car cdr null?
;; 0. similar to combination but has more constraints on input and output.
;; 1. Here null? returns boolean will be constrained by if.
;; 2. For simplicity, I allow List{T} to match null list.
(define list-pred-or-selector-combination '(car cdr null?))
(define (list-pred-or-selector-combination-expr? object)
  (and (combination-expr? object)
       (memq (car object) list-pred-or-selector-combination)))
;; This won't influence annotate-expr etc due to match-args wrapping.
; ;; not all special-forms.
; (define a-bit-special-forms '(if lambda define begin))
; (define (combination-expr? object)
;   (and (list? object)
;        (>= (length object) 1)
;        (not (memq (car object) (append list-pred-or-selector-combination a-bit-special-forms)))))

;; Here just give one demo for symbol.
(define annotate-list-expr
  (simple-generic-procedure 'annotate-list-expr 2 #f))
(define (get-list-var-type name env)
  (cdr (let loop ((env env))
         (or (assq name (car env))
             (if (pair? (cdr env))
                 (loop (cdr env))
                 ;; modified
                 (let ((tcell (make-list-type-cell name)))
                   (set-car! env (cons tcell (car env)))
                   tcell))))))
(define (make-list-type-cell name)
  ; (write-line (list "debug make-list-type-cell" (type-variable name)))
  (cons name (list-type (type-variable name))))

; (define (list-type base)
;   `(list ,base))
;; To make type-expression? able to recognize this.
(define list-type)
(define list-type?)
(receive (constructor predicate)
    (define-parametric-type-operator 'type:list)
  (set! list-type constructor)
  (set! list-type? predicate))
(define list-type-base cadr)

;; annotate-list-expr just ensure it returns one list type.
(define-generic-procedure-handler annotate-list-expr
  (match-args symbol? any-object?)
  (lambda (expr env)
    (let ((type (get-list-var-type expr env)))
      ; (write-line (list "call annotate-list-expr for symbol?" expr type (list-type? type)))
      (if (list-type? type)
        (make-texpr type expr)
        ; (error '***type-error***)
        (make-texpr (add-list-var-type expr env) expr)
        )
      )))
(define-generic-procedure-handler annotate-list-expr
  (match-args combination-expr? any-object?)
  (lambda (expr env)
    (make-texpr (list-type (type-variable))
                (make-combination-expr
                 (annotate-expr (combination-operator expr) env)
                 (map (lambda (operand)
                        (annotate-expr operand env))
                      (combination-operands expr))))))

;; 0. generic get-handler will get the latter added procedure if possible.
;; 1. Here I didn't constrain car input and outputs, similar to cdr.
;; But this can be done similar to cons (TODO), I won't do the routine duplicate work.
(define-generic-procedure-handler annotate-expr
  (match-args list-pred-or-selector-combination-expr? any-object?)
  (lambda (expr env)
    ; (write-line (list "call list-pred-or-selector-combination-expr? annotate-expr for" expr))
    (make-texpr (type-variable)
                ;; This will match program-constraints-1 for combination-expr?.
                (make-combination-expr
                 (annotate-expr (combination-operator expr) env)
                 (map (lambda (operand)
                        ;; modified
                        (annotate-list-expr operand env))
                      (combination-operands expr))))))
;; Here we only add one tag for operand types.
;; So program-constraints-1 and simplify-annotated-program-1 can work as before.

;;; '()
;; IGNORE: put the general at first to match at last if possible.
;; Here 2 preds has no intersection with other preds, so no worry for overloading when added after other preds.
(define (add-list-var-type name env)
  ;; always add one binding for the local frame (i.e. (car env))
  (cdr (let ((tcell (make-list-type-cell name)))
          (set-car! env (cons tcell (car env)))
          tcell)))
(define-generic-procedure-handler annotate-expr
  (match-args null? any-object?)
  (lambda (expr env)
    ; (write-line "call annotate-expr for null?")
    ;; This list type should depends on the context for consistency just as the above shows.
    (make-texpr (add-list-var-type expr env) expr)
    ))
;; Here we assume all types are same. Otherwise use union in 4.16. Here gives one demo for all numbers.
(define (list-of? base?)
  (lambda (obj) 
    (and
      (list? obj)
      ;; not overload null?
      (not (null? obj))
      (every base? obj)
      )
    ))
(define-generic-procedure-handler annotate-expr
  (match-args (list-of? number?) any-object?)
  (lambda (expr env)
    ; (write-line (list "call annotate-expr for (list-of? number?):" expr))
    (make-texpr (list-type (numeric-type)) expr)
    ))
;; rest trivial settings
(define-generic-procedure-handler program-constraints-1
  (match-args type-expression?
              (disjoin null? (list-of? number?)))
  (lambda (type expr)
    '()))

(define-generic-procedure-handler simplify-annotated-program-1
  (match-args type-expression?
              (disjoin null? (list-of? number?)))
  (lambda (type expr)
    expr))

;;; cons
; (define list-constructor-combination '(cons))
(define (cons-expr? object)
  (and (list? object)
       ;; modified
       (n:= (length object) 3)
       (eq? (car object) 'cons)))

(define (make-cons-combination-expr operator operands)
  (assert (eq? 'cons (texpr-expr operator)))
  (cons 'cons (cons operator operands)))
(define (cons-combination-expr? object)
  (and (list? object)
       ;; modified
       (n:= (length object) 4)
       (eq? (car object) 'cons)))
(define cons-combination-operator cadr)
(define cons-combination-operands cddr)

(define-generic-procedure-handler annotate-expr
  (match-args cons-expr? any-object?)
  (lambda (expr env)
    (make-texpr (list-type (type-variable)) ; modified
                ;; IGNORE: This will match program-constraints-1 for combination-expr?.
                ;; modified to call the specific cons-combination-expr? handler.
                (make-cons-combination-expr
                 (annotate-expr (combination-operator expr) env)
                 ;; modified
                 (let ((operands (combination-operands expr)))
                  (list (annotate-expr (car operands) env) (annotate-list-expr (cadr operands) env)))
                  ))))
;; IGNORE: Here assume cons accepts data with the same base type.
;; It is better to constrain with union that the result type is union of 2 bases.
(define-generic-procedure-handler program-constraints-1
  (match-args type-expression? cons-combination-expr?)
  (lambda (type expr)
    ; ; (write-line "call cons-combination-expr? program-constraints-1")
    (cons (constrain (texpr-type (cons-combination-operator expr))
                     (procedure-type 
                      ;; domain
                      (map texpr-type
                           (cons-combination-operands expr))
                      ;; codomain
                      type))
          (let ((operands (cons-combination-operands expr)))
            ; ; (write-line (list "debug" (cadr operands) ";" type))
            (append 
              ;; added
              ;; 0. this is better with 4.16.
              ; (constrain (list-type-base type) (union (texpr-type (car operands)) (list-type-base (cadr type))))
              (list
                (constrain (list-type-base type) (texpr-type (car operands)))
                (constrain (list-type-base (texpr-type (cadr operands))) (texpr-type (car operands))))
              ;; The above 
              ; (constrain (texpr-type (car operands)) (list-type-base (cadr operands)))
              
              (program-constraints (cons-combination-operator expr))
                  (append-map program-constraints
                              operands)
              )
              )
              )
            ))
(define-generic-procedure-handler simplify-annotated-program-1
  (match-args type-expression? cons-combination-expr?)
  (lambda (type expr)
    (make-combination-expr
     (simplify-annotated-program (cons-combination-operator expr))
     (map simplify-annotated-program
          (cons-combination-operands expr)))))

; (define (make-top-level-env-frame)
;   (let ((binary-numerical
;          (let ((v (numeric-type)))
;            (procedure-type (list v v) v)))
;         (binary-comparator
;          (let ((v (numeric-type)))
;            ;; See `(define-parametric-type-operator operator)` -> (lambda operands ...)
;            (procedure-type (list v v) (boolean-type))))
;         ;; IGNORE: added
;         ;; Since T in List{T} can be anything, not instantiate here with one fixed type-variable.
;         ; (unary-list-accessor
;         ;   (let* ((v ))
;         ;     ())
;         ;   )
;         )
;     (list (cons '+ binary-numerical)
;           (cons '- binary-numerical)
;           (cons '* binary-numerical)
;           (cons '/ binary-numerical)
;           (cons '= binary-comparator)
;           (cons '< binary-comparator)
;           (cons '> binary-comparator)
;           (cons '<= binary-comparator)
;           (cons '>= binary-comparator))))

(define map-test
  '(begin
    (define unary-map
      (lambda (proc items)
        (if (null? items)
          ()
          (cons (proc (car items))
                (unary-map proc (cdr items))))))
    ;; + is binary, so not use it here.
    ; (unary-map + (1 2 3))
    (unary-map (lambda (x) (* x x)) (1 2 3))
    )
  )
; (trace unify:dispatch)

;; I just checked simplify-annotated-program which is correct.
(pp (noisy-infer-program-types map-test))
; (begin
;  (define unary-map
;    (lambda (proc items)
;      (declare-type proc (type:procedure ((numeric-type)) (numeric-type)))
;      (declare-type items (type:list (numeric-type)))
;      (if (null? items)
;          ()
;          (cons (proc (car items)) (unary-map proc (cdr items))))))
;  (declare-type unary-map (type:procedure ((type:procedure ((numeric-type)) (numeric-type)) (type:list (numeric-type))) (type:list (numeric-type))))
;  (unary-map (lambda (x) (declare-type x (numeric-type)) (* x x)) (1 2 3)))

;; "debug dict error" in unify.scm
; (newline-pp '(dict (type:8 (numeric-type) ?) (cdr:4 (type:procedure ((? type:5)) (? type:5)) ?) (type:6 (? type:11) ?) (items:3 (? type:5) ?) (car:7 (type:procedure ((? type:5)) (numeric-type)) ?) (proc:2 (type:procedure ((numeric-type)) (? type:9)) ?) (cons:10 (type:procedure ((? type:9) (type:list (? type:11))) (type:list (? type:11))) ?) (quote:12 (type:procedure ((type:list (numeric-type))) (type:list (? type:11))) ?) (null?:14 (type:procedure ((? type:5)) (boolean-type)) ?) (type:17 (type:list (? type:11)) ?) (type:13 (type:list (? type:11)) ?) (type:15 (boolean-type) ?) (type:16 (type:list (? type:11)) ?) (unary-map:1 (type:procedure ((type:procedure ((numeric-type)) (? type:9)) (? type:5)) (type:list (? type:11))) ?) (type:18 (? type:19) ?)))

(define map-test-with-generic-type
  '(define unary-map
      (lambda (proc items)
        (if (null? items)
          ; '() ; not add one extra quote unexpectedly.
          ()
          (cons (proc (car items))
                (unary-map proc (cdr items))))))
  )
(pp (noisy-infer-program-types map-test-with-generic-type))

;; consider the above TODO
(define map-test-with-generic-type-annotated
  '(t
      (? unary-map:25)
      (define unary-map
        (t
          (type:procedure ((? proc:26) (type:list (? items:29))) (? type:43))
          (lambda (proc items)
            (t
            (? type:42)
            (if (t (? type:41) ((t (? null?:40) null?) (t (type:list (? items:29)) items)))
                (t (type:list (? |():39|)) ())
                (t
                  (type:list (? type:37))
                  (cons (t (? cons:36) cons)
                        (t (? type:35) ((t (? proc:26) proc) (t (? type:34) ((t (? car:33) car) (t (type:list (? items:29)) items)))))
                        (t (type:list (? type:32)) ((t (? unary-map:25) unary-map) (t (? proc:26) proc) (t (? type:31) ((t (? cdr:30) cdr) (t (type:list (? items:29)) items)))))))))))))
  )
(define map-test-with-generic-type-dict
  '((= (? unary-map:25) (type:procedure ((? proc:26) (type:list (? items:29))) (? type:43)))
    (= (? type:43) (? type:42))
    (= (boolean-type) (? type:41))
    (= (? type:42) (type:list (? |():39|)))
    (= (? type:42) (type:list (? type:37)))
    (= (? null?:40) (type:procedure ((type:list (? items:29))) (? type:41)))
    (= (? cons:36) (type:procedure ((? type:35) (type:list (? type:32))) (type:list (? type:37))))
    (= (? type:37) (? type:35))
    (= (? type:32) (? type:35))
    (= (? proc:26) (type:procedure ((? type:34)) (? type:35)))
    (= (? car:33) (type:procedure ((type:list (? items:29))) (? type:34)))
    (= (? unary-map:25) (type:procedure ((? proc:26) (? type:31)) (type:list (? type:32))))
    (= (? cdr:30) (type:procedure ((type:list (? items:29))) (? type:31))))
  )
(define map-test-with-generic-type-dict-with-car-cdr-constraints
  (append
    map-test-with-generic-type-dict
    '(
      ;; (t (? type:34) ((t (? car:33) car) (t (type:list (? items:29)) items)))
      (= (? type:34) (? items:29))
      ;; (t (? type:31) ((t (? cdr:30) cdr) (t (type:list (? items:29)) items)))
      (= (? type:31) (type:list (? items:29)))
      )
    )
  )
(define (noisy-infer-annotate-program-types-with-constraints annotate-program constraints)
  (let ((dict (unify-constraints constraints)))
    (if dict
        (let ((res ((match:dict-substitution dict) annotate-program)))
          (newline)
          ; (write-line '---)
          (pp (simplify-annotated-program res))
          ; (write-line '---)
          res
          )
        '***type-error***)))
(noisy-infer-annotate-program-types-with-constraints 
  map-test-with-generic-type-annotated
  map-test-with-generic-type-dict-with-car-cdr-constraints)
;; Here what type proc returns can't be known.
; (begin
;  (define unary-map
;    (lambda (proc items)
;      (declare-type proc (type:procedure ((? type:34)) (? |():39|)))
;      (declare-type items (type:list (? type:34)))
;      (if (null? items)
;          ()
;          (cons (proc (car items)) (unary-map proc (cdr items))))))
;  (declare-type unary-map (type:procedure ((type:procedure ((? type:34)) (? |():39|)) (type:list (? type:34))) (type:list (? |():39|)))))
