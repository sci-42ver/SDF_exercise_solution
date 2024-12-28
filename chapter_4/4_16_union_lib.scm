(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "common_lib.scm")
(define (union-term? term)
  (and
    (tagged-list? term '?:union)
    (n:> (length term) 1)
    ))
(define union-types cdr)
;; https://stackoverflow.com/a/8382387/21294350
(define (dedupe e eq-check?)
  (if (null? e) '()
    (cons (car e) (dedupe (filter (lambda (x) (not (eq-check? x (car e)))) 
                                  (cdr e)) eq-check?))))

(define (union-term . bases)
  (cons '?:union 
        (dedupe
          (append-map 
            (lambda (base) 
              (if (union-term? base)
                (union-types base)
                (list base))
              ) 
            bases)
          eq?
          ))
  )

(define (unify:left-with-union-term union-term-first terms2)
  (let ((first1 (car union-term-first)) (rest1 (cdr union-term-first))
                                        (first2 (car terms2)) (rest2 (cdr terms2)))
    (define (unify-constants dict succeed fail)
      (if (member first2 (union-types first1))
        (succeed dict fail rest1 rest2)
        (begin
          (write-line (list "left-with-union-term error for" first1 first2))
          (fail))))
    unify-constants))
;; 0. See SDF_exercises/chapter_4/4_15.scm
;; > generic get-handler will get the latter added procedure if possible.
;; 1. Here match:element-var? related procedures will reduce vars to the corresponding type lists.
;; 2. See SDF_exercises/chapter_4/4_16.scm "assumption about lhs is always at the left..."
;; for why this won't work
(define-generic-procedure-handler unify:gdispatch
                                  (match-args (car-satisfies union-term?)
                                              (car-satisfies list-term?))
                                  unify:left-with-union-term)

;;; procedure definition tag
;; consider lambda definition
(define (add-procedure-definition-tag data)
  (assert (or (match:element-var? data) (type-expression? data) (union-term? data)))
  (cons 'procedure-definition data)
  )
(define (procedure-definition-type? type)
  (tagged-list? type 'procedure-definition)
  )
(define (procedure-definition-union-type? type)
  (and 
    (procedure-definition-type? type)
    (union-term? (cdr type))
    )
  )
(define procedure-definition-type-data cdr)
;; 0. similar to match:element-var? dispatch.
;; 1. IGNORE: TODO add dispatcher for procedure-definition-type? with normal types.
(define-generic-procedure-handler unify:gdispatch
                                  (match-args (car-satisfies procedure-definition-union-type?)
                                              (car-satisfies list-term?))
                                  (lambda (procedure-definition-type list-type) 
                                    (unify:left-with-procedure-definition-term procedure-definition-type list-type)
                                    ))
(define-generic-procedure-handler unify:gdispatch
                                  (match-args (car-satisfies list-term?)
                                              (car-satisfies procedure-definition-union-type?))
                                  (lambda (list-type procedure-definition-type)
                                    ; (write-line (list "call (list-type procedure-definition-type) with" procedure-definition-type))
                                    (unify:left-with-procedure-definition-term procedure-definition-type list-type)
                                    ))
(define (unify:left-with-procedure-definition-term procedure-definition-term-first terms2)
  (let ((first1 (car procedure-definition-term-first)) (rest1 (cdr procedure-definition-term-first))
                                                       (first2 (car terms2)) (rest2 (cdr terms2)))
    (define (unify-constants dict succeed fail)
      ;; only this line is different from unify:left-with-union-term
      (let ((possible-union-types (union-types (procedure-definition-type-data first1))))
        ;; need member to recognize list equal.
        (if (member first2 possible-union-types)
          (succeed dict fail rest1 rest2)
          (begin
            (write-line (list "left-with-procedure-definition-term error for" possible-union-types first2))
            (fail)))
        ))
    unify-constants))

;; no use to reset type-expression? in match-args
;; As SICP says, these preds in match-args may be shared by others (i.e. pred in the global envs here) or not.
(set! type-expression?
  (lambda (object) 
    (or (type-variable? object)
        (primitive-type? object)
        (parametric-type? object)
        ;; added
        (procedure-definition-type? object)
        (union-term? object)
        )
    )
  )

;; not propagate procedure-definition tag.
(define (remove-procedure-definition-tag data)
  (if (procedure-definition-type? data)
    (procedure-definition-type-data data)
    data)
  )
;; Always propagate to differentiate definition from others.
; (define (maybe-substitute var-first terms)
;   (define (unify-substitute dict succeed fail)
;     (let ((var (car var-first)) (rest1 (cdr var-first))
;           ;; remove-procedure-definition-tag is done here since it will always be added when procedure-definition
;           ;; It won't be buried in var.
;           ;; TODO this needs more fine-grained control. We need propagation for if but not for others like (begin (+ 2 3) (+ "1" "2")) etc....
;           (term (remove-procedure-definition-tag (car terms))) (rest2 (cdr terms)))
;       (cond ((and (match:element-var? term)
;                   (match:vars-equal? var term))
;              ;; similar to the following action for match:vars-equal? in do-substitute.
;              (succeed dict fail rest1 rest2))
;             ((match:has-binding? var dict)
;              ;; will check consistency later.
;              ((unify:dispatch (cons (match:get-value var dict) rest1)
;                               ;; no need to change term here, since var won't be bound to procedure-definition.
;                               ;; procedure-definition is needed
;                               terms)
;               dict succeed fail))
;             (else
;              (let ((dict* (do-substitute var term dict)))
;                (if dict*
;                    (succeed dict* fail rest1 rest2)
;                    (begin
;                     (write-line (list "error for do-substitute" var term))
;                     (fail))))))))
;   unify-substitute)

;;; lambda
(define-generic-procedure-handler annotate-expr
                                  (match-args lambda-expr? any-object?)
                                  (lambda (expr env)
                                    (let ((env* (new-frame (lambda-bvl expr) env)))
                                      (make-texpr (procedure-type (map (lambda (name)
                                                                         ;; modified
                                                                         ;; Here env bindings doesn't contain procedure-definition-tag, so that won't be propagated.
                                                                         (add-procedure-definition-tag (get-var-type name env*)))
                                                                       (lambda-bvl expr))
                                                                  (type-variable))
                                                  (make-lambda-expr (lambda-bvl expr)
                                                                    (annotate-expr (lambda-body expr) env*))))))
(define-generic-procedure-handler program-constraints-1
                                  (match-args type-expression? if-expr?)
                                  (lambda (type expr)
                                    (append
                                      (list (constrain (boolean-type)
                                                       (texpr-type (if-predicate expr)))
                                            ;; modified
                                            ;; Always use procedure-definition-tag when this type can be decided.
                                            (constrain type
                                                       (add-procedure-definition-tag (union-term (texpr-type (if-consequent expr)) (texpr-type (if-alternative expr)))))
                                            )
                                      (program-constraints (if-predicate expr))
                                      (program-constraints (if-consequent expr))
                                      (program-constraints (if-alternative expr)))))

;; Here I only give some procedure-definition compound types but incomplete as one demo.
;; This is one routine work to give one complete implementation for all procedure-definition compound types
(define (procedure-definition-element-var-type? type)
  (and 
    (procedure-definition-type? type)
    (match:element-var? (cdr type))
    )
  )
(define (procedure-definition-type-first-remove-tag data)
  (let ((first (car data)))
    (cons (remove-procedure-definition-tag first) (cdr data))
    )
  )
(define-generic-procedure-handler unify:gdispatch
                                  (match-args (car-satisfies procedure-definition-element-var-type?)
                                              (car-satisfies list-term?))
                                  (lambda (procedure-definition-type-first list-type) 
                                    (write-line (list "call procedure-definition-element-var-type? (pl) with" procedure-definition-type-first list-type))
                                    (maybe-substitute (procedure-definition-type-first-remove-tag procedure-definition-type-first) list-type)
                                    ))
(define-generic-procedure-handler unify:gdispatch
                                  (match-args (car-satisfies list-term?)
                                              (car-satisfies procedure-definition-element-var-type?))
                                  (lambda (list-type procedure-definition-type-first)
                                    (write-line (list "call procedure-definition-element-var-type? (lp) with" procedure-definition-type-first list-type))
                                    (maybe-substitute (procedure-definition-type-first-remove-tag procedure-definition-type-first) list-type)
                                    ))

;; procedure-definition needs be kept when unify-constraints to make var can get that tag if necessary.
(define (match:map-vars get-value pattern)
  (let loop ((pattern pattern))
    (cond ((match:element-var? pattern)
           (get-value pattern (lambda () pattern)))
          ;; This is skipped for 4.16 which doesn't consider segment.
          ; ((match:segment-var? pattern)
          ;  (if (get-value pattern (lambda () #f))
          ;      (error "Ill-formed pattern:" pattern))
          ;  ;; (codes not included by the book) SDF_exercises TODO why not use substitution.
          ;  pattern)
          ;; added before general list.
          ((procedure-definition-element-var-type? pattern)
           (let ((var-pattern (remove-procedure-definition-tag pattern)))
             (get-value var-pattern (lambda () var-pattern))
             )
           )
          ((list? pattern)
           (append-map (lambda (sub)
                         (if (match:segment-var? sub)
                           (get-value sub
                                      (lambda () (list sub)))
                           (list (loop sub))))
                       pattern))
          (else pattern))))
