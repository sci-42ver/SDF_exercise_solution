(define string-type)
(define string-type?)
(receive (constructor predicate) (primitive-type 'string-type)
         (set! string-type constructor)
         (set! string-type? predicate))

;; trivial settings
(define-generic-procedure-handler annotate-expr
                                  (match-args string? any-object?)
                                  (lambda (expr env)
                                    (make-texpr (string-type) expr)))
(define-generic-procedure-handler program-constraints-1
                                  (match-args type-expression?
                                              (disjoin string?))
                                  (lambda (type expr)
                                    '()))
(define-generic-procedure-handler simplify-annotated-program-1
                                  (match-args type-expression?
                                              (disjoin string?))
                                  (lambda (type expr)
                                    expr))
