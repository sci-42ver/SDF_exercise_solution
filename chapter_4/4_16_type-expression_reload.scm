;; Here only add some for SDF_exercises/chapter_4/4_16.scm for demo.
;; This is incomplete.
(define-generic-procedure-handler simplify-annotated-program-1
                                  (match-args type-expression?
                                              (disjoin boolean? number? symbol?))
                                  (lambda (type expr)
                                    expr))
(define-generic-procedure-handler simplify-annotated-program-1
                                  (match-args type-expression? begin-expr?)
                                  (lambda (type expr)
                                    (make-begin-expr
                                      (append-map (lambda (x)
                                                    (splice-begin (simplify-annotated-program x)))
                                                  (begin-exprs expr)))))
(define-generic-procedure-handler simplify-annotated-program-1
                                  (match-args type-expression? combination-expr?)
                                  (lambda (type expr)
                                    (make-combination-expr
                                      (simplify-annotated-program (combination-operator expr))
                                      (map simplify-annotated-program
                                           (combination-operands expr)))))
(define-generic-procedure-handler simplify-annotated-program-1
                                  (match-args type-expression? if-expr?)
                                  (lambda (type expr)
                                    (make-if-expr
                                      (simplify-annotated-program (if-predicate expr))
                                      (simplify-annotated-program (if-consequent expr))
                                      (simplify-annotated-program (if-alternative expr)))))
