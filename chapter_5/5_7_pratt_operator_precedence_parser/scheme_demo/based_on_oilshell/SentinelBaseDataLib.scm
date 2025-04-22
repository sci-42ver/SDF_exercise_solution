(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "SentinelConstantTypeStrLib.scm")
;;; Initialization
(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "bidirectional_map.scm")
(define *token-type-list* (make-bidirectional-map make-equal-hash-table))
; (define (make-token-type-key caller op-str)
;   (assert (and (procedure? caller) (string? op-str)))
;   (list caller op-str)
;   )
(define (init-token-type-list)
  (*token-type-list* 'insert (make-prec-key 'Left ",") EXPR-LIST-TYPE-STR)
  (*token-type-list* 'insert (make-prec-key 'Null "(") NULL-PAREN-TYPE-STR)
  (*token-type-list* 'insert (make-prec-key 'Left "(") LEFT-PAREN-TYPE-STR)
  (*token-type-list* 'insert (make-prec-key 'Left ";") STATEMENT-BLOCK-TYPE-STR)
  (*token-type-list* 'insert (make-prec-key 'Left ":=") :=-TYPE-STR)
  (*token-type-list* 'insert (make-prec-key 'Null "if") IF-STATEMENT-TYPE-STR)
  (*token-type-list* 'insert (make-prec-key 'Left "if") LEFT-IF-TYPE-STR)
  )
(init-token-type-list)
(define ALL-TOKEN-TYPES (map cdr (hash-table->alist *token-type-list*)))
(define ALL-EXPR-TOKEN-TYPES
  (remove 
    (lambda (elm) (member elm NON-EXPR-TAG-WITH-EXPR-AS-DATA-END))
    ALL-TOKEN-TYPES)
  )
;; lambda_expr can't be in any other non-top expression (i.e. conditional_expression etc).
;; Same for conditional_expression.
(define ALL-NON-TOP-EXPR-TOKEN-TYPES
  (remove 
    (lambda (elm) (member elm `("lambda" ,LEFT-IF-TYPE-STR)))
    ALL-EXPR-TOKEN-TYPES)
  )