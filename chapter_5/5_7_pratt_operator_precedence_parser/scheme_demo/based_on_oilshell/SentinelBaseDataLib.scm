(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "SentinelConstantTypeStrLib.scm")
;;; Initialization
(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "bidirectional_map.scm")
(define *token-type-list* (make-multi-bidirectional-map default-hash-table-constructor))
; (define (make-token-type-key caller op-str)
;   (assert (and (procedure? caller) (string? op-str)))
;   (list caller op-str)
;   )
(define (init-token-type-list)
  (*token-type-list* 'insert EXPR-LIST-TYPE-STR "," 'Left)
  (*token-type-list* 'insert NULL-PAREN-TYPE-STR "(" 'Null)
  (*token-type-list* 'insert LEFT-PAREN-TYPE-STR "(" 'Left)
  (*token-type-list* 'insert STATEMENT-BLOCK-TYPE-STR ";" 'Left)
  (*token-type-list* 'insert :=-TYPE-STR ":=" 'Left)
  (*token-type-list* 'insert IF-STATEMENT-TYPE-STR "if" 'Null)
  (*token-type-list* 'insert LEFT-IF-TYPE-STR "if" 'Left)
  ;; The rest ones in prec-list-higher-than-or-op doesn't have both nud and led.
  (for-each
    (lambda (op)
      (let ((op-description
              (cond 
                ((equal? "-" op) "minus")
                ((equal? "+" op) "plus")
                (else (error (list "unknown op-description for" op "with both led and nud")))
                )
              ))
        (*token-type-list* 'insert (string-append "left-" op-description) op 'Left)
        (*token-type-list* 'insert (string-append "null-" op-description) op 'Null)  
        )
      )
    (lset-intersection BINARY-PM-OP-LST UNARY-OP-LST)
    )
  (*token-type-list* 'insert COMPARISON-TYPE-STR "if" 'Left)
  )
(init-token-type-list)
(define ALL-TOKEN-TYPES
  (get-val-lst-for-multi-hash-table *token-type-list*)
  )
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