;;; Initialization
(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "bidirectional_map.scm")
;; This *token-type-list* is based on context, so not same as that got from tokenizer.
(define *token-type-list* (make-multi-bidirectional-map default-hash-table-constructor))
; (define (make-token-type-key caller op-str)
;   (assert (and (procedure? caller) (string? op-str)))
;   (list caller op-str)
;   )
(define (init-token-type-list)
  (*token-type-list* 'insert EXPR-LIST-TYPE-STR "," 'Left)
  (*token-type-list* 'insert NULL-PAREN-TYPE-STR "(" 'Null)
  (*token-type-list* 'insert LEFT-PAREN-TYPE-STR "(" 'Left)
  (assert (*token-type-list* 'get "(" 'Left))
  ; (bkpt 'init-token-type-list *token-type-list*)
  (*token-type-list* 'insert STATEMENT-BLOCK-TYPE-STR ";" 'Left)
  (*token-type-list* 'insert :=-TYPE-STR ":=" 'Left)
  (*token-type-list* 'insert IF-STATEMENT-TYPE-STR "if" 'Null)
  (*token-type-list* 'insert LEFT-IF-TYPE-STR "if" 'LeftRightAssoc)
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
    (lset-intersection equal? BINARY-PM-OP-LST UNARY-OP-LST)
    )
  (for-each
    (lambda (op)
      ;; Here we have '("WARNING" "comparison-expr" "will have more than one keys to map. Here we just reset to keys" ("not in" left))' etc.
      ;; That is fine since 'getKeys is only used at last by get-prec and all these have the same prec.
      ; (*token-type-list* 'insert COMPARISON-TYPE-STR op 'Left)
      ;; The above one is not used since "get-prec" is used for the original type.
      ;; Although we can set! COMPARISON-TYPE-STR for that token, but that may influence the rest APIs.
      (*token-type-list* 'insert op op 'Left)
      )
    COMPARISON-OP-LST
    )
  (for-each
    (lambda (op)
      (*token-type-list* 'insert op op 'Left)
      )
    (append SHIFT-OP-LST OTHER-BINARY-OP-LST
      (list "or" "and")
      (list "|" "^" "&")
      )
    )
  (for-each
    (lambda (op)
      (*token-type-list* 'insert op op 'Null)
      )
    (cons "not" AWAIT-OP-LST)
    )
  (for-each
    (lambda (op)
      (*token-type-list* 'insert op op 'LeftRightAssoc)
      )
    '("**")
    )
  )
(define (subtype type)
  (assert (string? type))
  (cond 
    ((any (lambda (type*) (equal? type type*)) '("left-plus" "null-plus"))
      "+")
    ((any (lambda (type*) (equal? type type*)) '("left-minus" "null-minus"))
      "-")
    (else
      (write-line (list "no available subtype for" type))
      type
      ))
  )
(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "lib/sentinel/SentinelConstantTypeStrLib.scm")
(init-token-type-list)
(define ALL-TOKEN-TYPES
  (get-val-lst-for-multi-hash-table (*token-type-list* 'get-keys-val-table))
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