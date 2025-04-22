(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "SentinelBaseDataLib.scm")
;; token-type -> prec, then order it.
;; token-type is set by 
(define (get-expr-token-types-with-consistent-prec token-type)
  (assert (Token-type? token-type))
  (let ((cur-prec (get-prec token-type)))
    (append
      (filter
        (lambda (type)
          (>= (get-prec type) cur-prec)
          )
        ALL-NON-TOP-EXPR-TOKEN-TYPES
        )
      ;; needed because they are atom or primary https://docs.python.org/3/reference/expressions.html.
      `(,LEFT-PAREN-TYPE-STR ,NULL-PAREN-TYPE-STR "{")
      )
    )
  )

(define (get-caller-type caller)
  (assert (procedure? caller))
  (let ((type (hash-table-ref* *handler-type-list* caller)))
    (and
      (not type)
      (error (list "unrecognized caller" caller))
      )
    type
    )
  )
(define (%get-token-type-from-caller-and-op caller op-str)
  (*token-type-list* 'get (make-prec-key (get-caller-type caller) op-str))
  )
(define (get-token-type-from-caller-and-op caller token)
  (%get-token-type-from-caller-and-op caller (Token-val token))
  )