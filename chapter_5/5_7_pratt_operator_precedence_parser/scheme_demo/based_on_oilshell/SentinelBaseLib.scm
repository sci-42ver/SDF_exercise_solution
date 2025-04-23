(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "SentinelBaseDataLib.scm")
;; 0. For or etc,
;; left can be or_test
;; rhs won't be or-expr due to prec relation, i.e. self-rbp=self-lbp instead of rbp<lbp.
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

;; led-nud-caller means either led or nud.
(define (get-caller-type led-nud-caller)
  (assert (procedure? led-nud-caller))
  (let ((type (hash-table-ref* *handler-type-list* led-nud-caller)))
    (and
      (not type)
      (error (list "unrecognized led-nud-caller" led-nud-caller))
      )
    type
    )
  )
;; All caller-type's are implicitly set in spec-with-implicit-prec called by (MakePythonParserSpec).
(define (%get-token-type-from-caller-and-op caller op-str)
  (*token-type-list* 'get (get-caller-type caller) op-str)
  )
(define (get-token-type-from-caller-and-op caller token)
  (%get-token-type-from-caller-and-op caller (Token-val token))
  )