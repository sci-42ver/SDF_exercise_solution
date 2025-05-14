(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
; (load "SentinelBaseDataLib.scm")
;; 0. For or etc,
;; left can be or_test
;; rhs won't be or-expr due to prec relation, i.e. self-rbp=self-lbp instead of rbp<lbp.
(define (get-expr-token-types-with-consistent-prec token)
  (assert (Token? token))
  (let ((cur-prec (get-prec token)))
    (append
      (filter
        (lambda (type)
          (>= (get-prec-by-token-type type) cur-prec)
          )
        ALL-NON-TOP-EXPR-TOKEN-TYPES
        )
      ;; needed because they are atom or primary https://docs.python.org/3/reference/expressions.html.
      `(,LEFT-PAREN-TYPE-STR ,NULL-PAREN-TYPE-STR "{"
                             ; ,@CONSTANT-TYPE-STR ; just atom in Python grammar.
                             ;; Here this proc is only used by pred-ensuring-expr-with-consistent-precedence
                             ;; which is only used by op with precedence lower than primary.
                             ;; So it is fine to put all primary types here.
                             ,@PRIMARY-TYPE-LIST
                             )
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
  (let ((type (*token-type-list* 'get op-str (get-caller-type caller))))
    ;; only allowed to use in appropriate places.
    (assert type)
    type
    )
  )
(define (get-token-type-from-caller-and-op caller token)
  (%get-token-type-from-caller-and-op caller (Token-val-str token))
  )
