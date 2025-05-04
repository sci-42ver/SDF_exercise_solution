;;;;;; Tag strings
(define LEFT-IF-TYPE-STR "left-if")
(define LEFT-PAREN-TYPE-STR "call")
(define NULL-PAREN-TYPE-STR "paren")
(define :=-TYPE-STR "define")
(define EXPR-LIST-TYPE-STR "expr-list")
(define STATEMENT-BLOCK-TYPE-STR "statement-block")
(define IF-STATEMENT-TYPE-STR "null-if")
;; see 5_7_regexp_lib_simplified_based_on_effbot_based_on_irregex.scm
;; Here "get" for x[idx] in LeftIndex is skipped.
(define VAR-TYPES (list ID-TAG-STR))
(define TUPLE-TYPES `(,@VAR-TYPES "tuple"))

;; with-expr-as-data-end means these things all have grammar ending with expression.
(define NON-EXPR-TAG-WITH-EXPR-AS-DATA-END
  `(,STATEMENT-BLOCK-TYPE-STR
    ,IF-STATEMENT-TYPE-STR
    ,EXPR-LIST-TYPE-STR
    ,:=-TYPE-STR
    ))

;; 0. implicitly consider expr precedence is higher than NON-EXPR-TAG-WITH-EXPR-AS-DATA-END.
;; 1. For left-if, 
;; 1.a. "lambda" & ":=" & "EXPR-LIST" & "LEFT-IF-TYPE-STR" (all the former accepts expression at the end) 
;; & "STATEMENT-BLOCK" & "IF-STATEMENT" (these ends with statement) can't be its left.
;; 1.b. For the consq, ":=" & "EXPR-LIST" & "STATEMENT-BLOCK" can't be here due to their lbp<left-if-rbp.
;; "lambda" & "IF-STATEMENT" are possible due to being nud.
;; "LEFT-IF-TYPE-STR" is possible due to LeftRightAssoc.
(define tag-with-precedence-lower-than-OR
  `("lambda" ,LEFT-IF-TYPE-STR ,@NON-EXPR-TAG-WITH-EXPR-AS-DATA-END))

(define COMPARISON-TYPE-STR "comparison-expr")

(define CONSTANT-TYPE-STR `(,ID-TAG-STR ,NUMBER-TAG-STR ,STAR-ARG-TAG-STR))

(define ATTRIBUTEREF-TYPE-STR ".")

(define QUOTE-TYPE-STR "'")
(define ATOM-TYPE-LIST 
  `(,@CONSTANT-TYPE-STR 
    ,NULL-PAREN-TYPE-STR
    ;; list_display | dict_display | set_display are all skipped in ParsePythonDemo.scm.
    ;; generator_expression & yield are also skipped.

    ;; from Scheme
    ,QUOTE-TYPE-STR
    ))

(define PRIMARY-TYPE-LIST
  `(,@ATOM-TYPE-LIST
    ,ATTRIBUTEREF-TYPE-STR
    ;; subscription | slicing are skipped. (see PrsSubscription and PrsSlicing)
    ,LEFT-PAREN-TYPE-STR
    )
  )
