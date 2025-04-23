;;;;;; Tag strings
(define LEFT-IF-TYPE-STR "left-if")
(define LEFT-PAREN-TYPE-STR "call")
(define NULL-PAREN-TYPE-STR "paren")
(define :=-TYPE-STR "define")
(define EXPR-LIST-TYPE-STR "expr-list")
(define STATEMENT-BLOCK-TYPE-STR "statement-block")
(define IF-STATEMENT-TYPE-STR "null-if")
;; see 5_7_regexp_lib_simplified_based_on_effbot_based_on_irregex.scm
(define VAR-TYPES (list ID-TAG-STR "get"))
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
