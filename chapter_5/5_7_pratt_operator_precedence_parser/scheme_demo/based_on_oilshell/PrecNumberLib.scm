;; borrow from oilshell (IMHO better than pratt_new_compatible_with_MIT_GNU_Scheme.scm which has no regular number pattern relation)
(define PREC-STEP 6)
(define RIGHT-ASSOC-MINUS 1)
(assert (> PREC-STEP RIGHT-ASSOC-MINUS))

;;; Prec list:
;; Here 1 means PREC-STEP and so on.
;; lbp: 0#N( N{ N_if :=#<1#L; lambda#<2#,#<3#L_if#
;; rbp (=lbp or lbp-1)
;;; IGNORE Here I define one grammar precedence order list from high to low:
(define UNUSED-PREC-MARKING-END -1)
(define UNUSED-BASE-PREC 0)
(define BASE-PREC 0) ; init rbp for parsing

;; similarly only stop on } and allow any expr/stmt inside.
(define NULL-BRACE-PREC UNUSED-BASE-PREC)

;;; IMHO statement prec should be less than all expr prec, see https://docs.python.org/3/reference/simple_stmts.html#grammar-token-python-grammar-expression_stmt
;; here I assume RHS of expression_stmt just allow *any* expr. Also see https://stackoverflow.com/questions/79544489/level-2-expression-in-c#comment140282231_79544622 https://stackoverflow.com/a/63677576/21294350
;; 0. It should not bind anything left from other op's because it is just one token to end statement.
;; 1. "> NULL-BRACE-PREC" is not needed here since we explicitly assert ";" delimeter inside NullBrace.
;; 2. Here we should not allow "if a;b then ..." because that makes ambiguity
;; since ; means end of one statement so that if-statement is ended.
(define LEFT-SEMICOLON-PREC BASE-PREC)
(define EXPR-BASE-PREC LEFT-SEMICOLON-PREC)
;; https://docs.python.org/3/reference/simple_stmts.html#expression-statements
;; IMHO expr can be one statement so that they can share base-prec.
(define STATEMENT-BASE-PREC EXPR-BASE-PREC)
;;; IGNORE different from pratt_new_compatible_with_MIT_GNU_Scheme.scm
;; Here we allow "if a,b then ..." (this isn't allowed in Python due to assignment_expression can't be list https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-if_stmt) 
;; so its prec should be less than COMMA-PREC.
;;; Python
;; > if_stmt ::= "if" assignment_expression ":" suite
(define NULL-IF-PREC STATEMENT-BASE-PREC)
;; 0. IGNORE Same value as pratt_new_compatible_with_MIT_GNU_Scheme.scm
;; to make extension more flexible.
;; 0.a. See the above "See DenotationLib.scm ..."
;; 1. Naming convention follows oilshell Python implementation https://peps.python.org/pep-0008/#constants.
;; 2. IGNORE Here we allow a:=b,c => (define a (tuple b c))
;; For Python a:=expr, so a:=b,c is one error.
;; But here we allows that since := is define instead of "a named expression".
;; So the above means (tuple (define a b) c)
(define COMMA-PREC EXPR-BASE-PREC)
(define LEFT-COLON-PREC STATEMENT-BASE-PREC)
; (define Null-MUL-PREC EXPR-BASE-PREC)

;;;;;; PYTHON EXPR BEGINNING
;;;; see https://docs.python.org/3/reference/expressions.html#operator-precedence
;;; IGNORE "," is not listed in Python precedence list.
;; Here I just assume , has one higher precedence than :=.
;;; IGNORE See DenotationLib.scm comment for LeftComma, here comma should not be manipulated as one normal op.
;;; := should grab b in "a;b := 2".
(define :=-PREC (+ PREC-STEP EXPR-BASE-PREC))

;; 0. IGNORE IMHO lambda should not bind anything at the right because it just manipulates with expr_list until ":".
;; ~~so~~ lexer list (lambda a := b : ...) will throw error(s).
;; That is checked by ensure-identifier.
;; 1. Here lambda a: b;c won't bind "b;c" statement into body due to prec.
(define LAMBDA-RBP EXPR-BASE-PREC) ; used for body parsing

;; should be greater than COMMA-PREC for Python since conditional_expression is one expr.
;; Although lambda_expr is also one expr, it is nud. LAMBDA-RBP< COMMA-PREC is to reuse LeftComma just like NullParen.
(define LEFT-IF-PREC (+ PREC-STEP (max :=-PREC LAMBDA-RBP)))

(define NULL-PAREN-PREC UNUSED-BASE-PREC)
