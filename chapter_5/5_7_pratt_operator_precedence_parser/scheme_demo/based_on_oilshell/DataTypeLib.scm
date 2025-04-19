;;; used by both Parser.scm and ParserSpec.scm.
;; Different from Python, MIT/GNU Scheme doesn't have one way to only offer bp val. But Racket can.
;; see SDF_exercises/chapter_5/5_7_naive_algorithm_for_operator_precedence_parser/5_7_precedence_lib.scm
(define (NullInfo #!optional nud bp)
  ;; Different from pratt_new_compatible_with_MIT_GNU_Scheme.scm
  ;; Here it defaults to be minimal bp. Anyway we should not rely the default value but always explicit set one expected bp.
  (list (or* nud NullError) (or* bp 0))
  )
(define (LeftInfo #!optional led lbp rbp)
  (list (or* led LeftError) (or* lbp 0) (or* rbp 0))
  )
(define get-nud car)
(define get-null-bp cadr)
(define get-led car)
(define get-left-lbp cadr)
(define get-left-rbp caddr)

;;; used by SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell/Parser.scm
(define (Token type val #!optional loc)
  (declare (ignore loc))
  (list type val)
  )
(define Token-type car)
(define Token-val cadr)
(define Token-type=? string=?)

(define (Loc row col) (list row col))

;; borrow from oilshell (IMHO better than pratt_new_compatible_with_MIT_GNU_Scheme.scm which has no regular number pattern relation)
(define PREC-STEP 6)

;;;;;; BEGIN PREC DEFINITION
;;; Prec list:
;; Here 1 means PREC-STEP and so on.
;; lbp: 0#N( N{ N_if :=#<1#L; lambda#<2#,#<3#L_if#
;; rbp (=lbp or lbp-1)
(define NULL-PAREN-PREC 0)
;; similarly only stop on } and allow any expr/stmt inside.
(define NULL-BRACE-PREC NULL-PAREN-PREC)
;; different from pratt_new_compatible_with_MIT_GNU_Scheme.scm
;; Here we allow "if a,b: ..." (this isn't allowed in Python due to assignment_expression can't be list https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-if_stmt) 
;; so its prec should be less than COMMA-PREC.
;; IMHO statement prec should be less than all expr prec, see https://docs.python.org/3/reference/simple_stmts.html#grammar-token-python-grammar-expression_stmt
;; here I assume RHS of expression_stmt just allow *any* expr. Also see https://stackoverflow.com/questions/79544489/level-2-expression-in-c#comment140282231_79544622 https://stackoverflow.com/a/63677576/21294350
(define NULL-IF-PREC NULL-PAREN-PREC)

;; 0. It should not bind anything left from other op's because it is just one token to end statement.
;; 1. "> NULL-BRACE-PREC" is not needed here since we explicitly assert ";" delimeter inside NullBrace.
(define LEFT-SEMICOLON-PREC NULL-PAREN-PREC)

;;; IGNORE "," is not listed in Python precedence list.
;; Here I just assume , has one higher precedence than :=.
;;; See DenotationLib.scm comment for LeftComma, here comma should not be manipulated as one normal op.
(define :=-PREC 0)

(define LAMBDA-RBP (+ PREC-STEP :=-PREC)) ; < COMMA-PREC
;; 0. IGNORE Same value as pratt_new_compatible_with_MIT_GNU_Scheme.scm
;; to make extension more flexible.
;; 0.a. See the above "See DenotationLib.scm ..."
;; 1. Naming convention follows oilshell Python implementation https://peps.python.org/pep-0008/#constants.
(define COMMA-PREC (+ PREC-STEP LAMBDA-RBP))

;; should be greater than COMMA-PREC for Python since conditional_expression is one expr.
;; Although lambda_expr is also one expr, it is nud. LAMBDA-RBP< COMMA-PREC is to reuse LeftComma just like NullParen.
(define LEFT-IF-PREC (+ PREC-STEP COMMA-PREC))
;;;;;; END PREC DEFINITION

(define Null-Error-List (list ")" "]" ":" "eof" ";" "}"))

(define comma-token (Token "," ","))
(define semicolon-token (Token ";" ";"))
(define right-paren-token (Token ")" ")"))

;; always return one list
(define (get-possible-tuple-contents possible-tuple)
  (cond
    ((tuple? possible-tuple) (cdr possible-tuple))
    (else (list possible-tuple)))
  )

;; see SDF_exercises/chapter_5/5_7_re_lib/5_7_regexp_lib_simplified_based_on_effbot_based_on_irregex.scm
(define var-types (list "id" "get"))
