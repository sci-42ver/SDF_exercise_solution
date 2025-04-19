(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "5_7_tokenize_lib.scm")
(load "5_7_tokenize_tests.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "DataTypeLib.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/")
(load "5_7_regexp_lib_simplified_based_on_effbot_based_on_irregex.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "ParserSpec.scm")
(load "DenotationLib.scm")

(define (MakePythonParserSpec)
  (let ((spec (ParserSpec)))
    ;;; IGNORE No "--" etc in Python and language L (see SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/README.md).
    ;;; Here just follow nud/led's in pratt_new_compatible_with_MIT_GNU_Scheme.scm

    ;; $
    ;; All related data are manipulated either explicitly here or implicitly with default values.
    (spec 'Null -1 NullError Null-Error-List)
    ;; 0. , is used in pratt_new_compatible_with_MIT_GNU_Scheme.scm
    ;; for lambda, (a1, a2) or proc(a1, a2)
    ;; So we can always construct one list for the parent
    ;; 0.a. If we put "," inside the whole Python operator precedence list,
    ;; then it should be higher than the top "(" to grab a1.
    ;; Then arg1 + arg2 , arg3 will be arg1 + (arg2 , arg3) which is wrong.
    ;; So it should be manipulated specifically.
    ;; 0.b. Why LeftComma is offered in pratt-parsing-demo/arith_parse.py
    ;; is due to C & Shell has that sequence operator, i.e. "a, b" returns b after evaluating a.
    ;; This is not in Python, but the latter offers tuple construction for that as arith_parse.py says.
    ;; ---
    ;; So when used by other nud's like lambda or led's, "," is manipulated specifically.
    ;; This led is just used for the bare case "a, b".
    (spec 'Left COMMA-PREC LeftComma (list ","))
    ;; 0. CLOSE-PAREN is shown above. Here lbp is only needed to be less than all rbp's.
    (spec 'Left 200 LeftFuncCall (list "("))
    (spec 'Null NULL-PAREN-PREC NullParen (list "("))
    ;; 1. IGNORE LEFT-BRACE, SEMICOLON, RIGHT-BRACE are skipped due to they are not used in expression (see https://en.cppreference.com/w/c/language/operator_precedence).
    ;; parse-matchfix-modified have been used similarly in lambda and open-paren-nud manipulation there.
    ;; SEMICOLON, RIGHT-BRACE are just like "," and ")" here both with default error led/nud's.
    ;; 1.a. LEFT-BRACE, SEMICOLON, RIGHT-BRACE should be added since statement may be used in lambda.
    ;; (I won't give one extensive support for Python statement... That is due to focus here is expr)
    (spec 'Null NULL-BRACE-PREC NullBrace (list "{"))
    (spec 'Left LEFT-SEMICOLON-PREC LeftSemicolon (list ";"))
    ;; IGNORE For if – else (not offered in oilshell since it is not used in C-expr),
    ;; we should do as https://docs.python.org/3/reference/expressions.html#if-expr
    ;; instead of that in pratt_new_compatible_with_MIT_GNU_Scheme.scm
    (spec 'Null NULL-IF-PREC NullIf (list "if"))
    (spec 'Left LEFT-IF-PREC LeftIf (list "if"))
    ))
(define (MakeParser str)
  (Parser (MakePythonParserSpec) (Tokenize str))
  )

(cd "~/SICP_SDF/SDF_exercises/common-lib/")
(load "logic_lib.scm")
(define (ParsePythonDemo str #!optional expected)
  ;; Here I won't add one extra class wrapper for simplicity.
  (let ((res ((MakeParser str) 'Parse)))
    ;; assert is not same as Python with 2 args
    (and* expected (assert (equal? res expected)))
    ;; Use write to enable outputting cycle.
    (format #t "~40S ~S~%" str res)
    res
    )
  )
