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
    (spec 'Null UNUSED-PREC-MARKING-END NullError Null-Error-List)
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
    (spec-with-implicit-prec 'Left LeftComma (list ","))
    ;; 0. CLOSE-PAREN is shown above. Here lbp is only needed to be less than all rbp's.
    (spec-with-implicit-prec 'Left LeftFuncCall (list "("))
    (spec-with-implicit-prec 'Null NullParen (list "("))
    ;; 1. IGNORE LEFT-BRACE, SEMICOLON, RIGHT-BRACE are skipped due to they are not used in expression (see https://en.cppreference.com/w/c/language/operator_precedence).
    ;; parse-matchfix-modified have been used similarly in lambda and open-paren-nud manipulation there.
    ;; SEMICOLON, RIGHT-BRACE are just like "," and ")" here both with default error led/nud's.
    ;; 1.a. LEFT-BRACE, SEMICOLON, RIGHT-BRACE should be added since statement may be used in lambda.
    ;; (I won't give one extensive support for Python statement... That is due to focus here is expr)
    (spec-with-implicit-prec 'Null NullBrace (list "{"))
    (spec-with-implicit-prec 'Left LeftSemicolon (list ";"))

    (spec-with-implicit-prec 'Null NullLambda (list "lambda"))
    
    ;; IGNORE For if – else (not offered in oilshell since it is not used in C-expr),
    ;; we should do as https://docs.python.org/3/reference/expressions.html#if-expr
    ;; instead of that in pratt_new_compatible_with_MIT_GNU_Scheme.scm
    (spec-with-implicit-prec 'Null NullIf (list "if"))
    (spec-with-implicit-prec 'LeftRightAssoc LeftIf (list "if"))

    (spec-with-implicit-prec 'Left LeftDefine (list ":="))
    ;;;;;; For pratt_new_compatible_with_MIT_GNU_Scheme.scm
    ;; All op's above "-" have been implemented.
    ;; Then it has "-", "**", "/", "=" (i.e. "==" in Python), "not", "QUOTE-SYMBOL" left.

    ;;;;;; ensure-consistent check
    ;; 0. all binary ops in [or, and, &, ^, |, shift-op, a_expr-op, m_expr-op]
    ;; has the pattern: self-expr = self-expr-one-level-up | self-expr self-op self-expr-one-level-up
    ;; so ensure-consistent is fine where rhs can be also self-expr if we group from right to left instaed of with the reverse direction.
    ;; 1. all unary-prefix ops in [not, u_expr-op]
    ;; has pattern: self-expr = self-expr-one-level-up | self-op self-expr
    ;; ensure-consistent is still fine since "self-op self-expr" can be only higher "self-op self-expr-one-level-up".
    ;; the nested self-op is allowed by nud always.
    ;; 2. binary ops in [comparison, ]
    ;; has pattern: self-expr = self-expr-one-level-up (self-op self-expr-one-level-up)*
    ;; This implicitly allows self-expr = self-expr (self-op self-expr)*
    ;; So ensure-consistent is still fine
    ;; 3. binary ops in [power, ]
    ;; has pattern: self-expr = (self-expr-one-level-up | self-expr-two-levels-up) [self-op self-expr-one-level-down]
    ;; So ensure-consistent won't work.
    ;; > The power operator binds more tightly than unary operators on its left; it binds less tightly than unary operators on its right.
    ;; 3.a. Here for **, 
    ;; "u_expr ::= power" implies we can have "** power", i.e. rhs's prec >= self-prec plus u_expr-op.
    ;; 3.b. Due to right to left, power ** rest can't happern since lhs must be grouped later than rhs.
    ;; so lhs's prec > self-prec which is already implied by LeftRightAssoc.
    ;;;;;; effbot implementation check
    ;; 0. Just see "The Python Expression Grammar" part.
    ;; infix_r & infix has no seq.
    ;; prefix has no Sentinel.
    ;; 1. IMHO "infix("not", 60)" is inappropriate since it allows a not b.
    ;;;; BEHAVIOR
    ;; See LeftLogical for or&and.
    ;; For not (i.e. ! in oilshell), here we add Sentinel.
    ;;;; TODO tests see LeftLogical
    (spec-with-implicit-prec 'Left LeftLogical (list "or"))
    (spec-with-implicit-prec 'Left LeftLogical (list "and"))
    (spec-with-implicit-prec 'Null NullPrefixOpWithSentinel (list "not"))

    (spec-with-implicit-prec 'Left PrsComparison COMPARISON-OP-LST)

    (spec-with-implicit-prec 'Left LeftBitwise (list "&"))
    (spec-with-implicit-prec 'Left LeftBitwise (list "^"))
    (spec-with-implicit-prec 'Left LeftBitwise (list "|"))

    ;;;; BEHAVIOR
    ;; > shift_expr ::= a_expr | shift_expr ("<<" | ">>") a_expr
    ;; 0. Seq is implied similar to LeftLogical
    ;; 1. a_expr implies we can still use ensure-consistent Sentinel.
    ;; 2. not in pratt_new_compatible_with_MIT_GNU_Scheme.scm
    ;; oilshell uses LeftBinaryOp (so comparison is similar to LeftLogical)
    ;;;; TODO tests
    ;; a & b << c >> d => (& a (>> (<< b c) d))
    (spec-with-implicit-prec 'Left PrsSeqWithSentinel SHIFT-OP-LST)

    ;;;; BEHAVIOR
    ;; 0. similar to the above except pratt_new_compatible_with_MIT_GNU_Scheme.scm implements this with parse-nary.
    ;; So see LeftLogical for comparison.
    ;; 0.a. Here I allow (/ a b c) which means (/ a (* b c)) same as Scheme.
    ;;;; TODO tests
    ;; a * d + b << c => (<< (+ (* a d) b) c)
    (spec-with-implicit-prec 'Left PrsSeqWithSentinel BINARY-PM-OP-LST)
    (spec-with-implicit-prec 'Left PrsSeqWithSentinel OTHER-BINARY-OP-LST)

    ;;;; BEHAVIOR
    ;; Comparison see %NullPrefixOp.
    ;;;; TODO tests
    ;; +-a*b => (* (+ (- a)) b)
    (spec-with-implicit-prec 'Null NullPrefixOpWithSentinel UNARY-OP-LST)

    (spec-with-implicit-prec 'LeftRightAssoc PrsPower '("**"))

    ;;;; BEHAVIOR
    ;; trivially not in pratt_new_compatible_with_MIT_GNU_Scheme.scm and oilshell shell parser
    ;; Also not in the old Python effbot parser implementation
    ;;;; TODO tests
    ;; TODO for primary
    ;; await await a => error
    (spec-with-implicit-prec 'Null PrsAwait AWAIT-OP-LST)
    ;;;;;; ensure-consistent check finish
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
