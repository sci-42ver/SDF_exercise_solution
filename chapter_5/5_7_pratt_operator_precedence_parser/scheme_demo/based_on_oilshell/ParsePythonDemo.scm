(cd "~/SICP_SDF/SDF_exercises/chapter_5")
; (load "5_7_tokenize_lib.scm")
(cd "~/SICP_SDF/SDF_exercises/chapter_5")
; (load "5_7_tokenize_tests.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
; (load "DataTypeLib.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
; (load "ParserSpec.scm")
; (load "DenotationLib.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
; (load "SentinelLib.scm")
(cd "~/SICP_SDF/SDF_exercises/common-lib/")
; (load "application_lib.scm")

(define (MakePythonParserSpec)
  (let ((spec (ParserSpec)))
    ;;; IGNORE No "--" etc in Python and language L (see SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/README.md).
    ;;; Here just follow nud/led's in pratt_new_compatible_with_MIT_GNU_Scheme.scm

    ;; $
    ;; All related data are manipulated either explicitly here or implicitly with default values.
    (spec 'Null UNUSED-BASE-BP NullError Null-Error-List)
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
    (spec-with-implicit-prec spec 'Left LeftComma (list ","))
    ;; 0. CLOSE-PAREN is shown above. Here lbp is only needed to be less than all rbp's.
    (spec-with-implicit-prec spec 'Left LeftFuncCall (list "("))
    (spec-with-implicit-prec spec 'Null NullParen (list "("))
    ;; 1. IGNORE LEFT-BRACE, SEMICOLON, RIGHT-BRACE are skipped due to they are not used in expression (see https://en.cppreference.com/w/c/language/operator_precedence).
    ;; parse-matchfix-modified have been used similarly in lambda and open-paren-nud manipulation there.
    ;; SEMICOLON, RIGHT-BRACE are just like "," and ")" here both with default error led/nud's.
    ;; 1.a. LEFT-BRACE, SEMICOLON, RIGHT-BRACE should be added since statement may be used in lambda.
    ;; (I won't give one extensive support for Python statement... That is due to focus here is expr)
    (spec-with-implicit-prec spec 'Null NullBrace (list "{"))
    (spec-with-implicit-prec spec 'Left LeftSemicolon (list ";"))

    (spec-with-implicit-prec spec 'Null NullLambda (list "lambda"))
    
    ;; IGNORE For if – else (not offered in oilshell since it is not used in C-expr),
    ;; we should do as https://docs.python.org/3/reference/expressions.html#if-expr
    ;; instead of that in pratt_new_compatible_with_MIT_GNU_Scheme.scm
    (spec-with-implicit-prec spec 'Null NullIf (list "if"))
    (spec-with-implicit-prec spec 'LeftRightAssoc LeftIf (list "if"))

    (spec-with-implicit-prec spec 'Left LeftDefine (list ":="))
    ;;;;;; For pratt_new_compatible_with_MIT_GNU_Scheme.scm
    ;; it has "QUOTE-SYMBOL" left.

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
    (spec-with-implicit-prec spec 'Left LeftLogical (list "or"))
    (spec-with-implicit-prec spec 'Left LeftLogical (list "and"))
    (spec-with-implicit-prec spec 'Null NullPrefixOpWithSentinel (list "not"))

    (spec-with-implicit-prec spec 'Left PrsComparison COMPARISON-OP-LST)

    (spec-with-implicit-prec spec 'Left LeftBitwise (list "&"))
    (spec-with-implicit-prec spec 'Left LeftBitwise (list "^"))
    (spec-with-implicit-prec spec 'Left LeftBitwise (list "|"))

    ;;;; BEHAVIOR
    ;; > shift_expr ::= a_expr | shift_expr ("<<" | ">>") a_expr
    ;; 0. Seq is implied similar to LeftLogical
    ;; 1. a_expr implies we can still use ensure-consistent Sentinel.
    ;; 2. not in pratt_new_compatible_with_MIT_GNU_Scheme.scm
    ;; oilshell uses LeftBinaryOp (so comparison is similar to LeftLogical)
    ;;;; TODO tests
    ;; a & b << c >> d => (& a (>> (<< b c) d))
    (spec-with-implicit-prec spec 'Left PrsSeqWithSentinel SHIFT-OP-LST)

    ;;;; BEHAVIOR
    ;; 0. similar to the above except pratt_new_compatible_with_MIT_GNU_Scheme.scm implements this with parse-nary.
    ;; So see LeftLogical for comparison.
    ;; 0.a. Here I allow (/ a b c) which means (/ a (* b c)) same as Scheme.
    ;;;; TODO tests
    ;; a * d + b << c => (<< (+ (* a d) b) c)
    (spec-with-implicit-prec spec 'Left PrsPlusMinusWithSentinel BINARY-PM-OP-LST)
    (spec-with-implicit-prec spec 'Left PrsSeqWithSentinel OTHER-BINARY-OP-LST)

    ;;;; BEHAVIOR
    ;; Comparison see %NullPrefixOp.
    ;;;; TODO tests
    ;; +-a*b => (* (+ (- a)) b)
    (spec-with-implicit-prec spec 'Null NullPrefixOpWithSentinel UNARY-OP-LST)

    (spec-with-implicit-prec spec 'LeftRightAssoc PrsPower '("**"))

    (spec-with-implicit-prec spec 'Null PrsAwait AWAIT-OP-LST)
    ;;;;;; ensure-consistent check finish

    ;; > attributeref ::= primary "." identifier
    ;; Here primary can be atom like "literal"
    ;; So '"1" . __eq__' (allow space inside) and "1".__eq__("2") is fine https://stackoverflow.com/a/65474446/21294350.
    (spec-with-implicit-prec spec 'Left PrsAttribute '(".") '("get-attrib") MAX-BP)
    ;; similar to the above
    (spec-with-implicit-prec spec 'Null NullPrefixOp '("'") '("quote") MAX-BP)

    ;;; 0. For simplicity, from here I won't implement Sentinel. Anyway it can be done similar to the above.
    ;; If you are interested, you can do that.
    ;;; 1. IGNORE Also here [] has different parsing rules for Subscription and Slicing,
    ; (spec-with-implicit-prec spec 'Left PrsSubscription '("[") '("sublist") MAX-BP)
    ; (spec-with-implicit-prec spec 'Left PrsSlicing '("[") '("slicing") MAX-BP)
    
    ;;;; BEHAVIOR for NullParen (detailed)
    ;; 0. Notice for "parenth_form", we need to ensure "[starred_expression]"
    ;; IMHO that can be combined with expr_list as I have done in NullParen.
    ;; 0.a. See https://docs.python.org/3/reference/grammar.html for tuple definition not shown in https://docs.python.org/3/reference/expressions.html#expression-lists
    ;; 0.a.0. Why
    ;; > In that case, a | must be used before the first alternative
    ;; 0.a.1. IGNORE Notice these 2 are not compatible in some way at least for list_display which allows assignment_expression while the latter only has "assignment_expression" for genexps or args.
    ;; The latter has "list: '[' [star_named_expressions] ']'"
    ;; The former has "list_display ::= "[" [flexible_expression_list | ...] "]""
    ;; star_named_expression in the latter means similar to flexible_expression
    ;; where starred_expression < "'*' bitwise_or" plus "expression !':='".
    ;; Actually "[lambda n: n]" is fine, so use the latter link although I have used the former for all the above...
    ;; 0.a.1.0. The latter has
    ;; > star_named_expression:
    ;; > | '*' bitwise_or 
    ;; > | named_expression
    ;; > 
    ;; > named_expression:
    ;; > | assignment_expression
    ;; > | expression !':='
    ;; so it adds "named_expression".
    ;; 0.a.2. Here I skipped generator_expression/genexp (probably similar. I won't dig into the routine comparison.)
    ;; and also yield_atom/group-yield_expr-part.
    ;; Anyway they are just some specific syntaxes without introducing new parsing techniques.
    ;; 0.a.2.a. I have implemented parenth_form/group-named_expression-part.
    ;; SO in summary, consume-possible-elems-implicitly-and-the-ending-token in NullParen
    ;; should use named_expression-sentinel for ([...:=]elm) part (parenth_form)
    ;; while use star_named_expression's one for ()/(elm,)/(elm,...[,]) parts (* part is skipped due to similarity with "not").
    ;; 1. Comparison see NullParen
    ;;;; TODO tests

    ;;;; BEHAVIOR for list
    ;; Here I will still use https://docs.python.org/3/reference/expressions.html#list-displays as the main reference
    ;; because it is more descriptive although the latter one is more exact.
    ;; As the above implies, list_display (list display) means (list | listcomp) in the latter although has less support.
    ;; IGNORE By searching '"["', this is the mere expr about list.
    ;; 0. > list: '[' [star_named_expressions] ']'
    ;; Similar to tuple but allows [arg] to be one list.
    ;; 1. Again "listcomp" is skipped.

    ;;;; BEHAVIOR for set
    ;; just same as list but with the different delimeters.
    ;; > set: '{' star_named_expressions '}'

    ;;;; BEHAVIOR for dict
    ;; > dict: '{' [double_starred_kvpairs] '}'
    ;; > double_starred_kvpair:
    ;; > | '**' bitwise_or 
    ;; > | kvpair
    ;; So same as set but with "*"=>"**" & enforced mapping with ":="=>":".

    (spec-with-implicit-prec spec 'Null NullConstant CONSTANT-TYPE-STR '("unused" "unused") UNUSED-BP-MARKING-END)
    spec
    ))

(define (MakeParser str)
  (Parser (MakePythonParserSpec) (Tokenize str))
  )

(cd "~/SICP_SDF/SDF_exercises/common-lib/")
; (load "logic_lib.scm")
;; i.e. ParseShell in oilshell
(define (ParsePythonDemo str #!optional expected)
  ;; Here I won't add one extra class wrapper for simplicity.
  (let* ((res ((MakeParser str) 'Parse))
         (res-expr (get-GeneralNode-val res)))
    ;; assert is not same as Python with 2 args
    (and* expected (assert* (equal? expected res-expr) (list "unequal" expected res-expr)))
    ;; Use write to enable outputting cycle.
    (format #t "~40S ~S~%~40S~%" str res res-expr)
    res
    )
  )
