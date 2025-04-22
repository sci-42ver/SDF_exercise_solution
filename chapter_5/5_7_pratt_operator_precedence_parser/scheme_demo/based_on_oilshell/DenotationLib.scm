;;; reference
;; effbot (only checked for those not in oilshell and pratt_new_compatible_with_MIT_GNU_Scheme.scm) https://web.archive.org/web/20101216050812/http://effbot.org/zone/simple-top-down-parsing.htm#multi-token-operators

;;; This obviously can't catch some possible syntax errors in expr 
;; because the grammar of this exercise is *not shown explicitly* (so actually no exact definition for error...),
;; And obviously we won't implement one parser like the actual one for Python or C etc.

;;; DEBUG INFO: 
;; ";;;; BEHAVIOR" and ";;;; TODO tests" number expected: 9 (by re pat "^\(define ")

(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "loop_lib.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell/")
(load "DataTypeLib.scm")
(load "Parser.scm")
(load "DenotationBaseLib.scm")

;;;; BEHAVIOR
;; 0. different from oilshell (see the following).
;; No corresponding one in pratt_new_compatible_with_MIT_GNU_Scheme.scm
;; 1. allow trailing which is not allowed in Shell (in Bash $((1,)) throws error).
;;;; TODO tests
;; 1,;1,2,;1,+2,=>1,(+2),;
;;; IGNORE TODO Emm... Actually "," must have one much more complexer manipulation in Python which **can't be done by Pratt Parsing**.
;; If using Pratt Parsing, then "+" should just consume the left and then try to find the rhs.
;;; 0. +'s rbp > ,'s lbp Then 1+2, is "(1+2)," and 1,+2, is ~~"((1,)+2)," (wrong)~~
;; 1,(+2), exceptedly (implied by 1,-2, results in (1, -2). It is flexible_expression_list).
;; 1. +'s rbp <= ,'s lbp Then the former example above is wrong with 1+(2,).
(define (LeftComma p token left rbp)
  ;; 0. For pratt-parsing-demo/arith_parse.py
  ;; a,b,c is same as ((a,b),c).
  ;; But here (tuple (tuple a b) c) is obviously different from (tuple a b c),
  ;; so similar to pratt_new_compatible_with_MIT_GNU_Scheme.scm
  ;; we use prsnary.
  ;; 1. For the trailing comma https://docs.python.org/3/reference/expressions.html#expression-lists,
  ;; we check whether we can get one new nud, see the above.
  ;; Emm... I won't dig into the complex syntax grammar rules to find the detailed examples where trailing "," is allowed...
  (new-GeneralNode-simplified
    (PrsSeq p token left rbp 'tuple)
    token
    (get-token-type-from-caller-and-op LeftComma token)
    )
  ;;; IGNORE since tuple is returned and the 1st element may be also one tuple which should be concatenated,
  ;; we should not depend on the type of left.

  ;;; IGNORE Here I returned (tuple ...)
  )

;;;; BEHAVIOR 
;; different from oilshell (see the following).
;; same as pratt_new_compatible_with_MIT_GNU_Scheme.scm allowing tuple ()/(a,b[,]) etc where [] meansing optional besides (expr).
;;;; TODO tests
;; besides those in pratt_new_compatible_with_MIT_GNU_Scheme.scm:
;; error (a+b;a**b)
(define (NullParen p token unused-rbp)
  (declare (ignore token)) ; since delimeter is comma.
  (new-GeneralNode-simplified
    (consume-possible-elems-implicitly-and-the-ending-token
      p
      unused-rbp
      ")"
      'tuple
      comma-token
      COMMA-PREC
      )
    token
    ;; needed to differentiate it from "a,b..." since the latter is not one expr but the former is.
    (get-token-type-from-caller-and-op NullParen token)
    )
  )

;;;; BEHAVIOR
;; 0. Here different from pratt_new_compatible_with_MIT_GNU_Scheme.scm,
;; we can also allow trailing comma, so we do similar to open-paren-nud, i.e. the above NullParen.
;; 1. Trivially same as oilshell.
;;;; TODO tests
;; proc(), proc(a), proc(a,b), proc(a,b,) (actually all done above in NullParen).
(define (LeftFuncCall p token left unused-rbp)
  ;; borrowed from oilshell.
  (ensure-var left)
  ;; token is unused in NullParen
  (new-GeneralNode-simplified
    (cons 
      (get-GeneralNode-val left) 
      (get-possible-tuple-contents (NullParen p token NULL-PAREN-PREC)))
    token
    (get-token-type-from-caller-and-op LeftFuncCall token)
    )
  )

;;;; BEHAVIOR
;; 0. Semicolon usage in C (checking the standard is a bit too complex https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf)
;; so see https://www.geeksforgeeks.org/role-of-semicolon-in-various-programming-languages/#
;; > Semicolons are end statements in C.
;; > They are *not* used in between the control flow statements but are used in separating the *conditions in looping*. 
;; For simplicity I only consider the 1st.
;; 1. Similar to NullParen.
;; So extension: Here I just allow this be one expr... Anyway book exercise doesn't say anything about grammar definition for that infix expression.
;;;; TODO tests
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Lambda-Expressions.html#index-lambda-4
;; needs at least one expr.
;; lambda a:{} => (lambda (a) (begin))
;; lambda a:{a+a} => (lambda (a) (+ a a))
;; lambda a:{a+a;a**5} => (lambda (a) (begin (+ a a) (** a 5)))
;; lambda a:{a+a;a**5;} => (lambda (a) (begin (+ a a) (** a 5)))
;; non-error {a,b} => (tuple a b)
(define (NullBrace p token unused-rbp)
  (new-GeneralNode-simplified
    (consume-possible-elems-implicitly-and-the-ending-token
      p
      unused-rbp
      "}"
      'begin
      semicolon-token
      LEFT-SEMICOLON-PREC
      )
    token
    )
  )

;;;; BEHAVIOR
;; 0. Similar to C, here I allow something like "int a=1; int b=2;" without outer Braces.
;;;; TODO tests (the latter 2 for NullBrace containing ";")
;; 0. lambda a:a+a;a**5; => (begin (lambda (a) (+ a a)) (** a 5))
;; The last just means ";" is like "," but due to statement ending it binds nothing from others at the left.
;; 1. a,b;c,d (begin (tuple a b) (tuple c d))
(define (LeftSemicolon p token left rbp)
  ; token won't be used in PrsNary* of PrsSeq, so fine to set-Token-type! beforehand.
  (new-GeneralNode-simplified
    (PrsSeq p token left rbp 'begin)
    token
    (get-token-type-from-caller-and-op LeftSemicolon token)
    )
  )

;;;; BEHAVIOR
;; 0. same as pratt_new_compatible_with_MIT_GNU_Scheme.scm
;; but 
;; 0.a. ensures arg-node?
;; 0.b. again here we allows "," in body, so rbp<COMMA-PREC.
;; 1. not in oilshell
;; 2. similar to effbot except
;; 2.a. it uses explicit argument_list to check "token.id" without calling Parse
;; 2.b. it doesn't allow trailing comma.
;;;; TODO tests
;; "lambda a;: ..." error
;; "lambda a+b,: ..." error
;; "lambda a,**b,*c,: ..." works
;; "lambda a: b:=4" works
;; "lambda a: lambda b: a*b" works
;; pratt_new_compatible_with_MIT_GNU_Scheme.scm ones (not allowing := same as Python).
;;;; Won't implement
;; > If a parameter has a default value, all following parameters up until the “*” must also have a default value — 
;; > this is a syntactic restriction that is not *expressed by the grammar*.
(define (NullLambda p token rbp)
  (declare (ignore token)) ; since delimeter is comma.
  (let ((intermediate 
          (consume-possible-elems-implicitly-and-the-ending-token
            p
            'unused-rbp
            ":"
            'lambda
            comma-token
            COMMA-PREC
            arg-node?
            )))
    (let ((body-contents (get-GeneralNode-val (p 'ParseUntil rbp))))
      (CompositeNode
        token
        (cons*-wrapper 
          'lambda
          (get-tagged-lst-data (get-GeneralNode-val intermediate))
          body-contents
          )))
    )
  )

;;;; BEHAVIOR
;; trivial by returning self same as oilshell
;; For pratt_new_compatible_with_MIT_GNU_Scheme.scm, this is inherent inside nudcall.
;;;; TODO tests
;; /, * => return self. (also see lambda)
(define (NullConstant p token unused-rbp)
  (declare (ignore p))
  (set-Token-type-same-as-val! token) ; type is string.
  (CompositeNode token (string->symbol (Token-val token))) ; pass symbol to be eval'ed in Scheme.
  )

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/")
(load "5_7_regexp_lib_simplified_based_on_effbot_based_on_irregex.scm")
;;;; BEHAVIOR
;; 0. same as pratt_new_compatible_with_MIT_GNU_Scheme.scm with different bp
;; i.e. Use Python doc precedence ordering.
;; 1. not in oilshell
;;;; TODO tests
;; a+b:=c error
;; a:=b,c => (tuple (define a b) c)
;; a:=b:=c error
;; a:= lambda k:3+b+k is fine
;; a:= b or c => (define a (or b c))
(define (LeftDefine p token left unused-rbp)
  (ensure-identifier left)
  ;; token-type is unused in LeftBinaryOp
  (new-GeneralNode-simplified
    ;; Python
    ;; > assignment_expression ::= [identifier ":="] expression
    (LeftBinaryOp p token left EXPR-BASE-PREC)
    token 
    (get-token-type-from-caller-and-op LeftDefine token)
    )  
  )

;;;; BEHAVIOR
;; same as pratt_new_compatible_with_MIT_GNU_Scheme.scm
;; less general than https://docs.python.org/3/reference/compound_stmts.html#the-if-statement
;; since ~~not allowing *conditional* else and also~~ no such a "elif".
;; Here I allow "conditional else" as paper does.
;; So "dangling else" is solved with {} https://en.wikipedia.org/wiki/Dangling_else instead of indent https://docs.python.org/3/reference/compound_stmts.html#compound-statements.
;;;; TODO tests
;; IGNORE Here "if a,b then a,b, else b,a," is allowed.
;; Same as Python if a,b ... is forbidden.
;; "if a then if b then s1 else s2" is same as "if a then { if b then s1 else s2 }".
;; "if a then { if b then s1 } else s2"
(define (NullIf p token bp)
  (let* ((pred (p 'ParseUntil bp))
         ;; different bp from pratt_new_compatible_with_MIT_GNU_Scheme.scm, but same behavior.
         (consequent (begin (p 'Eat "then") (p 'ParseUntil bp))))
    (set-Token-type! token (get-token-type-from-caller-and-op NullIf token))
    (CompositeNode
      token
      (cons*-wrapper
        'if
        (get-GeneralNode-val pred)
        (get-GeneralNode-val consequent)
        (if (p 'AtToken "else")
          (begin (p 'Eat "else") (get-GeneralNode-val (p 'ParseUntil bp)))
          '())
        ))
    )
  )

;;;; BEHAVIOR
;; 0. not in pratt_new_compatible_with_MIT_GNU_Scheme.scm and oilshell
;; 0.a. similar to effbot (but I don't know why it uses infix_r for "or" etc instead of "if")
;; https://web.archive.org/web/20121101045119/https://docs.python.org/3/reference/expressions.html#comparisons
;; uses "chain from left to right" (so a<b<c is not same as ((a<b) < c))
;; 0.a.0. see SentinelLib.scm: here ensure-or-test-expr is necessary.
;; 1. similar to ** in Python, so similar to that in pratt_new_compatible_with_MIT_GNU_Scheme.scm and oilshell
;;;; TODO tests
;; a:= 3+b if a**2 else b => (define a (if (** a 2) (+ 3 b) b))
;; a:= lambda k:3+b+k if a else b;c (not one error) => (begin (define a (lambda (k) (if a (+ 3 b k) b))) c)
;; a:= 3+b+k if a else b;c => (begin (define a (if a (+ 3 b k) b)) c)
;; a:= 3+b+k if a else {b;c} => (define a (if a (+ 3 b k) (begin b c)))
;; a if b if b_pred else b_alt else a_alt => (if (if b_pred b b_alt) a a_alt)
;; a if b else c if c_pred else c_alt => (if b a (if c_pred c c_alt)) https://stackoverflow.com/a/79497941/21294350
(define (LeftIf p token left rbp)
  (define get-intermediate-consq get-binary-left)
  (define get-intermediate-pred get-binary-right)
  (let ((intermediate (LeftBinaryOp p token left rbp))) ; (if consq pred)
    (let ((pred (get-intermediate-pred intermediate))
          (consq (get-intermediate-consq intermediate)))
      ;; Python
      ;; > conditional_expression ::= or_test ["if" or_test "else" expression]
      (ensure-or-test-expr pred consq)

      (set-Token-type! token (get-token-type-from-caller-and-op LeftIf token))
      (p 'Eat "else")
      (let ((alt (p 'ParseUntil EXPR-BASE-PREC)))
        (CompositeNode
          token
          (cons*-wrapper
            'if
            (get-GeneralNode-val pred)
            (get-GeneralNode-val consq)
            (get-GeneralNode-val alt)
            ))
        )
      )
    )
  )
;;;; BEHAVIOR
;; Same as + etc in pratt_new_compatible_with_MIT_GNU_Scheme.scm with parse-nary.
;; So not same as oilshell LeftBinaryOp.
;; For or, left can't be those op's ending with expression.
(define (LeftOr p token left rbp)
  
  )
