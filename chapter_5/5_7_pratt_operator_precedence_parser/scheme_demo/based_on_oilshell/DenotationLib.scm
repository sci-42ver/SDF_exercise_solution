;;; This obviously can't catch some possible syntax errors in expr because the grammar of this exercise is *not shown explicitly*,
;; And obviously we won't implement one parser like the actual one for Python or C etc.

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
  (PrsSeq parser token left rbp 'tuple)
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
  (consume-possible-elems-implicitly-and-the-ending-token
    p
    unused-rbp
    ")"
    'tuple
    comma-token
    COMMA-PREC
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
  (and (not (member (Token-type left) var-types))
    (ParseError (list left "can't be called"))
    )
  (let ((res (cons left (get-possible-tuple-contents (NullParen p token NULL-PAREN-PREC)))))
    (set-Token-type! token "call")
    (CompositeNode token res)
    )
  )

;;;; BEHAVIOR
;; 0. Semicolon usage in C (checking the standard is a bit too complex https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf)
;; so see https://www.geeksforgeeks.org/role-of-semicolon-in-various-programming-languages/#
;; > Semicolons are end statements in C.
;; > They are *not* used in between the control flow statements but are used in separating the *conditions in looping*. 
;; For simplicity I only consider the 1st.
;; 1. Similar to NullParen.
;;;; TODO tests
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Lambda-Expressions.html#index-lambda-4
;; needs at least one expr.
;; lambda a:{} => (lambda (a) (begin))
;; lambda a:{a+a} => (lambda (a) (+ a a))
;; lambda a:{a+a;a**5} => (lambda (a) (begin (+ a a) (** a 5)))
;; lambda a:{a+a;a**5;} => (lambda (a) (begin (+ a a) (** a 5)))
;; non-error {a,b} => (tuple a b)
(define (NullBrace p token unused-rbp)
  (declare (ignore token)) ; since delimeter is comma.
  (consume-possible-elems-implicitly-and-the-ending-token
    p
    unused-rbp
    "}"
    'begin
    semicolon-token
    LEFT-SEMICOLON-PREC
    )
  )

;;;; BEHAVIOR
;; 0. Similar to C, here I allow something like "int a=1; int b=2;" without outer Braces.
;; 1. Extension: Here I just allow this be one expr... Anyway book exercise doesn't say anything about grammar definition for that infix expression.
;;;; TODO tests (the latter 2 for NullBrace containing ";")
;; lambda a:a+a;a**5; => (begin (lambda (a) (+ a a)) (** a 5))
;; The last just means ";" is like "," but due to statement ending it binds nothing from others at the left.
(define (LeftSemicolon p token left rbp)
  (PrsSeq parser delimeter left rbp 'begin)
  )

;;;; BEHAVIOR
;; 0. same as pratt_new_compatible_with_MIT_GNU_Scheme.scm
;; but 
;; 0.a. ensures arg-node?
;; 0.b. again here we allows "," in body, so rbp<COMMA-PREC.
;; 1. not in oilshell
;;;; TODO tests
;; "lambda a;: ..." error
;; "lambda a+b,: ..." error
;; "lambda a,**b,*c,: ..." works
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
    (let ((body (p 'ParseUntil rbp)))
      (CompositeNode
        token
        (cons* 
          'lambda
          (get-tagged-lst-data intermediate)
          body
          )))
    )
  )

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/")
(load "5_7_regexp_lib_simplified_based_on_effbot_based_on_irregex.scm")
;;;; BEHAVIOR
;; 0. same as pratt_new_compatible_with_MIT_GNU_Scheme.scm
;; but 
;; 0.a. ensures arg-node?
;; 0.b. again here we allows "," in body, so rbp<COMMA-PREC.
;; 1. not in oilshell
;; 2. Use Python doc precedence ordering.
;;;; TODO tests
;; a+b:=c error
;; a:=b,c
(define (LeftDefine p token left rbp)
  (assert (equal? ID-TAG-STR (get-GeneralNode-token-type left)))
  (LeftBinaryOp p token left rbp)
  )

;;;; BEHAVIOR
;; same as pratt_new_compatible_with_MIT_GNU_Scheme.scm
;; less general than https://docs.python.org/3/reference/compound_stmts.html#the-if-statement
;; since ~~not allowing *conditional* else and also~~ no such a "elif".
;; Here I allow "conditional else" as paper does.
;; So "dangling else" is solved with {} https://en.wikipedia.org/wiki/Dangling_else instead of indent https://docs.python.org/3/reference/compound_stmts.html#compound-statements.
;;;; TODO tests
;; Here "if a,b then a,b, else b,a," is allowed.
;; "if a then if b then s1 else s2" is same as "if a then { if b then s1 else s2 }".
;; "if a then { if b then s1 } else s2"
(define (NullIf p token bp)
  (let* ((pred (p 'ParseUntil bp))
         ;; different bp from pratt_new_compatible_with_MIT_GNU_Scheme.scm, but same behavior.
         (consequent (begin (p 'Eat "then") (p 'ParseUntil bp))))
    (set-Token-type! token "null-if")
    (CompositeNode
      token
      (cons* 
        'if
        pred
        then
        (if (p 'AtToken "else")
          (begin (p 'Eat "else") (p 'ParseUntil bp))
          '())
        ))
    )
  )

(define (LeftIf p token left rbp)
  
  )
