(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "loop_lib.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell/")
(load "DataTypeLib.scm")
(load "Parser.scm")
(load "DenotationBaseLib.scm")

;;; different from oilshell (see the following). 
;; No corresponding one in pratt_new_compatible_with_MIT_GNU_Scheme.scm
;;; allow trailing which is not allowed in Shell (in Bash $((1,)) throws error).
;; TODO tests: 1,;1,2,;1,+2,;
;;; TODO Emm... Actually "," must have one much more complexer manipulation in Python which **can't be done by Pratt Parsing**.
;; If using Pratt Parsing, then "+" should just consume the left and then try to find the rhs.
;; 0. +'s rbp > ,'s lbp Then 1+2, is "(1+2)," and 1,+2, is "((1,)+2)," (wrong).
;; 1. +'s rbp <= ,'s lbp Then the former example above is wrong with 1+(2,).
(define (LeftComma p token left rbp)
  ;; 0. For SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/python_demo/pratt-parsing-demo/arith_parse.py
  ;; a,b,c is same as ((a,b),c).
  ;; But here (tuple (tuple a b) c) is obviously different from (tuple a b c),
  ;; so similar to pratt_new_compatible_with_MIT_GNU_Scheme.scm
  ;; we use prsnary.
  ;; 1. For the trailing comma https://docs.python.org/3/reference/expressions.html#expression-lists,
  ;; we check whether we can get one new nud, see the above.
  ;; Emm... I won't dig into the complex syntax grammar rules to find the detailed examples where trailing "," is allowed...
  (cons 'tuple (cons left (PrsNary* token p)))
  ;;; IGNORE since tuple is returned and the 1st element may be also one tuple which should be concatenated,
  ;; we should not depend on the type of left.

  ;;; IGNORE Here I returned (tuple ...)
  )

;;; IGNORE different from SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/python_demo/pratt-parsing-demo/arith_parse.py
;; to allow tuple besides (expr).
(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/pratt_new_compatible_with_MIT_GNU_Scheme/")
(load "compatible_lib.scm")
(define (consume-elems-and-the-ending-paren p rbp)
  (prog1 (p 'ParseUntil rbp) (p 'Eat ")")))
;; 0. function as open-paren-nud.
;; 1. Different from oilshell (i.e. bash) to allow ()=>(tuple).
(define (NullParen p token bp)
  (declare (ignore token)) ; since delimeter is comma.
  (cond 
    ((p 'AtToken ")") (list 'tuple))
    (else
      ;; 0. We can implicitly use LeftComma implied by grammar rule
      ;; Parenthesized form https://docs.python.org/3/reference/expressions.html#parenthesized-forms
      ;; is based on Expression lists https://docs.python.org/3/reference/expressions.html#expression-lists
      ;; 1. see SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/python_demo/pratt-parsing-demo/tests.py
      ;; x[1,2] implies also considering tuple inherently.
      ;; 2. Here I choose ParseUntil to reuse the above LeftComma for modularity.
      ;; 3. Similar to loop in prsmatch-modified but with
      ;; 3.a. ending-token consumption at the end
      ;; 3.b. element list construction is implicitly done in ParseUntil.
      ;; 3.c. (error 'comma-or-match-not-found (token-read stream)) is implicitly done
      ;; by (p 'Eat ")") but more general to allow possible extension like (a;b;).
      (consume-elems-and-the-ending-paren p bp)
      ))
  )

;; 0. Here different from pratt_new_compatible_with_MIT_GNU_Scheme.scm,
;; we can also allow trailing comma, so we do similar to open-paren-nud, i.e. the above NullParen.
;; 1. Trivially same as oilshell.
;;; Tests LeftComma ones plus proc(), proc(a), proc(a,b), proc(a,b,).
(define (LeftFuncCall p token left unused_rbp)
  ;; borrowed
  (and (not (member (Token-type left) var-types))
    (ParseError (list left "can't be called"))
    )
  (cons left (get-possible-tuple-contents (NullParen p token NULL-PAREN-PREC)))
  )
