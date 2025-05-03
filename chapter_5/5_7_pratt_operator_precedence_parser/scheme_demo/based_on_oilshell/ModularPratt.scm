;; define-syntax put first to make all the following can use that, see https://stackoverflow.com/q/79147397/21294350.
(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "list_lib.scm")
(load "application_lib.scm")
(load "test_lib.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/")
(load "pratt_new_compatible_with_MIT_GNU_Scheme/compatible_lib.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/")
; (load "5_7_regexp_lib_simplified_based_on_effbot_based_on_irregex.scm")
(load "5_7_regexp_lib_simplified_based_on_effbot_and_irregex_not_using_coroutine.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "Parser.scm")

(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "pair_lib.scm")
(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "ParserSpec.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "lib/exception/ExceptionLib.scm")
(load "lib/denotation/DenotationLib.scm")
(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "lib/sentinel/SentinelBaseDataLib.scm")
; (load "SentinelLib.scm")
(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "string_lib.scm")
(load "pred_lib.scm")
(load "base_procedure_lib.scm")
(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "lib/sentinel/SentinelBaseLib.scm")
(load "lib/sentinel/SentinelLib.scm")
(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "ParsePythonDemo.scm")

;;; orig
;;; OPEN-PAREN etc & value-if-symbol & value-if-symbol* are not needed due to all intermediate elements are string.

;;; For pl,
;; > (set! l (append l '($)))
;; is done in 5_7_regexp_lib_simplified_based_on_effbot_based_on_irregex.scm
;; > toplevel-parse
;; stream for that is in Parser proc where "Next" does "get" but not return token back. 
;; "peek" here can be done only inside Parser bundle self by checking token.
;; (eof-val) is replaced by error since we have explicitly added eof at the beginning IMHO (so no implicit eof).


;;; For toplevel-parse,
;; same as ParseUntil where token-read in parse is partially done in Parse.
;; error is thrown if starting with eof instead of just stopping further parsing.

;;; For nudcall,
;; all in spec where error is explicitly added by NullError.
;; There is no token without both nud and led, except those used implicitly in nud/lef possibly. 

;;; For ledcall,
;; similarly, either have one valid led or LeftError.

;;; For lbp & rbp, I said somewhere default base-bp is better.

;;; For parse, see the above toplevel-parse. It is just also inside ParseUntil.

;;; For header, see get-header-for-token.

;;; For parse-prefix & parse-infix & parse-nary & parse-matchfix-modified (prsmatch-modified)
;;; & prsmatch
;; Use `grep 'parse-prefix' -r based_on_oilshell` etc.

;;; For parse-matchfix
;; See parse-matchfix-modified part in based_on_oilshell.

;;; For delim-err & erb-err
;; it is combined with NullError.

;;; For premterm-err,
;; it is combined with NullError and also inherent in ParseUntil.

;;; For get-syntax & set-syntax
;; They are inside spec and other tables like *token-type-list* etc.
;;; For their implementation, they are similar to multi-hash-set! etc except
;; 0. multi-hash-ref has no implicit "empty-hash-table".
;; 1. More general with > 2 levels.

;;; For *defsyntax,
;; no corresponding part. Just set! independently for each related data structure.

;;; For defsyntax etc, just see get-syntax & set-syntax.

(define-syntax pl-assert 
  (syntax-rules ()
    ((_ expected exp)
      (begin
        (ParsePythonDemo exp expected)
        ; (ParsePythonDemo exp)
        )
      )
    )
  )

(trace NullParen)
(trace PrsNary*)
(trace return-last)
(trace ensure-consistent)
; (trace multi-hash-ref)
; (trace multi-hash-ref*)
(trace get-expr-token-types-with-consistent-prec)
(trace sentinel-for-one-node)
; (trace %get-prec)
(trace get-left-lbp)

(pl-assert 
  '(if (g a b) (> a b) (+ (* k c) (* a b)))
  "if g(a,b) then a>b else k * c + a * b"
  )

;; 0. = should == in ex 5.7.
;; 1. TODO how to assert for one error to happen which is said in one former lib of one rec/lec etc.
; (pl-assert 
;   '(define (f a) (+ a (/ b c))) 
;   "f ( a ) := a + b / c")
;Assertion failed: (equal? id-tag-str (get-generalnode-token-type node))

;; it is interpreter's responsibility to define ==.
(pl-assert 
  '(== (f a) (+ a (/ b c))) 
  "f ( a ) == a + b / c")

(pl-assert 
  '(g) 
  "g ( )")

;; tests from SDF_exercises/chapter_5/5_7.scm
(pl-assert
  '(- (expt b 2) (* 4 a c))
  "b ** 2 - 4 * a * c")
(pl-assert 
  '(/ (+ (- b) (sqrt discriminant)) (* 2 a))
  "( - b + sqrt ( discriminant ) ) / ( 2 * a )")

; ;; tuple
(pl-assert 
  '(+ 3 2)
  "( 3 ) + ( 2 )")
(pl-assert 
  '(+ (tuple) 2)
  "( ) + ( 2 )")
(pl-assert 
  '(+ (tuple 1 3 7) 2)
  "( 1 , 3 , 7 ) + ( 2 )")
(pl-assert 
  '(+ (tuple 1 3 7) 2)
  "( 1 , 3 , 7 , ) + ( 2 )")

(pl-assert 
  '(define fact (lambda (n) (if (== n 0) 1 (* n (fact (- n 1))))))
  "fact := lambda n : if n == 0 then 1 else n * fact ( n - 1 )")

;; Here all possible elements for parameter_list in SDF_exercises/chapter_5/5_7_naive_algorithm_for_operator_precedence_parser/5_7_precedence_lib.scm
;; are included here (Based on memory for what is done to this program at some time after finishing this program).
(pl-assert
  '(define fact (lambda (a k b c *args kwarg1 **kwargs) (if (== n 0) 1 (* n (fact (- n 1))))))
  ;; 0. Here b=0 is not considered for simplicity.
  ;; 1. Notice here we need "**kwargs" immediate follows "," otherwise it can't
  ;; be differentiated from "a **kwargs" etc.
  ;; 2. k := 2 is excluded by arg-node?.
  "fact := lambda a , k , b , c ,*args , kwarg1 ,**kwargs : if n == 0 then 1 else n * fact ( n - 1 )")

;;;; Compatibility with Python
;;; 0. Call https://docs.python.org/3/reference/expressions.html#calls
;; Possible arg types:
;; a. assignment_expression := (see SDF_exercises/chapter_5/5_7_related_python_behavior/assignment_expression_arg.py for its difference from keyword_item)
;; [identifier ":="] expression
;; b. *expression
;; c. identifier=expression
;; d. **expression
;; In Scheme only the mere expression is supported natively.
;; For parsing, all can be supported although := inside arglist is a bit weird.

; (pl-assert 'ignore "lambda a :") ; throw error consistent with MIT/GNU Scheme convention
;Unexpected end of input when we needs one nud

(pl-assert
  '(lambda (a) (begin (expt a a) (* a 5)))
  "lambda a : { a ** a ; a * 5 }")

(pl-assert '(and 1 2 3) "1 and 2 and 3")
;; here and should not use the greatest rbp...
(pl-assert '(and 1 (+ 2 3) 3) "1 and 2 + 3 and 3")

;; Although := in Python has the lowest precedence, it won't does something like "1 and 2 := 3".
;; TODO That is due to the grammar and its related constraint (https://stackoverflow.com/a/79544622/21294350), so the actual parser is complexer...

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell/test")
(load "parse_tests.scm")
