;; NullError
; (pl-assert
;   'ignore
;   ")")
; ((token ")" ")") "can't be used in prefix position")

;;; LeftComma
;; left for interpreter to simplify this.
(pl-assert
  '(tuple 1 (+ 2))
  "1,+2,")

(pl-assert
  '(tuple left)
  "left,")
(pl-assert
  '(tuple left arg1)
  "left,arg1")
(pl-assert
  '(tuple left arg1)
  "left,arg1,")
;; This works in Python like "1,*[1]".
(pl-assert
  '(tuple left arg1 *rest)
  "left,arg1,*rest")
(pl-assert
  '(tuple left arg1 arg2)
  "left,arg1,arg2")

(pl-assert
  '(tuple (+ 1 2))
  "1+2,")

;;; LeftFuncCall
(pl-assert
  '(proc)
  "proc()")
(pl-assert
  '(proc a)
  "proc(a)")
(pl-assert
  '(proc a b)
  "proc(a,b)")
(pl-assert
  '(proc a b)
  "proc(a,b,)")

;;; NullParen
; (pl-assert
;   'error-expected
;   "(a+b;a**b)")
;expected ")", got (token ";" ";")
(pl-assert
  '(define a 1)
  "(a:=1)")

;;; NullBrace
(pl-assert
  '(lambda (a) (begin))
  "lambda a:{}")
(pl-assert
  '(lambda (a) (+ a a))
  "lambda a:{a+a}")
(pl-assert
  '(lambda (a) (begin (+ a a) (expt a 5)))
  "lambda a:{a+a;a**5}")
(pl-assert
  '(lambda (a) (begin (+ a a) (expt a 5)))
  "lambda a:{a+a;a**5;}")
(pl-assert
  '(tuple a b)
  "{a,b}")

;;; LeftSemicolon
(pl-assert
  '(begin (lambda (a) (+ a a)) (expt a 5))
  "lambda a:a+a;a**5;")
(pl-assert
  '(begin (tuple a b) (tuple c d))
  "a,b;c,d")

;;; NullLambda
; (pl-assert
;   'error-expected
;   "lambda a;: a")
;expected ":", got (token ";" ";")
(trace arg-node?)
; (pl-assert
;   'error-expected
;   "lambda a+b,: a")
; (#[compound-procedure 117 arg-node?] (compositenode (token "left-plus" "+") (+ a b)) "fails")

;; left for interpreter to manipulate with "**b *c" 
(trace PrsNary)
(pl-assert
  '(lambda (a **b *c) (+ a (g b) c))
  "lambda a,**b,*c,: a+g(b)+c")
(pl-assert
  '(lambda (a **b *c) (define b 4))
  "lambda a,**b,*c,: b:=4")
(pl-assert
  '(lambda (a) (lambda (b) (* a b c)))
  "lambda a: lambda b: a*b*c")

;;; NullIf
;; Here we allow this. You can add sentinel to forbid this.
(pl-assert
  '(if (tuple a b) a b)
  "if a,b then a else b")
(pl-assert
  '(if a (if b s1 s2))
  "if a then if b then s1 else s2")
(pl-assert
  '(if a (if b s1) s2)
  "if a then { if b then s1 } else s2")

;;; LeftIf
(pl-assert
  '(define a (if (expt a 2) (+ 3 b) b))
  "a:= 3+b if a**2 else b")
(pl-assert
  '(begin (define a (lambda (k) (if a (+ 3 b k) b))) c)
  "a:= lambda k:3+b+k if a else b;c")
(pl-assert
  '(begin (define a (if a (+ 3 b k) b)) c)
  "a:= 3+b+k if a else b;c")
(pl-assert
  '(define a (if a (+ 3 b k) (begin b c)))
  "a:= 3+b+k if a else {b;c}")
;; error-expected
; (pl-assert
;   '(if (if b_pred b b_alt) a a_alt)
;   "a if b if b_pred else b_alt else a_alt")
; ((compositenode (token "left-if" if) (if b_pred b b_alt)) "has tag in" ("lambda" "left-if" "statement-block" "null-if" "expr-list" ...))
(pl-assert
  '(if b a (if c_pred c c_alt))
  "a if b else c if c_pred else c_alt")
(pl-assert
  '(tuple a (if c b d))
  "a,b if c else d")

;;; LeftDefine
; (pl-assert
;   'error-expected
;   "a+b:=c")
; ((compositenode (token "left-plus" "+") (+ a b)) "isn't one identifier")
(pl-assert
  '(tuple (define a b) c)
  "a:=b,c")
(trace LeftDefine)
;; This needs sentinel since assignment_expression is not one expression which can be checked by "a:=2,"
;; You can add that similar to how other sentinels are added.
; (pl-assert
;   'error-expected
;   "a:=b:=c")

(pl-assert
  '(define a (lambda (k) (+ 3 b k)))
  "a:= lambda k:3+b+k")
(pl-assert
  '(define a (or b c))
  "a:= b or c")

;;; LeftLogical & NullPrefixOpWithSentinel for "not"
(pl-assert
  '(if b a (or c d))
  "a if b else c or d")
(pl-assert
  '(if (or b c) a d)
  "a if b or c else d")
(pl-assert
  '(or a (and b c (not d)))
  "a or b and c and not d")
; (pl-assert
;   'error-expected
;   "a or b or")
;Unexpected end of input when we needs one nud

;;; PrsComparison
(pl-assert
  '(or a (and b c) (and (not (<= d e)) f) (not (and (in g h) (not-in h i) (== i j))))
  ; '(and (or a (and b c) (not (<= d e))) (or f (not (and (in g h) (not-in h i) (== i j)))))
  "a or b and c or not d <= e and f or not g in h not in i == j")

;;; LeftBitwise
(pl-assert
  '(or a (and b c) (and (not (<= d e)) f) (not (and (in g h) (not-in h i) (== i (bitwise-or (^ (& j k) m) n)))))
  ; '(and (or a (and b c) (not (<= d e))) (or f (not (and (in g h) (not-in h i) (== i j)))))
  "a or b and c or not d <= e and f or not g in h not in i == j & k ^ m | n")

;;; PrsSeqWithSentinel for shift_expr
(pl-assert
  '(& a (>> (<< b c) d))
  "a & b << c >> d")

;;; PrsPlusMinusWithSentinel & PrsSeqWithSentinel for OTHER-BINARY-OP-LST
(pl-assert
  '(<< (+ (* a d) b) c)
  "a * d + b << c")

;;; NullPrefixOpWithSentinel for UNARY-OP-LST
(pl-assert
  '(* (+ (- a)) b)
  "+-a*b")

;;; PrsPower
(pl-assert
  '(expt (get-attrib a elm) pow)
  "a.elm**pow")
(trace NullPrefixOpWithSentinel)
(trace NullPrefixOpWithCustomSentinel)
(pl-assert
  '(- (expt (await a) (- (expt b (~ c)))))
  "- await a ** -b ** ~c")
(pl-assert
  '(expt a (await b))
  "a ** await b")
