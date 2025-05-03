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
