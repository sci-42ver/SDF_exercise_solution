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
