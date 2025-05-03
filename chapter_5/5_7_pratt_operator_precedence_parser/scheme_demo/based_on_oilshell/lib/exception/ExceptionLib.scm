(define (NullError p token bp)
  (error (list token "can't be used in prefix position")))
(define (LeftError p token left rbp)
  (error (list token "can't be used in infix position")))

(define ParseError error)
