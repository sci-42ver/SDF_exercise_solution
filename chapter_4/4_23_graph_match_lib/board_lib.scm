;; extracted just from address-of for convenience
(define (address-transform board addr)
  (if (board 'white-move?)
    addr
    (invert-address addr))
  )
(define (board-address board x y)
  
  )