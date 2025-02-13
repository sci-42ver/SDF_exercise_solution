;; extracted just from address-of for convenience
(define (address-transform board addr)
  (if (board 'white-move?)
    addr
    (invert-address addr))
  )
(define (board-address board x y)
  (address-transform board (make-address x y))
  )
(define (board-address* board addr)
  (address-transform board addr)
  )

(define (my-piece? board pos)
  (eq? (board 'color) (piece-color (board 'piece-at pos))))

(define (white? board)
  (eq? 'white (board 'color))
  )
(define (black? board)
  (eq? 'black (board 'color))
  )
