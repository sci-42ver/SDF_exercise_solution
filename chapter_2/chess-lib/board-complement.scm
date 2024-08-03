(define (is-position-occupied-by-non-king-opponent? coords board)
  (and
    (eq? 'occupied-by-opponent (position-info coords board))
    ;; notice the order.
    ;; > The king can be put in check but cannot be captured.
    (not (eq? 'King (piece-type (board-get coords board))))))