;; IGNORE: address (from node), type, color (from board) can decide whether initial.
;; king, rook can move backwards, so we need to use one edge to show whether initial.
(define (occupied-by-and-initial type)
  (lambda (place-node dict) 
    (let ((piece (piece-in place-node dict)))
      (write-line (list "occupied-by-and-initial" type place-node piece (and piece (piece-initial-mark piece))))
      (and piece
           (eq? type (piece-type piece))
           ;; added
           (piece-initial-mark piece))
           ))
  )

(define (pawn_piece? piece)
  (eq? (piece-type piece) 'pawn))
(define (rook_piece? piece)
  (eq? (piece-type piece) 'rook))

(define (opponent place-node dict)
  (let ((target-piece (piece-in place-node dict)))
    (and
      target-piece
      (piece-is-opponent?
        target-piece
        (piece-in (match:get-value 'source-node dict) dict)))
    ))
