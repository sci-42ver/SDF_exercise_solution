;; IGNORE: address (from node), type, color (from board) can decide whether initial.
;; king, rook can move backwards, so we need to use one edge to show whether initial.
(define (occupied-by-and-initial type)
  (lambda (place-node dict) 
    (let ((piece (piece-in place-node dict)))
      (and piece
           (eq? type (piece-type piece)))
           ;; added
           (piece-initial-mark piece)
           ))
  )

(define (pawn_piece? piece)
  (eq? (piece-type piece) 'pawn))
(define (rook_piece? piece)
  (eq? (piece-type piece) 'rook))
