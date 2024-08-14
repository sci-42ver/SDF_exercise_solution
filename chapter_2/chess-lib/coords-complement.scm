(define cross-directions 
  (list forward-direction
        backward-direction
        left-direction
        right-direction))

(define all-directions 
  (append cross-directions
          diagonal-directions))

(define (possible-directions piece)
  (case (piece-type piece)
    ((Rook) cross-directions)
    ((Bishop) diagonal-directions)
    ((King Queen) all-directions)
    ; ((Pawn) forward-direction)
    ))
