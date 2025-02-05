(define basic-queen-move
  `((? source-node ,(occupied-by 'queen))
    (* north (?* intermediate-possible-nodes ,unoccupied))
    north (? target-node ,maybe-opponent)))
(define all-queen-moves
  (symmetrize-move basic-queen-move
                   rotate-45 rotate-90 rotate-180))

(define basic-rook-move
  `((? source-node ,(occupied-by 'rook))
    (* north (?* intermediate-possible-nodes ,unoccupied))
    north (? target-node ,maybe-opponent)))

(define all-rook-moves
  (symmetrize-move basic-rook-move
                   rotate-90 rotate-180))

(define basic-bishop-move
  `((? source-node ,(occupied-by 'bishop))
    (* northeast (?* intermediate-possible-nodes ,unoccupied))
    northeast (? target-node ,maybe-opponent)))

(define all-bishop-moves
  (symmetrize-move basic-bishop-move
                   rotate-90 rotate-180))
