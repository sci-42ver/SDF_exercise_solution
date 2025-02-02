;; > A pawn is (almost) the only
;; > piece whose possible moves depend on its position or the position
;; > of a neighboring opponent.
;; 0. "depend on its position"
;; https://en.wikipedia.org/wiki/Rules_of_chess#Movement
;; > If it has not yet moved, a pawn also has the option of moving two squares straight forward, provided both squares are vacant.
;; 1. "neighboring opponent"
;; En passant https://en.wikipedia.org/wiki/Rules_of_chess#En_passant
;; > When a pawn advances two squares on its *initial* move and ends the turn adjacent to an enemy pawn on the same rank, it may be captured en passant by the enemy pawn 
;; > as if it had moved only one square.
;; > This capture is legal only on the move *immediately* following the pawn's advance.
;; i.e. as if the normal capture.
;; Also see footnote 16.

;; pawn's move
;; 0. > straight forward one square
;; 1. > If it has not yet moved ... moving two squares straight forward
;; 2. > capture an enemy piece on either of the two squares diagonally in front of the pawn
;; 3. > The pawn is also involved in the two special moves en passant and *promotion*.
;; en passant is skipped by the Exercise
;; promotion just changes node's property.

;; castling
;; > A special move with the king known as castling is allowed *only once* per player, per game
;; is implied by
;; > The king and rook involved in castling must *not have previously moved*

;; > the rule that a king cannot be moved into check
;; i.e. the following wikipedia where leaves means "the opposing player"'s recent move can check king and then we leave "king in check" without doing some action to avoid that.
;; > It is illegal to make a move that places or leaves one's king in check.
;; leave corresponds to where we need to avoid capture at the next move
;; > The king can be *put in check* but cannot be captured

;;; > We have shown how to make *patterns* ...
;; So we can ignore the implementation for initial?, check?.
;; This is just how the book does for occupied-by etc whose related codes aren't shown in the book.
;;; compared with SDF_exercises/software/sdf/pattern-matching-on-graphs/chess-moves.scm
;; Here we add
;; 0. Castling
;; 1. all-pawn-moves

;;; a. trivial based on all-queen-moves
;; wrong. We need to change the type label
(define basic-bishop-move (cadr (symmetrize-move basic-queen-move rotate-45)))
(define all-bishop-moves
  (symmetrize-move basic-bishop-move
                   rotate-90 rotate-180))

;; same as code base
(define basic-rook-move basic-queen-move)
(define simple-rook-moves (symmetrize-move basic-rook-move rotate-90 rotate-180))

;;; b. For the above point 1, we need to check the next move's row (but that is unsupported by make-graph-node. So add initial?).
;; For the above point 2, we use piece-is-opponent?.
(define (opponent place-node dict)
  (piece-is-opponent?
   (piece-in place-node dict)
   (piece-in (match:get-value 'source-node dict) dict)))
(define pawn-capture-moves
  (map
    (lambda (direction)
      `((? source-node ,(occupied-by 'pawn))
        ,direction (? target-node ,opponent))
      )
    '(northeast northwest))
  )
;; or
(define basic-pawn-capture-move
  `((? source-node ,(occupied-by 'pawn))
      northeast (? target-node ,opponent))
  )
(define pawn-capture-moves
  (symmetrize-move basic-pawn-capture-move rotate-90)
  )

(define basic-pawn-moves
  ;; TODO occupied-by-and-initial
  (list
    ;; point 0
    `((? source-node ,(occupied-by 'pawn))
      ;; Use target-node at last always to ensure capture? work.
      north (? target-node ,unoccupied))
    ;; point 1
    `((? source-node ,(occupied-by-and-initial 'pawn))
      ;; Here (? ,unoccupied) is fine. See (gmatch:compile-var var)->(match-list? var gmatch:var-type? procedure?).
      north (? ,unoccupied)
      north (? target-node ,unoccupied)
      )))
(define all-pawn-moves
  (append pawn-capture-moves basic-pawn-moves))

;;; c. just means
;; > The king moves exactly one square horizontally, vertically, or diagonally.
;; similar to a.
(define all-directions 
  '(east
    north
    northeast
    northwest
    south
    southeast
    southwest
    west))
(define simple-king-moves
  (map
    (lambda (direction)
      `((? source-node ,(occupied-by 'king))
        ,direction (? target-node ,maybe-opponent))
      )
    all-directions)
  )
;; or
;; same as SDF_exercises/software/sdf/pattern-matching-on-graphs/chess-moves.scm
(define basic-king-move
  `((? source-node ,(occupied-by 'king))
      north (? target-node ,maybe-opponent))
  )
(define simple-king-moves
  (symmetrize-move basic-king-move
                   rotate-45 rotate-90 rotate-180)
  )

;;; d. 
;; > The king and rook involved in castling must not have previously moved;
;; > The castling rook must be on the same rank as the king
;; similar to b.
;; > There must be no pieces between the king and the rook;
;; use unoccupied
;; > The king may not currently be under attack, nor may the king pass through or end up in a square that is under attack by an enemy piece
;; add check?

;; IGNORE: Since these 2 pieces are of the same color, we can assume source piece is always king.
(define (same-color place-node dict)
  (piece-is-same-color?
   (piece-in place-node dict)
   (piece-in (match:get-value 'source-node dict) dict)))
(define (piece-is-same-color? piece my-piece)
  (not (piece-is-opponent? piece my-piece)))
(define castling-king-moves
  ;; TODO occupied-by-and-initial-and-bl-rook-initial...
  (list
    `((? source-node ,(occupied-by-and-initial-and-br-rook-initial 'king))
      ;; TODO unoccupied-and-unchecked.
      east (? ,unoccupied-and-unchecked)
      east (? target-node ,unoccupied-and-unchecked)
      )
    `((? source-node ,(occupied-by-and-initial-and-bl-rook-initial 'king))
      west (? ,unoccupied-and-unchecked)
      west (? target-node ,unoccupied-and-unchecked)
      )))
;; TODO king-castling
(define (king-castling-with_bl parameters)
  body)

;; white-br-rook moves 2 steps west but black-br-rook is 3.
(define white-castling-rook-moves
  ;; TODO occupied-by-and-initial-and-king-castling...
  (list
    `((? source-node ,(occupied-by-and-initial-and-white-and-king-castling-with_bl-and-bl 'rook))
      ;; > There must be no pieces between the king and the rook;
      east (? ,unoccupied)
      east (? ,unoccupied)
      east (? target-node ,unoccupied)
      )
    `((? source-node ,(occupied-by-and-initial-and-white-and-king-castling-with_br-and-br 'rook))
      west (? ,unoccupied)
      west (? target-node ,unoccupied)
      )))

;; black is similar.
;; Notice white king goes east 2 steps and black king *also* goes east 2 steps
;;; IGNORE: See SDF_exercises/software/sdf/pattern-matching-on-graphs/chess-board.scm
;; where black will automatically use the reverse directions.

(define all-king-moves
  (append castling-king-moves simple-king-moves)
  )
(define all-rook-moves
  (append white-castling-rook-moves black-castling-rook-moves simple-rook-moves)
  )

;;; Here all Movement in wikipedia have been checked with SDF_exercises/chapter_4/4_25.scm addition about En passant etc.
