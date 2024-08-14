;; > Another difference is that capture is by displacement rather than jump.
;; so both positions are `landing`.
(define (chess-capture pos pmove)
  (finish-move 
    ;; > replaces it on its square
    (new-piece-position pos
                        ;; will add 2 changes (one to remove piece one to add flag)
                        ;; > The captured piece is thereby permanently removed from the game.
                        (capture-piece-at pos pmove))))
