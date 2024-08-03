(define (chess-capture pos pmove)
  (finish-move 
    (new-piece-position pos
      ;; will add 2 changes (one to remove piece one to add flag)
      (capture-piece-at pos pmove))))