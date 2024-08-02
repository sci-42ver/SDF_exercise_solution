(define (update-piece-type updater pmove piece-type)
  (let* ((change (final-change pmove))
         (piece (get-piece change))
         (piece* (updater piece piece-type)))
    ;; uses `update-board` to return one new board.
    (add-change (make-change (board-replace-piece (get-board change)
                                                  piece
                                                  piece*)
                             piece* ; one specific piece instead of all pieces.
                             (get-flags change))
                pmove)))