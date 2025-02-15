(define (populate-sides board)

  (define (populate-side color home-row pawn-row)

    (define (do-column col type)
      (add-piece col home-row type)
      (add-piece col pawn-row 'pawn))

    (define (add-piece col row type)
      ;; from SDF_exercises/chapter_4/4_23_graph_match_lib/initial_piece_lib.scm
      ((board 'node-at (make-address col row))
       'connect! 0 (make-piece type color #t #f))
      ;; added
      (set! piece_positions (cons (make-address col row) piece_positions))
      )

    (do-column 0 'rook)
    (do-column 1 'knight)
    (do-column 2 'bishop)
    (do-column 3 'queen)
    (do-column 4 'king)
    (do-column 5 'bishop)
    (do-column 6 'knight)
    (do-column 7 'rook))

  ;; to ensure initial if populate-sides is called twice.
  (set! piece_positions '())

  (populate-side 'white 0 1)
  (populate-side 'black 7 6))