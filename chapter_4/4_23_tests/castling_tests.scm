(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'pattern-matching-on-graphs)

(load "4_23_lib_corrected.scm")

(define (populate-sides* board)

  (define (populate-side color home-row pawn-row)

    (define (do-column col type)
      (add-piece col home-row type)
      ; (add-piece col pawn-row 'pawn)
      )

    (define (add-piece col row type)
      ;; see SDF_exercises/chapter_4/4_23_graph_match_lib/initial_piece_lib.scm
      ((board 'node-at (make-address col row))
       'connect! 0 (make-piece type color #t #f)))

    (do-column 0 'rook)
    ; (do-column 1 'knight)
    ; (do-column 2 'bishop)
    ; (do-column 3 'queen)
    (do-column 4 'king)
    (do-column 5 'bishop)
    ; (do-column 6 'knight)
    (do-column 7 'rook))

  (populate-side 'white 0 1)
  (populate-side 'black 7 6))

(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_tests/")
(load "common_lib.scm")
(start-chess-game populate-sides*)

(chess-move* '(4 0) '(0 0))
(chess-move* '(3 0) '(0 0))
; ("invalid move for (king rook)" ((3 0) (0 0)) "with" #f #f)
(chess-move '(2 0) '(1 1))

