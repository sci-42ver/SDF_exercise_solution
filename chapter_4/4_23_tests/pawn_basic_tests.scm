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
       'connect! 0 (make-piece type color #t #f))
      ;; added
      (set! piece-positions (cons (make-address col row) piece-positions))
      )

    ; (do-column 0 'rook)
    ; (do-column 1 'knight)
    ; (do-column 2 'bishop)
    ; (do-column 3 'queen)
    (do-column 4 'king)
    ; (do-column 5 'bishop)
    ; (do-column 6 'knight)
    ; (do-column 7 'rook)
    (for-each (lambda (col) (add-piece col pawn-row 'pawn)) chess-board-indices)
    )
  
  ;; to ensure initial if populate-sides is called twice.
  (set! piece-positions '())

  (populate-side 'white 0 1)
  (populate-side 'black 7 6))

(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_tests/")
(load "common_lib.scm")
(start-chess-game populate-sides*)

(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "test_lib.scm")

;;; The above is same as en_passant_tests.scm except that we just only keep pawn and king (king is to ensure that the game can be played).
(cd "~/SICP_SDF/SDF_exercises/chapter_4/")
(load "4_23_tests/move_helper_lib.scm")
;;; test for the "opponent" normal case
(chess-move-forward-two-steps '(0 1))
(chess-move-forward-two-steps '(6 1))
(chess-move '(0 3) '(1 4))

;;; test for the "opponent" failure
(chess-move-forward-two-steps '(0 1))
(chess-move-forward-two-steps '(2 1))
(chess-move-forward-two-steps '(1 1))
(trace-wrapper (lambda () (chess-move '(2 3) '(1 4))) opponent)

;;; basic-pawn-moves only needs tests for occupied-by-and-initial which is already done in SDF_exercises/chapter_4/4_23_tests/tests_for_castling_and_the_rest.scm.

