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

    (do-column 0 'rook)
    ; (do-column 1 'knight)
    ; (do-column 2 'bishop)
    (do-column 3 'queen)
    (do-column 4 'king)
    ; (do-column 5 'bishop)
    ; (do-column 6 'knight)
    (do-column 7 'rook)
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

;;; The above is same as king_check_tests.scm except adding pawns.
(chess-move '(0 1) '(0 3))
(chess-move '(0 1) '(0 3))

(chess-move '(0 3) '(0 4))
(chess-move '(6 1) '(6 3))

(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_tests_for_not_demanded_features")
(load "sublist_lib.scm")
(assert (sublist? en-passant-moves all-pawn-moves))
; (write-line all-pawn-moves)

;;; test for normal case
(chess-move '(0 4) '(1 5))

;;; test for "just-now"
(chess-move '(2 1) '(2 3))
(chess-move '(6 1) '(6 3))
(chess-move '(4 1) '(4 2))
(chess-move '(6 3) '(6 4))
(chess-move '(7 1) '(7 2))
;; piece-advance-two-initially-just-now pred failure.
; (chess-move '(6 4) '(7 5))
; ("occupied-by-opponent-pawn-advanced-two-initially-just-now" "place-node" #[graph-node "6,4"] "captured piece addr" (7 4) (pawn black #f #f))

;;; test for "piece-advance-two-initially"
(chess-move '(2 1) '(2 2))
(chess-move '(7 2) '(7 3))
(chess-move '(2 2) '(2 3))
;; no context of "has advance-two-initially-just-now from"

;;; Other sub-tests should be similar. Anyway detailed tests doesn't ensure correctness.
;; Anyway occupied-by-opponent-pawn-advanced-two-initially-just-now can detect the correct case.

