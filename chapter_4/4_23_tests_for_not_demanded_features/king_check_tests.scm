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
    (do-column 7 'rook))
  
  ;; to ensure initial if populate-sides is called twice.
  (set! piece-positions '())

  (populate-side 'white 0 1)
  (populate-side 'black 7 6))

(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_tests/")
(load "common_lib.scm")
(start-chess-game populate-sides*)

(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "test_lib.scm")

;;; The above is same as castling_tests.scm except using queen which is more flexible.
(chess-move '(3 0) '(4 1))

;;; See SDF_exercises/chapter_4/4_23.scm
;;; > ... leaves one's king in check
;; black king must move.
(chess-move '(0 0) '(0 1))
;; Here rook captured-positions "(7 0) (7 6) (7 5) (7 4) (7 3) (7 2) (7 1)" are all shown.
; ("captured-positions-with-dup:" ((4 7) (7 6) (4 6) (5 6) (6 6) (7 2) (4 5) (5 4) (6 3) (3 0) (3 5) (3 4) (3 3) (3 2) (3 1) (0 3) (2 5) (1 4) (0 6) (2 6) (1 6) (2 7) (2 7) (1 7) (0 0) (0 6) (0 5) (0 4) (0 3) (0 2) (0 1) (4 7) (4 6) (2 6) (2 7) (7 0) (7 6) (7 5) (7 4) (7 3) (7 2) (7 1) (4 7) (6 7) (5 7)) "captured-positions:" ((4 7) (7 6) (4 6) (5 6) (6 6) (7 2) (4 5) (5 4) (6 3) (3 0) (3 5) (3 4) (3 3) (3 2) (3 1) (0 3) (2 5) (1 4) (0 6) (2 6) (1 6) (2 7) (1 7) (0 0) (0 5) (0 4) (0 2) (0 1) (7 0) (7 5) (7 4) (7 3) (7 1) (6 7) (5 7)) "with board color:" black "opponent-positions:" ((3 6) (0 7) (3 7) (7 7)) "piece-positions:" ((4 1) (7 7) (4 7) (3 7) (0 7) (7 0) (4 0) (0 0)) "place-node:" #[graph-node "4,7"])

;; > ... places ... one's king in check
(start-chess-game populate-sides*)
(chess-move '(3 0) '(5 2))
(chess-move '(3 0) '(2 0))
; ("king moved dest" (2 0) "is checked")
