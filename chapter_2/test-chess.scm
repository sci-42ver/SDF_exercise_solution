(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'abstracting-a-domain:factoring)

(load "chess-lib/chess-cfg.scm")
(load "chess-lib/chess-factoring.scm")
(load "chess-lib/coords-complement.scm")
(load "chess-lib/chess-rule-utils.scm")
(load "2_12.scm")

(load "chess-lib/piece-complement.scm")
(load "2_13.scm")

(load "~/SICP_SDF/SDF_exercises/software/sdf/common/testing.scm")
(load "chess-lib/test-chess-utils.scm")

(load "utils.scm")
(define-test 'chess
  (lambda ()
    (let ((board (make-board chess)))
      (assert-board 'black
        (append 
          (list (make-piece-type 'Rook 'black 0 0) (make-piece-type 'Knight 'black 1 0)
              (make-piece-type 'Bishop 'black 2 0) (make-piece-type 'King 'black 3 0)
              (make-piece-type 'Queen 'black 4 0) (make-piece-type 'Bishop 'black 5 0)
              (make-piece-type 'Knight 'black 6 0) (make-piece-type 'Rook 'black 7 0))
          (map (lambda (col) (make-piece-type 'Pawn 'black col 1)) (iota 8))
          (list (make-piece-type 'Rook 'white 0 0) (make-piece-type 'Knight 'white 1 0)
              (make-piece-type 'Bishop 'white 2 0) (make-piece-type 'Queen 'white 3 0)
              (make-piece-type 'King 'white 4 0) (make-piece-type 'Bishop 'white 5 0)
              (make-piece-type 'Knight 'white 6 0) (make-piece-type 'Rook 'white 7 0))
          (map (lambda (col) (make-piece-type 'Pawn 'white col 1)) (iota 8)))
        board)
      (displayln "possible pmoves:")
      (displayln (map summarize-move (generate-legal-moves board)))
      (assert-moves 
        summarize-move
        1
        ;; test pawn "moving two squares straight forward" and knight without capture.
        '(((0 . 1) (0 . 3))
          ((0 . 1) (0 . 2))
          ((1 . 1) (1 . 3))
          ((1 . 1) (1 . 2))
          ((2 . 1) (2 . 3))
          ((2 . 1) (2 . 2))
          ((3 . 1) (3 . 3))
          ((3 . 1) (3 . 2))
          ((4 . 1) (4 . 3))
          ((4 . 1) (4 . 2))
          ((5 . 1) (5 . 3))
          ((5 . 1) (5 . 2))
          ((6 . 1) (6 . 3))
          ((6 . 1) (6 . 2))
          ((7 . 1) (7 . 3))
          ((7 . 1) (7 . 2))
          ((1 . 0) (2 . 2))
          ((1 . 0) (0 . 2))
          ((6 . 0) (7 . 2))
          ((6 . 0) (5 . 2)))
        board)
      )))

;; See https://lichess.org/editor/8/8/8/8/4np2/3q2P1/5k1p/8_w_-_-_0_1?color=black for the following chess board layout.
(define (pawn-promotion-initial-pieces game)
  (list
    (make-piece 'black 'Pawn (make-coords 0 6)) ; test promotion
    ))
(define pawn-promotion-chess
  (make-chess* pawn-promotion-initial-pieces generate-moves-using-rule-interpreter))
(inherit-rules pawn-promotion-chess chess)

(define-test 'pawn-promotion-chess
  (lambda ()
    (let ((board (make-board pawn-promotion-chess)))
      ; (displayln (generate-legal-moves board))
      (displayln (map summarize-move-with-type (generate-legal-moves board)))
      (assert-moves
        summarize-move-with-type
        1
        '(((pawn (0 . 6) ()) (pawn (0 . 7) ()) (pawn (0 . 7) (move-is-finished)) (queen (0 . 7) (move-is-finished)))
            ((pawn (0 . 6) ()) (pawn (0 . 7) ()) (pawn (0 . 7) (move-is-finished)) (rook (0 . 7) (move-is-finished)))
            ((pawn (0 . 6) ()) (pawn (0 . 7) ()) (pawn (0 . 7) (move-is-finished)) (bishop (0 . 7) (move-is-finished)))
            ((pawn (0 . 6) ()) (pawn (0 . 7) ()) (pawn (0 . 7) (move-is-finished)) (knight (0 . 7) (move-is-finished))))
        board)
      )))

(define (capture-initial-pieces game)
  (list
    ;; test capture (all the following black can capture the white pawn)
    (make-piece 'white 'Pawn (make-coords 6 2))
    (make-piece 'black 'Pawn (make-coords 2 4))
    (make-piece 'black 'Knight (make-coords 3 4))
    (make-piece 'black 'Queen (make-coords 4 5))
    (make-piece 'black 'King (make-coords 2 6))
    ))
(define capture-chess
  (make-chess* capture-initial-pieces generate-moves-using-rule-interpreter))
(inherit-rules capture-chess chess)

(define-test 'capture-chess
  (lambda ()
    (let ((board (make-board capture-chess)))
      ; (displayln (generate-legal-moves board))
      (displayln (filter-map 
                    (lambda (move) (summarize-move-checking-type 'Pawn move)) 
                    (generate-legal-moves board)))
      (assert-moves
        (lambda (move) (summarize-move-checking-type 'Pawn move))
        1
        '(((pawn (2 . 4) ()) (pawn (2 . 4) ()) (pawn (2 . 4) (captures-pieces)) (pawn (1 . 5) (captures-pieces)) (pawn (1 . 5) (move-is-finished captures-pieces))) 
          ((pawn (2 . 4) ()) (pawn (2 . 5) ()) (pawn (2 . 5) (move-is-finished))))
        board)
      )))

(run-tests "" #t)