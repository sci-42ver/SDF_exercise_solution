;;; Only chebert of 4 possible solution repos have implemented this by searching "chess".
(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'abstracting-a-domain:factoring)

(load "chess-lib/chess-cfg.scm")
(load "chess-lib/chess-factoring.scm")
(load "chess-lib/coords-complement.scm")
(load "chess-lib/chess-rule-utils.scm")
(load "chess-lib/board-complement.scm")
(load "chess-lib/misc-utils.scm")
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
        ;; > moves straight forward one square
        ;; > moving two squares straight forward
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

;; See https://lichess.org/editor/8/1p6/8/1P6/4np2/3q2P1/3K1k1p/8_w_-_-_0_1?color=black for the following chess board layout.
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
    (make-piece 'white 'King (make-coords 3 1))
    (make-piece 'white 'Pawn (make-coords 1 4))
    (make-piece 'black 'Pawn (make-coords 6 1))
    ))
(define capture-chess
  (make-chess* capture-initial-pieces generate-moves-using-rule-interpreter))
(inherit-rules capture-chess chess)

(define-test 'capture-chess
  (lambda ()
    (let ((board (make-board capture-chess)))
      ; (displayln (generate-legal-moves board))
      (displayln (filter-map 
                    (lambda (move) (summarize-move-checking-type 'King move)) 
                    (generate-legal-moves board)))
      (assert-moves
        (lambda (move) (summarize-move-checking-type 'Pawn move))
        1
        ;; >  capture an enemy piece on either of the two squares diagonally
        '(((pawn (2 . 4) ()) (pawn (2 . 4) ()) (pawn (2 . 4) (captures-pieces)) (pawn (1 . 5) (captures-pieces)) (pawn (1 . 5) (move-is-finished captures-pieces))) 
          ((pawn (2 . 4) ()) (pawn (2 . 5) ()) (pawn (2 . 5) (move-is-finished))) 
          ((pawn (6 . 1) ()) (pawn (6 . 2) ()) (pawn (6 . 2) (move-is-finished))))
        board)
      (assert-moves
        (lambda (move) (summarize-move-checking-type 'Knight move))
        1
        '(((knight (3 . 4) ()) (knight (4 . 2) ()) (knight (4 . 2) (move-is-finished))) 
          ;; > The king can be put in check but cannot be captured
          ; ((knight (3 . 4) ()) (knight (4 . 6) ()) (knight (4 . 6) (move-is-finished))) 
          ((knight (3 . 4) ()) (knight (2 . 2) ()) (knight (2 . 2) (move-is-finished))) 
          ;; > The knight is not blocked by other pieces
          ((knight (3 . 4) ()) (knight (3 . 4) ()) (knight (3 . 4) (captures-pieces)) (knight (1 . 5) (captures-pieces)) (knight (1 . 5) (move-is-finished captures-pieces))) 
          ((knight (3 . 4) ()) (knight (5 . 5) ()) (knight (5 . 5) (move-is-finished))) 
          ((knight (3 . 4) ()) (knight (1 . 3) ()) (knight (1 . 3) (move-is-finished))) 
          ((knight (3 . 4) ()) (knight (5 . 3) ()) (knight (5 . 3) (move-is-finished))))
        board)
      (assert-moves
        (lambda (move) (summarize-move-checking-type 'Queen move))
        1
        '(;; (0 1) blocked by white king.
          ((queen (4 . 5) ()) (queen (4 . 0) ()) (queen (4 . 0) (move-is-finished))) 
          ((queen (4 . 5) ()) (queen (4 . 1) ()) (queen (4 . 1) (move-is-finished))) 
          ((queen (4 . 5) ()) (queen (4 . 2) ()) (queen (4 . 2) (move-is-finished))) 
          ((queen (4 . 5) ()) (queen (4 . 3) ()) (queen (4 . 3) (move-is-finished))) 
          ((queen (4 . 5) ()) (queen (4 . 4) ()) (queen (4 . 4) (move-is-finished)))

          ((queen (4 . 5) ()) (queen (4 . 5) ()) (queen (4 . 5) (captures-pieces)) (queen (1 . 5) (captures-pieces)) (queen (1 . 5) (move-is-finished captures-pieces))) 
          ((queen (4 . 5) ()) (queen (2 . 5) ()) (queen (2 . 5) (move-is-finished))) 
          ((queen (4 . 5) ()) (queen (3 . 5) ()) (queen (3 . 5) (move-is-finished)))

          ((queen (4 . 5) ()) (queen (7 . 5) ()) (queen (7 . 5) (move-is-finished))) 
          ((queen (4 . 5) ()) (queen (6 . 5) ()) (queen (6 . 5) (move-is-finished)))
          ((queen (4 . 5) ()) (queen (5 . 5) ()) (queen (5 . 5) (move-is-finished)))

          ((queen (4 . 5) ()) (queen (6 . 7) ()) (queen (6 . 7) (move-is-finished))) 
          ((queen (4 . 5) ()) (queen (5 . 6) ()) (queen (5 . 6) (move-is-finished)))

          ((queen (4 . 5) ()) (queen (2 . 7) ()) (queen (2 . 7) (move-is-finished))) 
          ((queen (4 . 5) ()) (queen (3 . 6) ()) (queen (3 . 6) (move-is-finished)))

          ((queen (4 . 5) ()) (queen (4 . 5) ()) (queen (4 . 5) (captures-pieces)) (queen (6 . 3) (captures-pieces)) (queen (6 . 3) (move-is-finished captures-pieces))) 
          ((queen (4 . 5) ()) (queen (5 . 4) ()) (queen (5 . 4) (move-is-finished)))
          ;; (-1 -1) blocked by black knight
          )
        board)
      (assert-moves
        (lambda (move) (summarize-move-checking-type 'King move))
        1
        '(((king (2 . 6) ()) (king (2 . 7) ()) (king (2 . 7) (move-is-finished))) 
          ((king (2 . 6) ()) (king (2 . 5) ()) (king (2 . 5) (move-is-finished))) 
          ((king (2 . 6) ()) (king (1 . 6) ()) (king (1 . 6) (move-is-finished))) 
          ((king (2 . 6) ()) (king (3 . 6) ()) (king (3 . 6) (move-is-finished))) 
          ((king (2 . 6) ()) (king (3 . 7) ()) (king (3 . 7) (move-is-finished))) 
          ((king (2 . 6) ()) (king (1 . 7) ()) (king (1 . 7) (move-is-finished))) 
          ((king (2 . 6) ()) (king (3 . 5) ()) (king (3 . 5) (move-is-finished))) 
          ((king (2 . 6) ()) (king (2 . 6) ()) (king (2 . 6) (captures-pieces)) (king (1 . 5) (captures-pieces)) (king (1 . 5) (move-is-finished captures-pieces))))
        board)
      )))

(run-tests "" #t)