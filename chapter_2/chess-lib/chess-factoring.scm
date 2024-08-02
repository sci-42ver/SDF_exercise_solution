(define (make-chess moves-generator)
  ;; same as wikipedia color
  ;; > chess involves many types of pieces.
  (make-game max-row max-col '(black white) '(King	Queen	Rook	Bishop	Knight	Pawn)
             chess-initial-pieces
             moves-generator
             chess-piece-summary))

(define (chess-initial-pieces game)
  (append-map (lambda (color)
                (append (map (lambda (column) (make-piece color 'Pawn (make-coords column 1))) (iota max-col))
                        ;; use apply to ensure `append` works for list parameter.
                        (apply append (map 
                                        (lambda (type column) 
                                          (list (make-piece color type (make-coords column 0))
                                                (make-piece color type (make-coords (- max-col 1 column) 0))))
                                        '(Rook Knight Bishop)
                                        (iota 3)))
                        (if (eq? color 'black)
                            (list (make-piece color 'King (make-coords 3 0))
                              (make-piece color 'Queen (make-coords 4 0)))
                            (list (make-piece color 'Queen (make-coords 3 0))
                              (make-piece color 'King (make-coords 4 0))))))
              (game-colors game)))

(define (chess-piece-summary piece)
  ;; See `%delete-piece` the latter should not occur.
  (if piece
      (string (case (piece-color piece)
                ((black) #\B)
                ((white) #\W)
                (else (error "Unknown color:" piece)))
              (case (piece-type piece)
                ((King) #\K)
                ((Queen) #\q)
                ((Rook) #\r)
                ((Bishop) #\b)
                ((Knight) #\k)
                ((Pawn) #\p)
                (else (error "Unknown type:" piece))))
      "  "))

(define chess
  ;; notice moves are not accumulated for later retrieval. See test-checkers.scm where `(2 . 2)` occurs.
  (make-chess generate-moves-using-rule-interpreter))