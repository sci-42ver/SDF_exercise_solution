;; from test-checkers.scm
(define (make-piece-type type color column row)
  (make-piece color type (make-coords column row)))

(define (assert-board expected-color expected-pieces board)
  (assert-equal expected-color (current-color board))
  (assert-lset= piece=? expected-pieces (board-pieces board)))

(define (assert-moves summarize-move-proc index expected-moves board)
  (let ((moves (generate-legal-moves board)))
    (assert-lset= equal? expected-moves (filter-map summarize-move-proc moves))
    ;; board-end-turn changes the color
    (get-final-board
      (find (lambda (move)
              (equal? (list-ref expected-moves index) (summarize-move-proc move)))
            moves))))

(define (make-chess* initial-pieces-generator moves-generator)
  (make-game max-row max-col '(black white) '(King	Queen	Rook	Bishop	Knight	Pawn)
             initial-pieces-generator
             moves-generator
             chess-piece-summary)
  )

(define (inherit-rules child parent)
  ;; must need these.
  (%set-aggregate-rules! child (%get-aggregate-rules parent))
  (%set-evolution-rules! child (%get-evolution-rules parent)))

(define (summarize-move-with-type move)
  (let loop
    ((coords-with-type-list
       (map (lambda (change)
              (let ((piece (get-piece change)))
                (list (piece-type piece) (piece-coords piece) (get-flags change))))
            ;; reverse first.
            (pmove->list move))))
    (if (pair? coords-with-type-list)
      ;; Here we don't need condense.
      (cons (car coords-with-type-list) (loop (cdr coords-with-type-list)))
      '())))

(define (summarize-move-checking-type type move)
  (let ((reverse-move (pmove->list move))) ; reverse first.
    (and (eq? type (piece-type (get-piece (car reverse-move))))
         (let 
           loop
           ((coords-with-type-list
              (map (lambda (change)
                     (let ((piece (get-piece change)))
                       (list (piece-type piece) (piece-coords piece) (get-flags change))))
                   reverse-move)))
           (if (pair? coords-with-type-list)
             ;; Here we don't need condense.
             (cons (car coords-with-type-list) (loop (cdr coords-with-type-list)))
             '())))))
