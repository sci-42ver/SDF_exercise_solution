;; from test-checkers.scm
(define (make-piece-type type color column row)
  (make-piece color type (make-coords column row)))

(define (assert-board expected-color expected-pieces board)
  (assert-equal expected-color (current-color board))
  (assert-lset= piece=? expected-pieces (board-pieces board)))

(define (assert-moves index expected-moves board)
  (let ((moves (generate-legal-moves board)))
    (assert-lset= equal? expected-moves (map summarize-move moves))
    ;; board-end-turn changes the color
    (get-final-board
     (find (lambda (move)
             (equal? (list-ref expected-moves index) (summarize-move move)))
           moves))))