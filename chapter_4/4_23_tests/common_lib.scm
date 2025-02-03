(define (make-chess-board populate-sides-proc)
  (let ((board (make-chess-board-internal)))
    (for-each (lambda (address)
                (connect-up-square address board))
              board-addresses)
    (populate-sides-proc board)
    board))

(define print-chess-board-orig print-chess-board)
(define (print-chess-board board)
  (newline)
  (print-chess-board-orig board)
  )
(define (start-chess-game populate-sides-proc)
  (set! the-board (make-chess-board populate-sides-proc))
  (print-chess-board the-board))
