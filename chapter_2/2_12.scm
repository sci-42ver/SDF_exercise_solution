;; I checked these 2 rules in https://en.wikipedia.org/wiki/Rules_of_chess#Basic_moves

;; bishop and queen are similar.
(define-evolution-rule 'rook-move chess
                       (lambda (pmove)
                         ;; Since Castling is not considered, only "Promotion" and "capture" can have 2 changes.
                         ;; The former is in "aggregate" while the latter is inside evolution.
                         ;; So here we won't meet with the multiple evolution case as in checkers *continuous* jump.
                         ;; So we don't need to check `is-pmove-empty?`.

                         ;; Notice the above holds for all pieces in chess.
                         (if (eq? 'Rook (piece-type (current-piece pmove)))
                           (get-direct-moves pmove) ; may return one list
                           '())))

;; See chebert which uses `is-pmove-finished?` to manipulate the following iteration. That is better.
(define (get-direct-moves pmove)
  (apply 
    append
    (filter-map
      (lambda (direction)
        ;; > One difference is that the range of motion of rooks, bishops, and queens is limited *only by obstruction*.
        ;; So we will continue proceed by `loop` until meeting with "obstruction".
        ;; Here we don't know where we may meet opponent (i.e. where to end the proceeding), so append-map can't be easily used.
        (let loop ((step-dist 1)
                   (res '()))
          (let ((landing
                  (compute-new-position direction step-dist pmove))
                ;; board is updated in test with `set!`
                (board (current-board pmove)))
            ;; avoid using explicit #f
            (if (is-position-on-board? landing board)
              (if (is-position-unoccupied? landing board)
                (loop (+ step-dist 1) (cons (finish-move (new-piece-position landing pmove)) res))
                ;; 1. > A piece moves to a vacant square except when capturing an opponent's piece.
                ;; 2. > pieces cannot jump over other pieces.
                ;; So we won't continue loop if encountering one pos occupied.
                ;; 3. Notice here we don't use and as others since we need to keep history accumulated pmoves.
                (if (is-position-occupied-by-non-king-opponent? landing board)
                  (cons (chess-capture landing pmove) res)
                  (non-null-lst res)))
              (non-null-lst res)))
          ))
      (possible-directions (current-piece pmove)))))

(define-evolution-rule 'knight-move chess
                       (lambda (pmove)
                         (if (eq? 'Knight (piece-type (current-piece pmove)))
                           (get-knight-moves pmove) ; may return one list
                           '())))

(define (get-knight-moves pmove)
  ;; here knight direction may be not general, so not define it in coords.scm.
  (define knight-directions 
    (let ((col-1-steps (append-map (lambda (col) 
                                     (list (make-coords col -2) 
                                           (make-coords col 2))) 
                                   '(1 -1))))
      ;; here we can't use reverse for pair. https://stackoverflow.com/a/23585211/21294350
      (append col-1-steps (map (lambda (e) (cons (cdr e) (car e))) col-1-steps))))
  (filter-map
    (lambda (direction)
      (let ((landing
              (compute-new-position direction 1 pmove))
            (board (current-board pmove)))
        ;; > The knight is not blocked by other pieces; it jumps to the new location.
        ;; > Except for any move of the knight and castling, pieces cannot jump over other pieces.
        ;; So we won't check whether passed positions are occupied, etc.
        (and (is-position-on-board? landing board)
             (if (is-position-unoccupied? landing board)
               (finish-move (new-piece-position landing pmove))
               (and (is-position-occupied-by-non-king-opponent? landing board)
                    (chess-capture landing pmove))))
        ))
    knight-directions))
