(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'abstracting-a-domain:factoring)

(load "chess-cfg.scm")
(load "chess-factoring.scm")

;; I checked these 2 rules in https://en.wikipedia.org/wiki/Rules_of_chess#Basic_moves

;; bishop and queen are similar.
(define-evolution-rule 'rook-move chess
  (lambda (pmove)
    ;; Since Castling is not considered, only "Promotion" and "capture" can have 2 changes.
    ;; The former is in "aggregate" while the latter is inside evolution.
    ;; So here we won't meet with the multiple evolution case as in checkers continuous jump.
    ;; So we don't need to check `is-pmove-empty?`.
    (if (eq? 'Rook (piece-type (current-piece pmove)))
        (get-direct-moves pmove) ; may return one list
        '())))

(define (get-direct-moves pmove)
  (filter-map
   (lambda (direction)
    ;; Here we don't know where we may meet opponent (i.e. where to end the proceeding), so append-map can't be easily used.
     (let loop ((step-dist 1)
                (res '()))
      (let ((landing
              (compute-new-position direction step-dist pmove))
            ;; board is updated in test with `set!`
            (board (current-board pmove)))
       (if (is-position-on-board? landing board)
          (if (is-position-unoccupied? landing board)
            (loop (+ step-dist 1) (append (finish-move (new-piece-position landing pmove)) res))
            ;; > A piece moves to a vacant square except when capturing an opponent's piece.
            (if (is-position-occupied-by-opponent? landing board)
              ;; > The king can be put in check but cannot be captured.
              (if (eq? 'King (piece-type (board-get landing board)))
                #f
                ;; > pieces cannot jump over other pieces.
                ;; So we won't continue loop.
                (append (capture-piece-at landing
                              (new-piece-position landing
                                                  pmove)) res))
              #f))
          #f))))
   (possible-directions (current-piece pmove))))

(load "coords-complement.scm")

(define (possible-directions piece)
  (case (piece-type piece)
    ((Rook) cross-directions)
    ((Bishop) diagonal-directions)
    ((King Queen) all-directions)
    ((Pawn) forward-direction)))

(define-evolution-rule 'knight-move chess
  (lambda (pmove)
    ;; Since Castling is not considered, only "Promotion" and "capture" can have 2 changes.
    ;; The former is in "aggregate" while the latter is inside evolution.
    ;; So here we won't meet with the multiple evolution case as in checkers continuous jump.
    ;; So we don't need to check `is-pmove-empty?`.
    (if (eq? 'Knight (piece-type (current-piece pmove)))
        (get-knight-moves pmove) ; may return one list
        '())))

(define (get-direct-moves pmove)
  ;; here knight direction may be not general, so not define it in coords.scm.
  (define knight-directions 
    (let ((col-1-steps (map (lambda (col) 
                              (list (make-coords col -2) 
                                    (make-coords col 2))) 
                            '(1 -1))))
      (append col-1-steps (map reverse col-1-steps))))
  (filter-map
   (lambda (direction)
    (let ((landing
            (compute-new-position direction 1 pmove))
          (board (current-board pmove)))
      ;; > The knight is not blocked by other pieces; it jumps to the new location.
      ;; So we won't check whether passed positions are occupied, etc.
      (if (is-position-on-board? landing board)
        (if (is-position-unoccupied? landing board)
          (finish-move (new-piece-position landing pmove))
          (if (is-position-occupied-by-opponent? landing board)
            (capture-piece-at landing
              (new-piece-position landing
                                  pmove))
            #f))
        #f)))
   knight-directions))