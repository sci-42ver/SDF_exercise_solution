(define-evolution-rule 'pawn-move chess
  (lambda (pmove)
    (if (eq? 'Pawn (piece-type (current-piece pmove)))
        (get-pawn-moves pmove) ; may return one list
        '())))

(define (get-pawn-moves pmove)
  (let* ((cur-piece (current-piece pmove))
         (cur-piece-coords (piece-coords cur-piece))
         (cur-piece-row (get-row cur-piece-coords))
         ;; promotion when 7 but it should be manipulated in "aggregate-rules".
         (move-directions (list forward-direction)))
         ;; > A pawn can capture an enemy piece on either of the two squares diagonally in front of the pawn.
         
         ;; > It cannot move to those squares when vacant except when capturing en passant.
         ;; So I use 2 directions
         (capture-directions forward-diagonal-directions)
    ;; Since pawn rule may be not generalized to others. So here I won't give 2 funcs for move and capture.
    (append
      (filter-map
        (lambda (direction)
          (let ((landing
                  (compute-new-position direction 1 pmove))
                (board (current-board pmove)))
            ;; similar to `get-jumps`.
            (and (is-position-on-board? landing board)
              (is-position-occupied-by-opponent? landing
                                               board)
              (capture-piece-at landing
                                (new-piece-position landing
                                                    pmove))))
        capture-directions)
      (filter-map
        (lambda (direction)
          (let loop ((step-dist 1)
                    (res '()))
            (let ((landing
                    (compute-new-position direction step-dist pmove))
                  (board (current-board pmove)))
            ; (if (and (is-position-on-board? landing board)
            ;       (is-position-unoccupied? landing board))
            ;   ;; very similar to `get-direct-moves`. TODO refactor to make one general func.
            ;   (begin
            ;     ;; > If it has not yet moved
            ;     ;; This check is based on
            ;     ;; > Pawns cannot move backwards.
            ;     (if (= cur-piece-row 1)
            ;       ;; > provided both squares are vacant
            ;       (loop (+ step-dist 1) (append (finish-move (new-piece-position landing pmove)) res))
            ;       (append (finish-move (new-piece-position landing pmove)) res)))
            ;   #f)

            (and (is-position-on-board? landing board)
              (is-position-unoccupied? landing board)
              ;; very similar to `get-direct-moves`. TODO refactor to make one general func.
              (lambda () 
                ;; > If it has not yet moved
                ;; This check is based on
                ;; > Pawns cannot move backwards.
                (if (= cur-piece-row 1)
                  ;; > provided both squares are vacant
                  (loop (+ step-dist 1) (append (finish-move (new-piece-position landing pmove)) res))
                  (append (finish-move (new-piece-position landing pmove)) res))))
              ))
        move-directions))))))

;; almost same as coronation.
(define-aggregate-rule 'promotion chess
  (lambda (pmoves)
    (append-map (lambda (pmove)
                  (let* ((cur-piece (current-piece pmove))
                        (cur-piece-row (get-row (piece-coords cur-piece))))
                    (if (and (= 7 cur-piece-row) (eq? 'Pawn (piece-type cur-piece)))
                      (map (lambda (type) 
                            (update-piece-type 
                              (lambda (piece type) (piece-new-type piece type)) pmove type))
                          '(Queen	Rook	Bishop	Knight))
                      (list pmove))))
      pmoves)))

;; almost same as rook
(define-evolution-rule 'bishop-move chess
  (lambda (pmove)
    (if (eq? 'Bishop (piece-type (current-piece pmove)))
        (get-direct-moves pmove) ; may return one list
        '())))
(define-evolution-rule 'queen-move chess
  (lambda (pmove)
    (if (eq? 'Queen (piece-type (current-piece pmove)))
        (get-direct-moves pmove) ; may return one list
        '())))

;; similar to `get-simple-moves`.
(define-evolution-rule 'king-move chess
  (lambda (pmove)
    (if (eq? 'King (piece-type (current-piece pmove)))
        (get-simple-moves-capture pmove) ; may return one list
        '())))

(define (get-simple-moves-capture pmove)
  (filter-map
   (lambda (direction)
     (let ((landing
            (compute-new-position direction 1 pmove))
           (board (current-board pmove)))
       (and (is-position-on-board? landing board)
          (if (is-position-unoccupied? landing board)
            (finish-move (new-piece-position landing pmove))
            (and (is-position-occupied-by-opponent? landing board)
              (capture-piece-at landing
                                (new-piece-position landing
                                                    pmove)))))))
   (possible-directions (current-piece pmove))))

;; Here I won't define `require-captures` similar to `require-jumps` 
;; since in real chess games what we want is not capture as more as possible 
;; but make "king" in checkmate although the former may imply the latter.