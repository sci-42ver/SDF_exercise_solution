;; IGNORE: address (from node), type, color (from board) can decide whether initial.
;; king, rook can move backwards, so we need to use one edge to show whether initial.
(define (occupied-by-and-initial type)
  (lambda (place-node dict) 
    (let ((piece (piece-in place-node dict)))
      (and piece
           (eq? type (piece-type piece)))
           ;; added
           (piece-initial-mark piece)
           ))
  )

;;; Here initial should be done on piece.
;; But (type color) can't decide piece and (make-piece type color) instance may be changed by (change-piece-type piece type).
;; So we can't use one table keyed by either of the above 2 things to track whether initial.
;; So we add one base-data to `make-piece` API.

;; IGNORE
; (define (mark-initial parameters)
;   (let ((node (board 'node-at address)))
;     (node 'connect! 'address address)
    
;     ))
; (define (make-chess-board)
;   (let ((board (make-chess-board-internal)))
;     (for-each (lambda (address)
;                 (connect-up-square address board)
;                 ;; added
;                 (mark-initial address board)
;                 )
;               board-addresses)
;     (populate-sides board)
;     board))

;; Here advance-two-initially-just-now should be only checked for pawn piece.
;; For simplicity, all are assumed to have the same make-piece API instead of marking advance-two-initially-just-now as #!optional.
(define (make-piece type color initial-mark advance-two-initially-just-now)
  (guarantee piece-type? type 'make-piece)
  (guarantee piece-color? color 'make-piece)
  (guarantee boolean? initial-mark 'make-piece)
  ;; https://softwareengineering.stackexchange.com/a/409460
  ;; here we must advance squares instead of other things
  ;; so no need for advance-two-squares-initial-just-now
  (guarantee boolean? advance-two-initially-just-now 'make-piece)
  (list type color initial-mark advance-two-initially-just-now))
(define piece-initial-mark caddr)
(define piece-advance-two-initially-just-now cadddr)

;; routine compatibility modification
(define (populate-sides board)

  (define (populate-side color home-row pawn-row)

    (define (do-column col type)
      (add-piece col home-row type)
      (add-piece col pawn-row 'pawn))

    (define (add-piece col row type)
      ((board 'node-at (make-address col row))
       'connect! 0 (make-piece type color #t #f)))

    (do-column 0 'rook)
    (do-column 1 'knight)
    (do-column 2 'bishop)
    (do-column 3 'queen)
    (do-column 4 'king)
    (do-column 5 'bishop)
    (do-column 6 'knight)
    (do-column 7 'rook))

  (populate-side 'white 0 1)
  (populate-side 'black 7 6))

(define (change-piece-type piece type)
  (make-piece 
    type 
    (piece-color piece) 
    (piece-initial-mark piece) 
    (piece-advance-two-initially-just-now piece)))

;; update helpers
(define (untick-piece-initial-mark piece)
  (if (piece-initial-mark piece) 
    piece
    (make-piece 
      (piece-type piece)  
      (piece-color piece) 
      #f
      (piece-advance-two-initially-just-now piece))))

;; all moves are done by simple-move
;; so update initial-mark here when necessary.
(define (simple-move board from to)
  ;;; IGNORE: for simplicity we update initial-mark after (board 'next-turn)
  ;; This is fine 
  (let ((my-piece (get-piece-to-move board from)))
    ;; A bunch of checks for validity of move:
    (let ((captured (board 'piece-at to)))
      (if (not (no-piece-or-opponent? captured my-piece))
          (error "Can't capture piece of same color:" captured)))
    ;; The move looks good; make it so:
    (board 
      'set-piece-at 
      to 
      ;; For modularity, we don't do both in one procedure.
      (untick-piece-initial-mark 
        (tick-piece-advance-two-initially-just-now board my-piece from to))) ; modified
    ;; Now update all the unaffected pieces to the next state of
    ;; the board:
    ;;; i.e. just update those pieces to exist in the next turn.
    (for-each (lambda (address)
                (if (not (or (address= from address)
                             (address= to address)))
                    (let ((p (board 'piece-at address)))
                      (if p
                          (board 
                            'set-piece-at 
                            address 
                            ;; 0. Here it will only untick when set
                            ;; So assume we just set that piece.
                            ;; Then here we won't do operation for that piece.
                            ;; 0.a. In the next turn,
                            ;; opponent may either do simple-move or en-passant-move (this doesn't captures to pos, so need simple-move*)
                            ;; For the former, "immediately" condition is not met, so untick.
                            ;; For the latter, this piece will be removed.
                            (untick-piece-advance-two-initially-just-now p))))))
              board-addresses)
    ;; just update turn
    (board 'next-turn))
  )

(define (untick-piece-advance-two-initially-just-now piece)
  (if (piece-advance-two-initially-just-now piece)
    (make-piece 
      (piece-type piece)  
      (piece-color piece) 
      (piece-initial-mark piece)
      #f)
    piece))

(define (initial-pawn-advance-two? board from to)
  ;; > Pawns cannot move backwards.
  (n:= 1 (address-y (address-transform board from)))
  ;; assume piece have been checked whether it follows the valid path.
  (n:= 3 (address-y (address-transform board to)))
  )
(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "./graph_match_lib/addr_lib.scm")
(define (initial-pawn-advance-two?* piece from to)
  (and
    (piece-initial-mark piece)
    ;; based on the above assumption
    ((lambda (diff-y) (n:= 2 (abs diff-y)))
      (shift-y (address-diff from to)))
    )
  )

(define (tick-piece-advance-two-initially-just-now board piece from to)
  (if 
    (and 
      (eq? (piece-type piece) 'pawn)
      (initial-pawn-advance-two? board from to)
      ;; put here to show these pred's are same.
      (initial-pawn-advance-two?* piece from to)
      )
    (let ((initial-mark (piece-initial-mark piece)))
      (if (not initial-mark)
        (error "pawn has been moved wrongly")
        (make-piece 
          (piece-type piece)  
          (piece-color piece) 
          initial-mark
          #t)))
    (if (piece-advance-two-initially-just-now piece)
      (error "piece-advance-two-initially-just-now mark should be cleared/unticked at the opponent turn.")
      piece)))

