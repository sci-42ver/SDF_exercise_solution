;;;; 0. provide occupied-by-and-initial which is the base for occupied-by-and-initial-and-black-and-br etc.
;;;; 1. Check all appropriate locations have used the correct "make-piece": In VSCode, "\(make-piece " with include "SDF_exercises/**/*.scm" and exclude "SDF_exercises/chapter_2,abstracting-a-domain".

;;; finished

;;;; needs SDF_exercises/chapter_4/4_23_graph_match_lib/base_lib.scm
(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_graph_match_lib/")
(load "common/base_lib.scm")

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
;; See SDF_exercises/chapter_4/4_23_graph_match_lib/combination/populate-sides.scm
; (define (populate-sides board)

;   (define (populate-side color home-row pawn-row)

;     (define (do-column col type)
;       (add-piece col home-row type)
;       (add-piece col pawn-row 'pawn))

;     (define (add-piece col row type)
;       ;; changed
;       ((board 'node-at (make-address col row))
;        'connect! 0 (make-piece type color #t #f)))

;     (do-column 0 'rook)
;     (do-column 1 'knight)
;     (do-column 2 'bishop)
;     (do-column 3 'queen)
;     (do-column 4 'king)
;     (do-column 5 'bishop)
;     (do-column 6 'knight)
;     (do-column 7 'rook))

;   (populate-side 'white 0 1)
;   (populate-side 'black 7 6))

(define (change-piece-type piece type)
  (make-piece 
    type 
    (piece-color piece) 
    (piece-initial-mark piece) 
    (piece-advance-two-initially-just-now piece)))

;; update helpers
(define (untick-piece-initial-mark piece)
  (if (piece-initial-mark piece)
    ;; I didn't check through the code base whether piece is used with eq?.
    ;; IMHO it just uses internal info's. If so, it is fine to always create one *new* piece. Otherwise we should not do that. 
    ;; Anyway, code base change-piece-type implies it is fine to "create one *new* piece".
    (make-piece 
      (piece-type piece)  
      (piece-color piece) 
      #f
      (piece-advance-two-initially-just-now piece))
    piece
    ))

;; 0. all moves are done by simple-move
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
      ;; changed
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
                            ;; changed
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

(load "common/board_lib.scm")
(define (initial-pawn-advance-two? board from to)
  ;; > Pawns cannot move backwards.
  (n:= 1 (address-y from))
  ;; assume piece have been checked whether it follows the valid path.
  (n:= 3 (address-y to))
  )
(load "../graph_match_lib/addr_lib.scm")
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
      (pawn_piece? piece)
      (initial-pawn-advance-two? board from to)
      ;; put here to show these pred's are same.
      (initial-pawn-advance-two?* piece from to)
      )
    (let ((initial-mark (piece-initial-mark piece)))
      (if (not initial-mark)
        (error "pred doesn't ensure initial-mark expectedly")
        (begin
          (write-line (list piece "has advance-two-initially-just-now from" from))
          (make-piece 
            (piece-type piece)  
            (piece-color piece) 
            initial-mark
            #t))))
    (if (piece-advance-two-initially-just-now piece)
      (error "piece-advance-two-initially-just-now mark should be cleared/unticked at the opponent turn.")
      piece)))

