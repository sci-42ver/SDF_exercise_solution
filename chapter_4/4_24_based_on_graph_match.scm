(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'pattern-matching-on-graphs)

(load "4_23_graph_match_lib/simple_move_orig_save.scm")
(load "4_24_based_on_graph_match_lib.scm")

(define (simple-move board from to)
  (let ((my-piece (get-piece-to-move board from)))
    ;; For debug
    (write-line (list "move" (board 'color) (piece-type my-piece) "piece" "at" from))
    ;; added
    (if (not (check-move-for-type board from to))
      (error (list "invalid move" my-piece from to)))
    ;; not needed at least for SDF_exercises/software/sdf/pattern-matching-on-graphs/chess-moves.scm
    ;; since all moves have (? target-node ,maybe-opponent) which has already checked no-piece-or-opponent?.
    ; ;; A bunch of checks for validity of move:
    ; (let ((captured (board 'piece-at to)))
    ;   (if (not (no-piece-or-opponent? captured my-piece))
    ;       (error "Can't capture piece of same color:" captured)))
    ;; The move looks good; make it so:
    (board 'set-piece-at to my-piece)
    ;; Now update all the unaffected pieces to the next state of
    ;; the board:
    ;;; i.e. just update those pieces to exist in the next turn.
    (for-each (lambda (address)
                (if (not (or (address= from address)
                             (address= to address)))
                    (let ((p (board 'piece-at address)))
                      (if p
                          (board 'set-piece-at address p)))))
              board-addresses)
    (board 'next-turn)))

;; similar tests as SDF_exercises/chapter_4/4_24.scm
(start-chess-game)
(load "graph_match_lib/addr_lib.scm")
;; fail due to not moving the pawn.
; (let ((from '(3 0)))
;   (chess-move from (address-sum '(1 1) from)))
(chess-move-orig '(4 1) '(4 3))
(chess-move-orig '(3 1) '(3 3))

;; succeed
; (let ((from '(3 0)))
;   (chess-move from (address-sum '(1 1) from)))
;; (assert (check-move-for-type 'queen '(3 4) '(5 4)))
;; is similar

;; fail due to no valid path
; (chess-move '(3 0) '(5 1))
;; res
; ("invalid move" (queen white) (3 0) (5 1))

;; fail due to maybe-opponent
; (chess-move '(1 0) '(3 1))
;; succeed
(chess-move '(1 0) '(2 2))

;;; black-move test
;; fail due to no valid path
(chess-move '(1 0) '(1 2))
; ("invalid move" (knight black) (1 0) (1 2))
;; won't match the close '(2 2).
; (((? source-node #[compound-procedure 14]) north (?) north (?) east (? target-node #[compound-procedure maybe-opponent])) "returns" #f "with dict" (dict (target-node #[graph-node "5,5"] ?) (source-node #[graph-node "6,7"] ?) (#[uninterned-symbol |G1|] #[<bundle> 19] ?)))
