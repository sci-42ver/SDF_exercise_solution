(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'pattern-matching-on-graphs)

(load "4_23_graph_match_lib/simple_move_orig_save.scm")

(load "4_24_moves_with_intermediate_nodes_lib.scm")
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
(chess-move-orig '(4 1) '(4 3)) ; white
(chess-move-orig '(3 1) '(3 3))

;;; correspond to (assert (check-move-for-type 'queen '(0 0) '(1 1)))
;; succeed
(let ((from '(3 0)))
  (chess-move from (address-sum '(1 1) from)))
; (((? source-node #[compound-procedure 18]) (* northeast (?* intermediate-possible-nodes #[compound-procedure unoccupied])) northeast (? target-node #[compound-procedure maybe-opponent])) 
;   "returns" ((4 1) (5 2) (6 3)) "with dict" 
;   (dict (target-node #[graph-node "7,4"] ?) 
;     (intermediate-possible-nodes (#[graph-node "6,3"] #[graph-node "5,2"] #[graph-node "4,1"]) ?*) 
;     (source-node #[graph-node "3,0"] ?) (#[uninterned-symbol |G1|] #[<bundle> 16] ?)))

;;; correspond to (assert (check-move-for-type 'queen '(3 4) '(5 4)))
;; prepare to ensure maybe-opponent
(chess-move-orig '(5 1) '(5 2))
(chess-move-orig '(5 1) '(5 2))
(chess-move-orig '(6 1) '(6 2))
(chess-move-orig '(6 1) '(6 2))
(chess-move-orig '(4 1) '(4 2))
;; actual move
(let ((from '(4 1)))
  (chess-move from (address-sum '(2 0) from)))

;;; correspond to (assert-not (check-move-for-type 'queen '(3 4) '(5 5)))
;; prepare for the possible queen move
(chess-move-orig '(5 2) '(5 3))
(chess-move-orig '(5 2) '(5 3))
;; fail* due to no valid path
(let ((from '(4 0)))
  (chess-move from (address-sum '(2 1) from)))
;; for black, this is from (3 7) to (1 6)
;; the closest is (1 5)
; (((? source-node #[compound-procedure 18]) (* northeast (?* intermediate-possible-nodes #[compound-procedure unoccupied])) northeast (? target-node #[compound-procedure maybe-opponent])) 
;   "returns" #f "with dict" 
;   (dict (target-node #[graph-node "0,4"] ?) 
;     (intermediate-possible-nodes (#[graph-node "1,5"] #[graph-node "2,6"]) ?*) 
;     (source-node #[graph-node "3,7"] ?) (#[uninterned-symbol |G1|] #[<bundle> 27] ?)))
;; res
; ("invalid move" (queen black) (4 0) (6 1))
;; correct
; (chess-move '(4 0) '(6 2))

;; IGNORE: fail due to maybe-opponent
;; '(3 1) has been moved before.
; (chess-move '(1 0) '(3 1))

;; to prepare for "white move"
(chess-move-orig '(0 1) '(0 3))
;; white move
;; succeed (both white and black are fine due to knight pos's are same for them.)
(chess-move '(1 0) '(2 2))

;;; correspond to (assert (check-move-for-type 'knight '(3 4) '(5 5)))
;; redundant move to ensure the expected color of the next turn.
; (chess-move-orig '(1 1) '(1 3))
(chess-move-orig '(3 3) '(2 4)) ; pawn capture
(chess-move-orig '(4 3) '(4 4))
(chess-move-orig '(2 4) '(2 5))
;; IGNORE: fail due to maybe-opponent
;; '(3 1) has been moved before.
; (chess-move '(1 0) '(3 1))
(let ((from '(2 2)))
  (chess-move from (address-sum '(2 1) from)))

;; to prepare for black-move test
; (chess-move-orig '(0 1) '(0 3))

;;; black-move test
;; fail* due to no valid path
(chess-move '(1 0) '(1 2))
; ("invalid move" (knight black) (1 0) (1 2))
;; won't match the close '(2 2). (Here "<bundle> 19" may differ based on the execution steps)
; (((? source-node #[compound-procedure 14]) north (?) north (?) east (? target-node #[compound-procedure maybe-opponent])) "returns" #f "with dict" (dict (target-node #[graph-node "5,5"] ?) (source-node #[graph-node "6,7"] ?) (#[uninterned-symbol |G1|] #[<bundle> 19] ?)))

;;; correspond to (assert-not (check-move-for-type 'knight '(3 4) '(5 4)))
(chess-move-orig '(1 1) '(1 2))
;; 0. fail* due to no valid path
;; 1. white
(let ((from '(4 3)))
  (chess-move from (address-sum '(2 0) from)))
;; IGNORE: i.e. from "3,4" to "1,4" not equal to closest correct one "1,3"
;; from "4,3" to "6,3" not equal to closest correct one "6,4"
; (((? source-node #[compound-procedure 14]) east (?) east (?) north (? target-node #[compound-procedure maybe-opponent])) "returns" #f "with dict" (dict (target-node #[graph-node "6,4"] ?) (source-node #[graph-node "4,3"] ?) (#[uninterned-symbol |G1|] #[<bundle> 24] ?)))

;;; The above expects to have 2 errors due to "fail*" number.