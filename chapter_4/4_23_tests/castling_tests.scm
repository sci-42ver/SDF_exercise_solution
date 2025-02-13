(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'pattern-matching-on-graphs)

(load "4_23_lib_corrected.scm")

(define (populate-sides* board)

  (define (populate-side color home-row pawn-row)

    (define (do-column col type)
      (add-piece col home-row type)
      ; (add-piece col pawn-row 'pawn)
      )

    (define (add-piece col row type)
      ;; see SDF_exercises/chapter_4/4_23_graph_match_lib/initial_piece_lib.scm
      ((board 'node-at (make-address col row))
       'connect! 0 (make-piece type color #t #f))
      ;; added
      (set! piece_positions (cons (make-address col row) piece_positions))
      )

    (do-column 0 'rook)
    ; (do-column 1 'knight)
    ; (do-column 2 'bishop)
    ; (do-column 3 'queen)
    (do-column 4 'king)
    (do-column 5 'bishop)
    ; (do-column 6 'knight)
    (do-column 7 'rook))

  (populate-side 'white 0 1)
  (populate-side 'black 7 6))

(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_tests/")
(load "common_lib.scm")
(start-chess-game populate-sides*)

(chess-move* '(4 0) '(0 0))
(chess-move* '(3 0) '(0 0))
; ("invalid move for (king rook)" ((3 0) (0 0)) "with" #f #f)
;; both can't meet "unoccupied" pred.
; (pp (list all-bishop-moves-from-code-base all-moves))
; (trace check-move-for-type)
; (trace %simple-move)
(chess-move '(2 0) '(1 1))
;; 0. the 3rd match-* call doesn't have northwest edge, so succeed of matcher and match-* both returns #f.
;; 1. the 2nd returns #f for matcher as the 3rd implies.
;; Then (succeed object dict) will call match-rest which binds target-node 
;; (gmatch:compile-edge->gmatch:compile-target->gmatch:compile-var->gmatch:var-matcher->(succeed object dict*)).
;; 1.a. succeed is only offered by graph-match, match-*, match-seq, gmatch:and.
;; Here we have finished match-* and at the end of match-seq, so we call succeed by graph-match, i.e. return dict.
; [Entering #[compound-procedure match-*]
;     Args: #[graph-node "5,7"]
;           (dict (source-node #[graph-node "5,7"] ?) (#[uninterned-symbol |G1|] #[<bundle> 34] ?))
;           #[compound-procedure 38]]
; [Entering #[compound-procedure match-*]
;     Args: #[graph-node "6,6"]
;           (dict (source-node #[graph-node "5,7"] ?) (#[uninterned-symbol |G1|] #[<bundle> 34] ?))
;           #[compound-procedure 38]]
; [Entering #[compound-procedure match-*]
;     Args: #[graph-node "7,5"]
;           (dict (source-node #[graph-node "5,7"] ?) (#[uninterned-symbol |G1|] #[<bundle> 34] ?))
;           #[compound-procedure 38]]
; [#f
;       <== #[compound-procedure match-*]
;     Args: #[graph-node "7,5"]
;           (dict (source-node #[graph-node "5,7"] ?) (#[uninterned-symbol |G1|] #[<bundle> 34] ?))
;           #[compound-procedure 38]]
; [(dict (target-node #[graph-node "7,5"] ?) (source-node #[graph-node "5,7"] ?) (#[uninterned-symbol |G1|] #[<bundle> 34] ?))
;       <== #[compound-procedure match-*]
;     Args: #[graph-node "6,6"]
;           (dict (source-node #[graph-node "5,7"] ?) (#[uninterned-symbol |G1|] #[<bundle> 34] ?))
;           #[compound-procedure 38]]
; [(dict (target-node #[graph-node "7,5"] ?) (source-node #[graph-node "5,7"] ?) (#[uninterned-symbol |G1|] #[<bundle> 34] ?))
;       <== #[compound-procedure match-*]
;     Args: #[graph-node "5,7"]
;           (dict (source-node #[graph-node "5,7"] ?) (#[uninterned-symbol |G1|] #[<bundle> 34] ?))
;           #[compound-procedure 38]]
;; Here "7,5" is at southeast (rotate-180-view of northwest) of "5,7".
; (((? source-node #[compound-procedure 25]) (* northwest (?* #[compound-procedure unoccupied])) northwest (? target-node #[compound-procedure maybe-opponent])) 
;   "returns" #f "with dict" 
;   (dict (target-node #[graph-node "7,5"] ?) (source-node #[graph-node "5,7"] ?) (#[uninterned-symbol |G1|] #[<bundle> 34] ?)))

(the-board 'piece-at '(6 6))
;; prepare for checking of king initial.
; (trace occupied-by-and-initial)
(chess-move '(2 0) '(2 1)) ; white move
(the-board 'piece-at '(6 6))
; (trace invert-address)
(chess-move* '(3 0) '(0 0)) ; black

