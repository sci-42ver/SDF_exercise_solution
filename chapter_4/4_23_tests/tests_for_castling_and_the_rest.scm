;;; also implicitly tests for SDF_exercises/chapter_4/4_23_graph_match_lib/initial_piece_lib.scm and SDF_exercises/chapter_4/4_23_graph_match_lib/check_lib.scm.

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
      (set! piece-positions (cons (make-address col row) piece-positions))
      )

    (do-column 0 'rook)
    ; (do-column 1 'knight)
    ; (do-column 2 'bishop)
    ; (do-column 3 'queen)
    (do-column 4 'king)
    (do-column 5 'bishop)
    ; (do-column 6 'knight)
    (do-column 7 'rook))
  
  ;; to ensure initial if populate-sides is called twice.
  (set! piece-positions '())

  (populate-side 'white 0 1)
  (populate-side 'black 7 6))

(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_tests/")
(load "common_lib.scm")
(start-chess-game populate-sides*)

;;; test for the normal case, initial
(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../common-lib/test_lib.scm")
(trace-wrapper (lambda () (chess-move* '(4 0) '(0 0))) mul-simple-move simple-move-moved-part get-self-positions untick-piece-initial-mark capture?*)
; [Entering #[compound-procedure untick-piece-initial-mark]
;     Args: (king white #t #f)]
; [(king white #f #f)
;       <== #[compound-procedure untick-piece-initial-mark]
;     Args: (king white #t #f)]
; [Entering #[compound-procedure untick-piece-initial-mark]
;     Args: (rook white #t #f)]
; [(rook white #f #f)
;       <== #[compound-procedure untick-piece-initial-mark]
;     Args: (rook white #t #f)]

; [Entering #[compound-procedure capture?*]
;     Args: #[<bundle> 15]
;           (0 0)
;           ((? source-node #[compound-procedure 14]) east (? #[compound-procedure unoccupied]) east (? #[compound-procedure unoccupied]) east (? target-node #[compound-procedure unoccupied...
;           #f]
; ("occupied-by-and-initial" rook #[graph-node "0,0"] (rook white #t #f) #t)
; [(capture (rook white #t #f) (3 0))
;       <== #[compound-procedure capture?*]
;     Args: #[<bundle> 15]
;           (0 0)
;           ((? source-node #[compound-procedure 14]) east (? #[compound-procedure unoccupied]) east (? #[compound-procedure unoccupied]) east (? target-node #[compound-procedure unoccupied...
;           #f]


;;;; Here I test for all preds (therefore their sub-preds) used in castling-king-moves and castling-rook-moves.
;;; test for unoccupied and king-castling-with_bl etc (The latter is dropped in the review later).
(trace unchecked)
(trace capture?*)
; (trace unoccupied)
;;; failure1
(chess-move* '(3 0) '(0 0))
(untrace unchecked)
(untrace capture?*)
; ("invalid move for (king rook)" ((3 0) (0 0)) "with" #f #f)
;; rook will test for king-castling-with_bl etc for source-node, if failure it won't check further.
;; so no more unoccupied tests for rook here.
; [Entering #[compound-procedure capture?*]
;     Args: #[<bundle> 30]
;           (0 0)
;           ((? source-node #[compound-procedure 21]) east (? #[compound-procedu...
;           #f]  
; ...
; ("unoccupied-and-unchecked" #[graph-node "5,7"] #f #t)
; ("unoccupied returns" #f "for" #[graph-node "5,7"])
; [#f
;       <== #[compound-procedure capture?*]
;     Args: #[<bundle> 30]
;           (0 0)
;           ((? source-node #[compound-procedure 21]) east (? #[compound-procedu...
;           #f] 
;; king test same as the above but called by capture?* for king.
; [Entering #[compound-procedure capture?*]
;     Args: #[<bundle> 30]
;           (3 0)
;           ((? source-node #[compound-procedure 13]) west (? #[compound-procedu...
;           #f]
; ...
; ("unoccupied-and-unchecked" #[graph-node "5,7"] #f #t)
; ("unoccupied returns" #f "for" #[graph-node "5,7"])
; [#f
;       <== #[compound-procedure capture?*]
;     Args: #[<bundle> 30]
;           (3 0)
;           ((? source-node #[compound-procedure 13]) west (? #[compound-procedu...                                                                                                            
;           #f]
;; both can't meet "unoccupied" pred.

; (pp (list all-bishop-moves-from-code-base all-moves))

;;; preparation for the correct move of (chess-move* '(3 0) '(0 0))
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
;;; prepare for checking of king initial.
; (trace occupied-by-and-initial)
(chess-move '(2 0) '(3 1)) ; white move

(the-board 'piece-at '(6 6))
; (trace invert-address)
(chess-move* '(3 0) '(0 0)) ; black

;;; test for initial
(chess-move '(3 1) '(4 0))
(let ((from '(1 1)))
  (chess-move from (address-sum '(1 1) from)))
(chess-move '(3 0) '(0 0))
(let ((from '(2 2)))
  (chess-move from (address-sum '(1 1) from)))
;;; failure2
(chess-move* '(4 0) '(0 0))
; ("occupied-by-and-initial" rook #[graph-node "0,0"] (rook white #f #f) #f)
; ("occupied-by-and-initial" king #[graph-node "4,0"] (king white #f #f) #f)
; ("invalid move for (king rook)" ((4 0) (0 0)) "with" #f #f)

;;; IGNORE (see SDF_exercises/chapter_4/4_23_graph_match_lib/castling_lib.scm): test for br-rook-initial etc

;;; test for white (checking for board color to choose the correct path) is skipped since that is done implicitly in castling-move

;;; test for bl?
;; occupied-by-and-initial for rook will only allow 4 pos's.
;; Color restriction then decreases this number to 2 (implied by allow-any-color param choice).
;; And then "whether-rook-left-pos" will just decreases this number to 1.
;; So bl? must be met if using castling-move abstraction.

;; So no need to write one test here (also for white pred etc). After all, only the analysis can guarantee the correctness as DMIA says.

;;; test for unchecked
(start-chess-game populate-sides*)
(chess-move* '(4 0) '(0 0))
;;; failure3
;; fail due to rook capture.
(chess-move* '(3 0) '(7 0))
;; (4 0) is captured
; ("captured-positions:" ((4 0) (4 6) (4 5) (4 4) (4 3) (4 2) (4 1) (3 7) (6 7) (6 6) (5 6) (1 7) (0 0) (0 6) (0 5) (0 4) (0 3) (0 2) (0 1)) "with board color:" black "opponent-positions:" ((4 7) (5 7) (0 7) (2 7)) "piece-positions:" ((3 0) (2 0) (7 7) (5 7) (4 7) (0 7) (7 0) (5 0)) "place-node:" #[graph-node "3,7"])
; ("unoccupied-and-unchecked" #[graph-node "3,7"] "unchecked-res" #f "unoccupied-res" #t)

;; 3 errors totally all thrown by "invalid move ...".
