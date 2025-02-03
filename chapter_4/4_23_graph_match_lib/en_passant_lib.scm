;;; finished
(cd "~/SICP_SDF/SDF_exercises/chapter_4")
;; Load procedure: opponent
(load "4_23_graph_match_lib/common/base_lib.scm")

;; See capture? where graph-match uses node from (board 'node-at from)
;; So black place-node will automatically use the inverted direction.
(define (west/east-addr board direction place-node)
  ;; returns the adjacent node based on view (see SDF_exercises/software/sdf/pattern-matching-on-graphs/graph.scm (graph-node-view delegate view)->(all-edges)).
  ;; Then address-of gets the addr of the current view.
  ;; With piece-at (i.e. totally 2 invert-address's), we will actually get the correct piece.
  (and
    ;; see gmatch:compile-edge
    (place-node 'has-edge? direction)
    (board 'address-of (place-node 'edge-value direction)))
  )
(load "./4_23_graph_match_lib/initial_piece_lib.scm")
;; see SDF_exercises/chapter_4/4_25.scm
(define (opponent* piece board)
  ;; 0. similar to piece-is-opponent?
  ;; 1. assume my-piece is the same color as board.
  (not (eq? (piece-color piece)
            (board 'color))) 
  )

;; check whether place-node's direction is occupied-by-opponent-pawn-advanced-two-initially-just-now
(define (occupied-by-opponent-pawn-advanced-two-initially-just-now direction place-node dict)
  (let* ((board (chess-dict:board dict))
         (west/east-addr-data (west/east-addr board direction place-node))
         (west/east-piece (and west/east-addr-data (board 'piece-at west/east-addr-data))))
    (and
      west/east-piece
      ;; here we may not have source-node bound as the following shows.
      (opponent* west/east-piece board)
      (pawn_piece? west/east-piece)
      ;; This tag needs clear when moved after that "advances-two-squares"
      ;; and markded when "advances-two-squares".
      (piece-advance-two-initially-just-now west/east-piece)
      )
    )
  )

;; 0. See simple-move* in SDF_exercises/chapter_4/4_23_graph_match_lib/initial_lib.scm
;; For simplicity, here we only add the feature related with en_passant based on code base one.
;; For the whole one, see simple_move_mod.scm
;; 1. IGNORE: IMHO better to use (en-passant-move board from to) to let the user check the validity
(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "pred_lib.scm")
(define (en-passant-move? board from to)
  ; ;; just recheck what is done in en-passant-moves...
  ; (and
  ;   (pawn_piece? piece)
  ;   (address= (make-address 1 1) (map abs (address-diff to from)))
  ;   )
  ;; 0. similar to capture? and 4.24.
  ;; 1. See SDF_exercises/software/sdf/pattern-matching-on-graphs/graph-match.scm gmatch:seq2 comments: 
  ;; We only need to check whether dict is #f to check whether path is matched.
  ;; Here we need at least one non-false dict. 
  (apply-or
    (map
      (lambda (path)
        (let ((dict 
                (graph-match path
                  (match:extend-dict chess-board:var
                                    board
                                    (match:new-dict))
                  (board 'node-at from))))
          (and dict
            (address= to (board 'address-of (match:get-value 'target-node dict)))
            )
          )
        )
      en-passant-moves
      )
    )
  )
(define (simple-move board from to)
  (let* ((my-piece (get-piece-to-move board from))
         ;; changed
         (is-en-passant-move (en-passant-move? board from to)))
    ;; A bunch of checks for validity of move:
    (let ((captured (board 'piece-at to)))
      (if (not (no-piece-or-opponent? captured my-piece))
          (error "Can't capture piece of same color:" captured)))
    ;; The move looks good; make it so:
    (board 'set-piece-at to my-piece)
    ;; Now update all the unaffected pieces to the next state of
    ;; the board:
    ;;; i.e. just update those pieces to exist in the next turn.
    (for-each (lambda (address)
                (if (not (or (address= from address)
                             (address= 
                              ;; changed
                              (if is-en-passant-move
                                ;; 0. if black, node-at and address-of will does 2 invert-address's.
                                ;; So consistency is kept.
                                ;; 1. Since en-passant-move moves forward,
                                ;; so south must have one valid node.
                                ;; so no has-edge? check here.
                                (board 'address-of ((board 'node-at to) 'edge-value 'south))
                                to) 
                              address)))
                    (let ((p (board 'piece-at address)))
                      (if p
                          (board 'set-piece-at address p)))))
              board-addresses)
    ;; just update turn
    (board 'next-turn)))

(define (occupied-by-and-east-occupied-by-opponent-pawn-advanced-two-initially-just-now type)
  (lambda (place-node dict)
    (occupied-by-opponent-pawn-advanced-two-initially-just-now 'east place-node dict)
    ))
(define (occupied-by-and-west-occupied-by-opponent-pawn-advanced-two-initially-just-now type)
  (lambda (place-node dict)
    (occupied-by-opponent-pawn-advanced-two-initially-just-now 'west place-node dict)
    ))
(define en-passant-moves
  (list
    `((? source-node ,(occupied-by-and-west-occupied-by-opponent-pawn-advanced-two-initially-just-now 'pawn))
      northwest (? target-node ,unoccupied))
    `((? source-node ,(occupied-by-and-east-occupied-by-opponent-pawn-advanced-two-initially-just-now 'pawn))
      northeast (? target-node ,unoccupied))
    )
  )