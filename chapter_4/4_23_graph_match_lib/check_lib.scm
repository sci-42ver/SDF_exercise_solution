;;; finished
;; 0. en-passant-move can't capture king, so skipped.
;; Similar for castling.
;; 1. NOTICE: here path must have target-node able to be unoccupied, so `pawn-capture-moves` in 4_23 needs small modification.
;; See "*vacant* squares which can be checked".
(define basic-pawn-possible-capture-move
  `((? source-node ,(occupied-by 'pawn))
      northeast (? target-node ,maybe-opponent))
  )
(define pawn-possible-capture-moves
  (symmetrize-move basic-pawn-possible-capture-move rotate-90)
  )
(define (get-capture-moves type)
  ;; similar to get-moves in 4.24
  (case type
    ;; > A pawn, unlike other pieces, captures differently from how it moves.
    ((pawn) pawn-possible-capture-moves)
    ;; Search target-node in SDF_exercises/chapter_4/4_23.scm. 
    ;; Here only maybe-opponent is allowed since only this can find those *vacant* squares which can be checked.
    ((rook) simple-rook-moves)
    ((knight) all-knight-moves)
    ((bishop) all-bishop-moves)
    ((queen) all-queen-moves)
    ((king) simple-king-moves)
    ))
(define (capture?* board from path allow-any-color)
  (let* ((my-piece ((if allow-any-color get-any-piece-to-move get-piece-to-move) board from))
         (dict
          (graph-match path
                       ;; > used by some pattern restrictions that need to interrogate the board.
                       ;; Used by `unoccupied` etc.
                       (match:extend-dict chess-board:var
                                          board
                                          (match:new-dict))
                       (board 'node-at from))))
    (and dict
         (let* ((target (match:get-value 'target-node dict))
                ;; we don't need one captured piece to move to there.
                ; (captured (board 'piece-in target))
                )
           ;; modified
           `(capture ,my-piece
                    ; ,captured
                    ,(board 'address-of target))))))
(define get-capture-res-addr caddr)
(define (get-any-piece-to-move board from)
  (let ((my-piece (board 'piece-at from)))
    (if (not my-piece)
        (error "No piece in this square:" from))
    ; (if (not (eq? (board 'color) (piece-color my-piece)))
    ;     (error "Can move only one's own pieces:" my-piece from))
    my-piece))
(define (get-target-pos board from path allow-any-color)
  (let ((res (capture?* board from path allow-any-color)))
    (and 
      res
      (get-capture-res-addr res)
      )))

;; similar to the above
(define (general-capture? board from path allow-any-color)
  (let* ((my-piece ((if allow-any-color get-any-piece-to-move get-piece-to-move) board from))
         (dict
          (graph-match path
                       ;; > used by some pattern restrictions that need to interrogate the board.
                       ;; Used by `unoccupied` etc.
                       (match:extend-dict chess-board:var
                                          board
                                          (match:new-dict))
                       (board 'node-at from))))
    (and dict
         (let* ((target (match:get-value 'target-node dict))
                ;; we don't need one captured piece to move to there.
                ; (captured (board 'piece-in target))
                (intermediate-possible-nodes
                  (and (match:has-binding? 'intermediate-possible-nodes dict)
                    (match:get-value 'intermediate-possible-nodes dict)))
                )
            (and intermediate-possible-nodes (assert (not (memq target intermediate-possible-nodes))))
           ;; modified
           `(capture ,my-piece
                    ; ,captured
                    ,(cons 
                      (board 'address-of target)
                      (if intermediate-possible-nodes
                        (map (lambda (node) (board 'address-of node)) intermediate-possible-nodes)
                        '())
                      ))))))
(define get-general-capture-res-addr get-capture-res-addr)
(define (get-intermediate-and-target-positions board from path allow-any-color)
  (let ((res (general-capture? board from path allow-any-color)))
    (and 
      res
      (get-general-capture-res-addr res)
      )))

;; 0. we get place-node address
;; Then we store all checked pos's by (capture?* board from path) in one data structure
;; where from is one of all nodes with pieces (also in one data structure to save time)
;; path is based on "from" type.
;; Then we get all checked pos's.
;; 1. All pieces may be better to be coupled with board. 
;; But (populate-sides board) is done after (make-chess-board-internal).
;; 1.a. So here just use dict to store that like chess-board:var does.
;; Then we need one helper to update that binding based on name.
;; `~/SICP_SDF/SDF_exercises/chapter_4$ grep match:binding-name -r .` has only 4_19_rename_dict_lib.scm results.
;; 1.a.0. But if we update checked pos's and piece pos's after each simple-move.
;; Then no dict at all...
;; So just use 2 global vars.
(define checked_positions '())
;; based on turn color
(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_graph_match_lib/")
(load "common/board_lib.scm")
(define (keep-only-opponents board positions)
  (remove (lambda (pos) (my-piece? board pos)) positions))
(define (unchecked place-node dict)
  (let* ((board (chess-dict:board dict))
        ;  (my-piece (board 'piece-in place-node))
         )
    ;; 0. We should not update captured-positions for each place-node
    ;; since that is decided by board instead of place-node.
    ;; 0.a. IGNORE: what's more, (get-target-pos board king-pos king-path)
    ;; may call unchecked which then again calls (get-intermediate-and-target-positions board king-pos king-path) here...
    (let* ((opponent-positions
            (keep-only-opponents 
              board
              (map 
                (lambda (addr) (board-address* board addr)) 
                piece_positions))
            )
          (captured-positions
            (delete-duplicates
              (append-map
                (lambda (addr)
                  (apply append
                    (filter-map
                      (lambda (path)
                        (get-intermediate-and-target-positions board addr path #t)
                        )
                      (get-capture-moves (piece-type (board 'piece-at addr)))
                      ))
                  )
                ;; Since piece_positions is default be based on white.
                ;; So we need to cater to the board color used in piece-at.
                opponent-positions
                )
              ;; all are got by address-of which may call invert-address->make-address->list.
              ;; so default to use equal?.
              ; equal?
              )
            ))
      ;; Here we can also directly check the equality between nodes.
      ;; For debug, we generate position.
      ; (write-line 
      ;   (list 
      ;     "captured-positions:" captured-positions 
      ;     "with board color:" (board 'color)
      ;     "opponent-positions:" opponent-positions
      ;     "piece_positions:" piece_positions
      ;     "place-node:" place-node
      ;     ))
      (not 
        (any 
          (lambda (addr)
            (address= (board 'address-of place-node) addr)
            )
          captured-positions
          ))
    ))
  )

(define piece_positions '())
(define (populate-sides board)

  (define (populate-side color home-row pawn-row)

    (define (do-column col type)
      (add-piece col home-row type)
      (add-piece col pawn-row 'pawn))

    (define (add-piece col row type)
      ((board 'node-at (make-address col row))
       'connect! 0 (make-piece type color))
      ;; added
      (set! piece_positions (cons (make-address col row) piece_positions))
      )

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
;; based on SDF_exercises/chapter_4/4_23_graph_match_lib/en_passant_lib.scm
(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_graph_match_lib/")
(load "en_passant_lib.scm")
;; 0. since it needs to check what piece is captured.
;; TODO I forgot why I wrote the above line. Maybe it meant for is-en-passant-move.
;; 1. See SDF_exercises/chapter_4/4_23_graph_match_lib/simple_move_mod.scm for correction about piece_positions.
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
    ;; changed*
    (set! piece_positions (delete from piece_positions))
    ;; Now update all the unaffected pieces to the next state of
    ;; the board:
    ;;; i.e. just update those pieces to exist in the next turn.
    (for-each (lambda (address)
                (if (not (or (address= from address)
                             (address= 
                              ;; changed
                              (if is-en-passant-move
                                ;; if black, node-at and address-of will does 2 invert-address's.
                                ;; So consistency is kept.
                                (let ((captured-pos (address-of ((board 'node-at to) 'edge-value 'south))))
                                  ;; changed*
                                  (set! piece_positions (cons to (delete captured-pos piece_positions)))
                                  captured-pos
                                  )
                                ;; IGNORE: *no need for changes due to (cons to (delete to piece_positions)).
                                (begin
                                  ;; changed*
                                  ;; 0. only when "to" is captured, we have (delete to piece_positions).
                                  ;; 1. See no-piece-or-opponent?
                                  ;; 2. Check: here only 3 pos's are possible to be influenced (from to captured-pos)
                                  ;; We just check for all possible delete and cons for them.
                                  ;; Trivially from and captured-pos can't be "cons"ed.
                                  (if (not (board 'piece-at to))
                                    (set! piece_positions (cons to piece_positions)))
                                  to)) 
                              address)))
                    (let ((p (board 'piece-at address)))
                      (if p
                          (board 'set-piece-at address p)))))
              board-addresses)
    ;; just update turn
    (board 'next-turn)))

(define (unoccupied-and-unchecked place-node dict)
  (write-line (list "unoccupied-and-unchecked" place-node (unoccupied place-node dict) (unchecked place-node dict)))
  (and 
    (unoccupied place-node dict)
    (unchecked place-node dict)
    )
  )