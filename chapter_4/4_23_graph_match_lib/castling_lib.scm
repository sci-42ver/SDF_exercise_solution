;; bottom-right piece depends on color.
(define (br-rook-initial board)
  (and 
    (rook? (board 'piece-at (make-address 0 7)))
    ((board 'node-at (make-address 0 7)) 'edge-value 'initial)
    )
  )
;; occupied-by-and-initial-and-a1-rook-initial similar
(define (occupied-by-and-initial-and-br-rook-initial type)
  (and 
    (occupied-by-and-initial type)
    
    ))

(define (capture?* board from path)
  (let* ((my-piece (get-piece-to-move board from))
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
;; we get place-node address
;; Then we store all checked pos's by (capture?* board from path) in one data structure
;; where from is one of all nodes with pieces (also in one data structure to save time)
;; path is based on from type.
;; Then we get all checked pos's.
(define (unoccupied-and-unchecked place-node dict)
  
  )

;; We use the 2nd move in castling-king-moves with graph-match as capture?* does.
(define (king-castling-with_a1 board)
  
  )

(define (occupied-by-and-initial-and-white-and-king-castling-with_a1-and-a1 parameters)
  (and
    (white? board)
    (a1? node)
    )
  )

