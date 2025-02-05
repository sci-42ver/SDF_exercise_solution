;;; Compared with SDF_exercises/chapter_4/4_24.scm
;; 0. we can consider black-move where to may be south of from for pawn.
;; 1. we can consider the predicate in var.
;;; Anyway graph-match is just 
;; > check that the piece being moved is allowed to move in the way requested
;; where way is path in moves list.
(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "pred_lib.scm")
(define (check-move-for-type board from to)
  ;; See SDF_exercises/chapter_4/4_23_graph_match_lib/en_passant_lib.scm
  (apply-or
    (map
      (lambda (path)
        (let ((dict 
                (graph-match path
                  (match:extend-dict chess-board:var
                                    board
                                    (match:new-dict))
                  (board 'node-at from))))
          (let ((res
                    (and dict
                      ;; We don't need capture necessarily.
                      (or
                        (address= to (board 'address-of (match:get-value 'target-node dict)))
                        (and
                          (match:has-binding? 'intermediate-possible-nodes dict)
                          (member 
                            to
                            (map 
                              (lambda (node) (board 'address-of node))
                              (match:get-value 'intermediate-possible-nodes dict)
                              )
                            ))
                        )
                      )   
                  ))
            ;; for debug infos
            (write-line (list path "returns" res "with dict" dict))
            res
            )
          )
        )
      ;; modified
      ; (get-moves (piece-type (get-piece-to-move board from)))
      ;; or just all moves and then let the first var-pred to filter.
      all-moves
      )
    )
  )
(define all-moves 
  (append 
    ; all-pawn-moves
    all-rook-moves
    all-knight-moves
    all-bishop-moves
    all-queen-moves
    all-king-moves
    ))
;; from 4.24
(define (get-moves type)
  ;; similar to piece->string
  ;; regex: \(([^(]*)\) #\\. -> ($1) all-$1-moves
  (case type
    ((pawn) all-pawn-moves)
    ((rook) all-rook-moves)
    ((knight) all-knight-moves)
    ((bishop) all-bishop-moves)
    ((queen) all-queen-moves)
    ((king) all-king-moves)
    ))