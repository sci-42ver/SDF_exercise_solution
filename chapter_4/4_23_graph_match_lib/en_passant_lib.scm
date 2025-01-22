;; see SDF_exercises/chapter_4/4_25.scm
(define (west/east-occupied-by-advances-two-squares-initial-just-now place-node dict)
  (let ((west/east-piece (board 'piece-at (west/east-addr place-node))))
    (and
      (eq? 'pawn (piece-type west/east-piece))
      ;; This tag needs clear when moved after that "advances-two-squares"
      ;; and markded when "advances-two-squares".
      (advances-two-squares-initial west/east-piece)
      )
    )
  )

;; TODO simple-move* in SDF_exercises/chapter_4/4_23_graph_match_lib/initial_lib.scm
