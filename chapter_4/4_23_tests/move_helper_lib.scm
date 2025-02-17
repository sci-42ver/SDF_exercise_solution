(cd "~/SICP_SDF/SDF_exercises/chapter_4/")
(load "graph_match_lib/addr_lib.scm")
(define (chess-move-forward-one-step from)
  (chess-move from (address-sum (make-address-shift 0 1) from)))