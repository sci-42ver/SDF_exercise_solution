; (cd "~/SICP_SDF/SDF_exercises/chapter_4")
; (load "../software/sdf/manager/load.scm")
; (manage 'command-apropos 'new)
; ;Value: (new-environment)
; (manage 'new 'pattern-matching-on-graphs)

;; https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_16.html
; (for-each 
;   (lambda (filename) 
;     (load filename)) 
;   ;; remove . and ..
;   (cddr (map ->namestring (directory-read "./4_23_graph_match_lib/"))))
(cd "./4_23_graph_match_lib")
(load "promotion_lib.scm")
;; 0. load initial_piece_lib.scm->common/base_lib.scm
(load "combination/simple_move_mod.scm")
;; 1. IGNORE: SDF_exercises/chapter_4/4_24_based_on_graph_match_lib.scm will change all-bishop-moves etc with SDF_exercises/chapter_4/4_24_moves_with_intermediate_nodes_lib.scm.
;; Only 3 locations have "define all-bishop-moves" by grep.
(load "4_24_moves_with_intermediate_nodes_lib.scm")
; (pp all-bishop-moves)
;; load common/board_lib.scm
(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_graph_match_lib")
;; will load check_lib.scm which will define populate-sides etc
(load "castling_lib.scm")
(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_graph_match_lib")
(load "combination/populate-sides.scm")
(cd "~/SICP_SDF/SDF_exercises/chapter_4")

(define all-rook-moves-from-code-base all-rook-moves)
(define all-king-moves-from-code-base all-king-moves)
(define all-bishop-moves-from-code-base all-bishop-moves)

(load "4_23.scm")
;; Just use SDF_exercises/software/sdf/pattern-matching-on-graphs/chess-moves.scm
;; 0. all-bishop-moves

(define simple-rook-moves all-rook-moves-from-code-base)
(define simple-king-moves all-king-moves-from-code-base)
(define all-king-moves
  (append castling-king-moves simple-king-moves)
  )
(define all-rook-moves
  (append white-castling-rook-moves black-castling-rook-moves simple-rook-moves)
  )

;; used by SDF_exercises/chapter_4/4_23_graph_match_lib/simple_move_mod.scm
(define all-pawn-moves
  (append pawn-capture-moves basic-pawn-moves en-passant-moves))
(define all-moves 
  (append 
    all-pawn-moves
    all-rook-moves
    all-knight-moves
    all-bishop-moves-from-code-base
    all-queen-moves
    all-king-moves
    ))
