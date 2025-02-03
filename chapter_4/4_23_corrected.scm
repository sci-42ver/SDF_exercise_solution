(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'pattern-matching-on-graphs)

;; https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_16.html
; (for-each 
;   (lambda (filename) 
;     (load filename)) 
;   ;; remove . and ..
;   (cddr (map ->namestring (directory-read "./4_23_graph_match_lib/"))))
(cd "./4_23_graph_match_lib")
(load "promotion_lib.scm")
;; load initial_piece_lib.scm->common/base_lib.scm
(load "simple_move_mod.scm")
;; load common/board_lib.scm
(load "castling_lib.scm")
(cd "..")

(load "4_23.scm")
;; Just use SDF_exercises/software/sdf/pattern-matching-on-graphs/chess-moves.scm
;; 0. all-bishop-moves

(define simple-rook-moves all-rook-moves)
(define simple-king-moves all-king-moves)
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
    all-bishop-moves
    all-queen-moves
    all-king-moves
    ))
