;; to allow move pawn whose pred's have not been defined with just patterns in SDF_exercises/chapter_4/4_23.scm/ 
(define simple-move-orig simple-move)
(define (chess-move-orig from to)
  (set! the-board (simple-move-orig the-board from to))
  (print-chess-board the-board))