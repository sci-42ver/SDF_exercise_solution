(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'abstracting-a-domain:factoring)

(load "chess-lib/chess-cfg.scm")
(load "chess-lib/chess-factoring.scm")
(load "chess-lib/coords-complement.scm")
(load "2_12.scm")

(load "chess-lib/piece-complement.scm")
(load "2_13.scm")

(load "~/SICP_SDF/SDF_exercises/software/sdf/common/testing.scm")
(load "chess-lib/test-chess-utils.scm")
(define-test 'checkers
  (lambda ()
    (let ((board (make-board chess)))
      (assert-board 'black
        (append 
          (list (make-piece-type 'Rook 'black 0 0) (make-piece-type 'Knight 'black 1 0)
              (make-piece-type 'Bishop 'black 2 0) (make-piece-type 'King 'black 3 0)
              (make-piece-type 'Queen 'black 4 0) (make-piece-type 'Bishop 'black 5 0)
              (make-piece-type 'Knight 'black 6 0) (make-piece-type 'Rook 'black 7 0))
          (map (lambda (col) (make-piece-type 'Pawn 'black col 1)) (iota 8))
          (list (make-piece-type 'Rook 'white 0 0) (make-piece-type 'Knight 'white 1 0)
              (make-piece-type 'Bishop 'white 2 0) (make-piece-type 'Queen 'white 3 0)
              (make-piece-type 'King 'white 4 0) (make-piece-type 'Bishop 'white 5 0)
              (make-piece-type 'Knight 'white 6 0) (make-piece-type 'Rook 'white 7 0))
          (map (lambda (col) (make-piece-type 'Pawn 'white col 1)) (iota 8)))
        board)
      (generate-legal-moves board)
      )))

(run-tests "" #t)