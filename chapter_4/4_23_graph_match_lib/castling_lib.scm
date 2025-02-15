;;;; needs SDF_exercises/chapter_4/4_23_graph_match_lib/base_lib.scm
(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_graph_match_lib/")
;; occupied-by-and-initial, 
(load "./initial_piece_lib.scm")

;; 0. bottom-right piece depends on color.
;; 1. br means bottom-right.
(define (rook-initial board addr)
  (let ((piece (board 'piece-at addr)))
    (and
      ;; no need for board-address since node-at will do invert-address appropriately.
      (rook_piece? piece)
      (piece-initial-mark piece)
      ))
  )
(define (br-rook-initial board)
  (rook-initial board (make-address 0 7)))
(define (bl-rook-initial board)
  (rook-initial board (make-address 0 0)))

;; occupied-by-and-initial-and-bl-rook-initial similar
(define (occupied-by-and-initial-and-br-rook-initial type)
  (lambda (place-node-expected-to-be-king dict) 
    (and 
      ((occupied-by-and-initial type) place-node-expected-to-be-king dict)
      (br-rook-initial (chess-dict:board dict))
      )))
;; similarly
(define (occupied-by-and-initial-and-bl-rook-initial type)
  (lambda (place-node-expected-to-be-king dict) 
    (and 
      ((occupied-by-and-initial type) place-node-expected-to-be-king dict)
      (bl-rook-initial (chess-dict:board dict))
      )))

(load "check_lib.scm")

;;; Emm... I forgot what I meant by "the 2nd move in castling-king-moves".
;; We use the 2nd move in castling-king-moves with graph-match as capture?* does.
(define (get-initial-king-pos board color)
  (if (eq? 'white (board 'color))
    (if (eq? color (board 'color))
      (make-address 4 0)
      (make-address 4 7))
    (if (eq? color (board 'color))
      (make-address 3 0)
      (make-address 3 7)))
  )
; (load "../pred_lib.scm")
(define (king-castling-with_bl board)
  (king-castling-check board get-bl-castling-king-move))
(define (king-castling-with_br board)
  (king-castling-check board get-br-castling-king-move))
(define (king-castling-check board accessor)
  ;; since we can only move the current color piece, so assume "(board 'color)".
  (let ((king-pos (get-initial-king-pos board (board 'color))))
    ((lambda (path)
        (let ((dict 
                (graph-match path
                  (match:extend-dict chess-board:var
                                    board
                                    (match:new-dict))
                  (board 'node-at king-pos))))
          (and dict
            ; (address= to (board 'address-of (match:get-value 'target-node dict)))
            )
          )
        )
      (accessor castling-king-moves)
      ))
  )

(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_graph_match_lib/")
(load "common/board_lib.scm")
;; called by 
;; occupied-by-and-initial-and-king-castling-with_bl-and-bl => 
;; occupied-by-and-initial-and-black-and-king-castling-with_bl-and-bl =>
;; black-castling-rook-moves =>
;; castling-rook-moves =>
;; castling-move
;; So all addr's are all based on board view (see (get-target-pos board rook-pos rook-path #f)->...->(board 'node-at from)). 
;; So fine to use 'address-of here.
(define (bl? node board)
  (address= (make-address 0 0) (board 'address-of node)))
(define (br? node board)
  (address= (make-address 7 0) (board 'address-of node)))
;; See castling-move where we must move 2 pieces simultaneously, so no need to recheck king-castling-with_bl etc.
(define (occupied-by-and-initial-and-king-castling-with_bl-and-bl type color)
  (lambda (place-node dict) 
    (let ((board (chess-dict:board dict))
          (pred
            (if (eq? 'white color)
              white?
              black?)
            ))
      ; (write-line (list "occupied-by-and-initial-and-king-castling-with_bl-and-bl:" type place-node dict ((occupied-by-and-initial type) place-node dict)))
      (and 
        ((occupied-by-and-initial type) place-node dict)
        ;; ensure we moved the same color as the current.
        ; (eq? (board 'color) color) ; duplicate of the next logical expression.
        (pred board)
        ; (king-castling-with_bl board)
        (bl? place-node board)
        )
      )
    ))
;; similar
(define (occupied-by-and-initial-and-king-castling-with_br-and-br type color)
  (lambda (place-node dict) 
    (let ((board (chess-dict:board dict))
          (pred
            (if (eq? 'white color)
              white?
              black?)
            ))
      (and 
        ((occupied-by-and-initial type) place-node dict)
        ;; ensure we moved the same color as the current.
        ; (eq? (board 'color) color)
        (pred board)
        (king-castling-with_br board)
        (br? place-node board)
        )
      )
    ))
(define (occupied-by-and-initial-and-black-and-king-castling-with_bl-and-bl type)
  (occupied-by-and-initial-and-king-castling-with_bl-and-bl type 'black))
(define (occupied-by-and-initial-and-black-and-king-castling-with_br-and-br type)
  (occupied-by-and-initial-and-king-castling-with_br-and-br type 'black))
(define (occupied-by-and-initial-and-white-and-king-castling-with_bl-and-bl type)
  (occupied-by-and-initial-and-king-castling-with_bl-and-bl type 'white))
(define (occupied-by-and-initial-and-white-and-king-castling-with_br-and-br type)
  (occupied-by-and-initial-and-king-castling-with_br-and-br type 'white))

;; Here we should not use simple-move for castling since it moves *one* piece and then begins the next turn.
;; Furthermore, these 2 pieces should be moved simultaneously, otherwise the `unoccupied` pred may be tested wrongly.
(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_graph_match_lib/")
(load "../graph_match_lib/addr_lib.scm")
(define (pos-left? pos1 pos2)
  (let ((diff (address-diff pos1 pos2)))
    (and
      (n:< (address-x diff) 0)  
      (n:= (address-y diff) 0)  
      )
    ))
(define (pos-left-in-the-board-of-this-turn? board pos1 pos2)
  ; (if (board 'white-move?)
  ;   (pos-left? pos1 pos2)
  ;   (pos-left? (invert-address pos1) (invert-address pos2)))
  ;; invert-address will be manipulated by node-at.
  (pos-left? pos1 pos2)
  )

(load "combination/simple_move_mod.scm")
(define (castling-move board king-pos rook-pos)
  ;; 0. based on SDF_exercises/chapter_4/4_23_graph_match_lib/simple_move_mod.scm
  ;; 1. similar to check-move-for-type
  (let ((whether-white-move (board 'white-move?))
        (whether-rook-left-pos (pos-left-in-the-board-of-this-turn? board rook-pos king-pos)))
    (let ((king-path
            (if whether-rook-left-pos
              (get-bl-castling-king-move castling-king-moves)
              (get-br-castling-king-move castling-king-moves))
            )
          (rook-path
            (if whether-white-move
              (if whether-rook-left-pos
                (get-white-bl castling-rook-moves)
                (get-white-br castling-rook-moves))
              (if whether-rook-left-pos
                (get-black-bl castling-rook-moves)
                (get-black-br castling-rook-moves)))
            )
          )
      ; (trace get-target-pos)
      (write-line (list "castling-move" king-pos rook-pos king-path rook-path))
      (let ((king-to (get-target-pos board king-pos king-path #f))
            (rook-to (get-target-pos board rook-pos rook-path #f)))
        ; (untrace get-target-pos)
        (if (and king-to rook-to)
          (begin
            ;; 0. see simple-move in SDF_exercises/chapter_4/4_23_graph_match_lib/simple_move_mod.scm
            ;; see chess-move
            ;; 1. This can't work since set-piece-at will set *all* pieces for the *next turn* of the current turn in one %simple-move.
            ;; Then the 2nd safe-simple-move can't set rook for the next turn of the current turn.
            ; (set! board (safe-simple-move board king-pos king-to))
            ; (display (list "new board-1 with move" king-pos king-to))
            ; (print-chess-board board)
            ; (set! board (safe-simple-move board rook-pos rook-to))
            ; (display "new board-2:")
            ; (print-chess-board board)

            (mul-simple-move board (list (new-from-to-pair king-pos king-to) (new-from-to-pair rook-pos rook-to)))
            (board 'next-turn)
            )
          (error (list "invalid move for (king rook)" (list king-pos rook-pos) "with" king-to rook-to)))
        )
      )
    )
  )
(cd "~/SICP_SDF/SDF_exercises/chapter_4/")
(load "pred_lib.scm")
(define (mul-simple-move board from-to-pairs)
  (let ((from-lst (map get-from from-to-pairs))
        (to-lst (map get-to from-to-pairs)))
    ;; we move from-to-pairs simultaneously, so we check *all beforehand*.
    (let ((preds 
            (map
              (lambda (from to)
                (check-move-for-type board from to)
                )
              from-lst to-lst
              )))
      (if (apply-and preds)
        'success
        (write-line (list "fail with preds:" preds "for from-to-pairs" from-to-pairs))))
    (for-each
      (lambda (from to)
        ;; moved each piece to the target pos to
        ;; and update piece_positions to remove those from's.
        ;; If later to is former from, it may be cons'ed later.
        (simple-move-moved-part board from to)
        )
      from-lst to-lst
      ))
  ;; 0. update for the rest where only those pieces not related with from-to-pairs are kept.
  ;; 1. Notice piece_positions may be updated to add to-lst.
  (simple-move-rest-part board from-to-pairs)
  )
(define (chess-move* king-pos rook-pos)
  (set! the-board (castling-move the-board king-pos rook-pos))
  (print-chess-board the-board))
