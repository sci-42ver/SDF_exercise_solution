(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_graph_match_lib/")
;; this has loaded en_passant_lib.scm->initial_piece_lib.scm
(load "check_lib.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_graph_match_lib/")
(load "simple_move_orig_save.scm")
(load "../4_24_based_on_graph_match_lib.scm")
; (pp all-bishop-moves)

;; 0. There are 3 simple-move locations with the following 2 plus initial_piece_lib.scm.
;; 1. based on SDF_exercises/chapter_4/4_23_graph_match_lib/check_lib.scm 
;; which is based on SDF_exercises/chapter_4/4_23_graph_match_lib/en_passant_lib.scm
;; 2. See SDF_exercises/chapter_4/4_23_graph_match_lib/castling_lib.scm
;; for why we define %simple-move.
(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_graph_match_lib/")
(load "common/board_lib.scm")
;; TODO I forgot why I don't also use from-to-pairs for simple-move-moved-part.
(define (simple-move-moved-part board from to)
  (let ((my-piece (get-piece-to-move board from)))
    ;; A bunch of checks for validity of move:
    (let ((captured (board 'piece-at to)))
      (if (not (no-piece-or-opponent? captured my-piece))
          (error "Can't capture piece of same color:" captured)))
    ;; not done here due to "... simultaneously ..." in SDF_exercises/chapter_4/4_23_graph_match_lib/castling_lib.scm.
    ; ;; changed
    ; ;; from SDF_exercises/chapter_4/4_24_based_on_graph_match.scm
    ; (if (not (check-move-for-type board from to))
    ;   (error (list "invalid move" my-piece from to)))
    ;; The move looks good; make it so:
    (board 'set-piece-at to
      ;; changed
      (untick-piece-initial-mark 
        (tick-piece-advance-two-initially-just-now board my-piece from to))
      )
    ;; We should not interleave this inside simple-move-moved-part if not backuping piece-positions since that will be changed during simple-move-... parts.
    ;; Here %unchecked won't be influenced since opponent pos's won't be changed by simple-move-moved-part.
    (if (eq? 'king (piece-type my-piece))
      (if (not (%unchecked (board 'node-at to) board))
        (error (list "king moved dest" to "is checked")))
      (let ((king-pos (get-current-king-pos board #t)))
        (if (not (%unchecked (board 'node-at king-pos) board))
          (error (list "king at" king-pos "is checked"))))
      )
    ;; changed*
    (set! piece-positions (delete (board-address* board from) piece-positions))
    )
  )
(cd "~/SICP_SDF/SDF_exercises/chapter_4/")
(load "pred_lib.scm")
(define new-from-to-pair cons)
(define get-from car)
(define get-to cdr)
;; It is just like one player moved multiple pieces of his own at one turn. See SDF_exercises/chapter_4/4_23_graph_match_lib/castling_lib.scm
(define (simple-move-rest-part board from-to-pairs)
  (let* (;; changed
          (from-lst (map get-from from-to-pairs))
          (to-lst (map get-to from-to-pairs))
          (is-en-passant-move-lst 
            (map 
              (lambda (from to) 
                (en-passant-move? board from to)) 
              from-lst to-lst))
          )
    ;; Now update all the unaffected pieces to the next state of
    ;; the board:
    ;;; i.e. just update those pieces to exist in the next turn.
    (let ((to*-lst
            (map
              (lambda (is-en-passant-move to)
                ;; changed
                (if is-en-passant-move
                  ;; 0. if black, node-at and address-of will does 2 invert-address's.
                  ;; So consistency is kept.
                  ;; 1. Anyway the output addr's are all based on view just as "to" param.
                  (let ((captured-pos (board 'address-of ((board 'node-at to) 'edge-value 'south))))
                    ;; changed*
                    (set! piece-positions 
                      (cons 
                        (board-address* board to) 
                        (delete (board-address* board captured-pos) piece-positions)))
                    captured-pos
                    )
                  ;; IGNORE: *no need for changes due to (cons to (delete to piece-positions)).
                  (begin
                    ;; changed*
                    ;; 0. only when "to" is captured, we have (delete to piece-positions).
                    ;; 1. See no-piece-or-opponent?
                    ;; 2. Check: here only 3 pos's are possible to be influenced (from to captured-pos)
                    ;; We just check for all possible delete and cons for them.
                    ;; Trivially from and captured-pos can't be "cons"ed.
                    (if (not (board 'piece-at to))
                      (set! piece-positions (cons (board-address* board to) piece-positions))
                      'to-pos-is-added-after-deleted
                      )
                    to))
                )
              is-en-passant-move-lst
              to-lst
              )
            ))
      (for-each (lambda (address)
                    (let ((pred-lst
                            (map
                              (lambda (from to*)
                                (or (address= from address)
                                  (address= to* address))
                                )
                              from-lst 
                              to*-lst
                              )))
                      (if (not (apply-or pred-lst))
                        (let ((p (board 'piece-at address)))
                          (if p
                              (begin
                                (display (list "set-piece-at for" address))
                                (board 'set-piece-at address
                                  ;; changed
                                  ;; If this has been ticked before, this will be unticked after opponent (assuming black) has moved one piece afterwards.
                                  ;; So now white is to move in the next turn. So no "just-now".
                                  (untick-piece-advance-two-initially-just-now p)
                                  ))
                              ))
                        ; (display (list address "set-piece-at fails for" pred-lst))
                        ))
                    )
                  board-addresses)
        ;; to allow the following set! to update board.
        board
      )
    )
  )
(define (%simple-move board from to)
  ;; See SDF_exercises/chapter_4/4_23_graph_match_lib/castling_lib.scm for why we do these changes.
  ;; changed
  (backup-piece-positions)
  (simple-move-moved-part board from to)
  (simple-move-rest-part board (list (new-from-to-pair from to)))
  )

(define (safe-simple-move board from to)
  (and
    (check-move-for-type board from to)
    (%simple-move board from to)
    ))

(define (simple-move board from to)
  (if (safe-simple-move board from to)
    ;; just update turn
    (board 'next-turn)
    (error (list "invalid move" from to)))
  )