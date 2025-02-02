(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_graph_match_lib/")
;; this has loaded en_passant_lib.scm->initial_piece_lib.scm
(load "check_lib.scm")

;; 0. There are 3 simple-move locations with the following 2 plus initial_piece_lib.scm.
;; 1. based on SDF_exercises/chapter_4/4_23_graph_match_lib/check_lib.scm 
;; which is based on SDF_exercises/chapter_4/4_23_graph_match_lib/en_passant_lib.scm
(define (simple-move board from to)
  (let* ((my-piece (get-piece-to-move board from))
         ;; changed
         (is-en-passant-move (en-passant-move? board from to)))
    ;; A bunch of checks for validity of move:
    (let ((captured (board 'piece-at to)))
      (if (not (no-piece-or-opponent? captured my-piece))
          (error "Can't capture piece of same color:" captured)))
    ;; The move looks good; make it so:
    (board 'set-piece-at to
      ;; changed
      (untick-piece-initial-mark 
        (tick-piece-advance-two-initially-just-now board my-piece from to))
      )
    ;; changed*
    (set! piece_positions (delete from piece_positions))
    ;; Now update all the unaffected pieces to the next state of
    ;; the board:
    ;;; i.e. just update those pieces to exist in the next turn.
    (for-each (lambda (address)
                (if (not (or (address= from address)
                             (address= 
                              ;; changed
                              (if is-en-passant-move
                                ;; if black, node-at and address-of will does 2 invert-address's.
                                ;; So consistency is kept.
                                (let ((captured-pos (address-of ((board 'node-at to) 'edge-value 'south))))
                                  ;; changed*
                                  (set! piece_positions (cons to (delete captured-pos piece_positions)))
                                  captured-pos
                                  )
                                ;; IGNORE: *no need for changes due to (cons to (delete to piece_positions)).
                                (begin
                                  ;; changed*
                                  ;; 0. only when "to" is captured, we have (delete to piece_positions).
                                  ;; 1. See no-piece-or-opponent?
                                  ;; 2. Check: here only 3 pos's are possible to be influenced (from to captured-pos)
                                  ;; We just check for all possible delete and cons for them.
                                  ;; Trivially from and captured-pos can't be "cons"ed.
                                  (if (not (board 'piece-at to))
                                    (set! piece_positions (cons to piece_positions)))
                                  to)) 
                              address)))
                    (let ((p (board 'piece-at address)))
                      (if p
                          (board 'set-piece-at address
                            ;; changed
                            (untick-piece-advance-two-initially-just-now p)
                            )))))
              board-addresses)
    ;; just update turn
    (board 'next-turn)))