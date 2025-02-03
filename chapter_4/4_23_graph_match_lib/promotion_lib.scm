;;; finished

;; See SDF_exercises/chapter_4/4_25.scm
(cd "~/SICP_SDF/SDF_exercises/chapter_4/4_23_graph_match_lib/")
(load "common/base_lib.scm")
(define (promotion? address piece)
  (and
    (pawn_piece? piece)
    ;; if white, just no invert.
    ;; otherwise 2 inverts.
    ;; Both have the same results.
    ;; So fine to check address-y.
    (n:= 7 (address-y (address-of (node-at address))))
    ))
(define (make-chess-board-internal)
  (let ((nodes
         (map (lambda (x)
                (map (lambda (y)
                       (make-graph-node (string x "," y)))
                     chess-board-indices))
              chess-board-indices)))
    (let loop ((turn 0))

      ;; coderef: node-at
      (define (node-at address)

        (define (get-node address)
          (list-ref (list-ref nodes (address-x address))
                    (address-y address)))

        ;; > White will see a node directly and Black will see the same node projected through the rotate-180-view.
        (if (white-move?)
            (get-node address)
            (graph-node-view (get-node (invert-address address))
                             rotate-180-view)))

      ;; coderef: piece-at
      (define (piece-at address)
        (piece-in (node-at address)))

      ;; coderef: piece-in
      (define (piece-in node)
        (and (node 'has-edge? turn)
             (node 'edge-value turn)))

      ;; coderef: address-of
      (define (address-of node)
        (let ((address (node 'edge-value 'address)))
          (if (white-move?)
              address
              (invert-address address))))

      ;; coderef: set-piece-at
      ;; changed
      (define (set-piece-at address piece)
        (if (promotion? address piece)
          (begin
            (display "This piece can be promoted. Which type do you want to promote to?")
            (let* ((type (read))
                   (promoted-piece
                    (make-piece type (piece-color piece))
                    ))
              ; (eq? 'pawn type)
              ((node-at address) 'connect! (+ turn 1) promoted-piece)
              )
            )
          ((node-at address) 'connect! (+ turn 1) piece))
        )

      ;; coderef: white-move?
      (define (white-move?)
        (even? turn))

      ;; coderef: color
      (define (color)
        (if (white-move?) 'white 'black))

      ;; coderef: next-turn
      (define (next-turn)
        (loop (+ turn 1)))

      ;; > If a predicate is not needed, bundle alternatively accepts #f as a
      ;; > first argument. In that case there will be no way to distinguish the
      ;; > created bundle procedure from other procedures
      (bundle #f
              node-at piece-at piece-in address-of
              ;; changed
              white-move?
              set-piece-at color next-turn))))
