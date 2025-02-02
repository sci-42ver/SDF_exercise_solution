(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'pattern-matching-on-graphs)

(define (simple-move board from to)
  (let ((my-piece (get-piece-to-move board from)))
    ;; added
    (if (not (check-move-for-type (piece-type my-piece) from to))
      (error (list "invalid move" (piece-type my-piece) from to)))
    ;; A bunch of checks for validity of move:
    (let ((captured (board 'piece-at to)))
      (if (not (no-piece-or-opponent? captured my-piece))
          (error "Can't capture piece of same color:" captured)))
    ;; The move looks good; make it so:
    (board 'set-piece-at to my-piece)
    ;; Now update all the unaffected pieces to the next state of
    ;; the board:
    ;;; i.e. just update those pieces to exist in the next turn.
    (for-each (lambda (address)
                (if (not (or (address= from address)
                             (address= to address)))
                    (let ((p (board 'piece-at address)))
                      (if p
                          (board 'set-piece-at address p)))))
              board-addresses)
    (board 'next-turn)))

;; Here the exercise seems to let us not use graph-match since it is before 4.5.6.
(define (get-moves type)
  ;; similar to piece->string
  ;; regex: \(([^(]*)\) #\\. -> ($1) all-$1-moves
  (case type
    ((pawn) all-pawn-moves)
    ((rook) all-rook-moves)
    ((knight) all-knight-moves)
    ((bishop) all-bishop-moves)
    ((queen) all-queen-moves)
    ((king) all-king-moves)
    ))

(load "./graph_match_lib/addr_lib.scm")

(define (step-address address step)
  ;; similar to rotate-45
  (case step
    ((east) (add-address-by-shifts address 1 0))
    ((north) (add-address-by-shifts address 0 1))
    ((northeast) (add-address-by-shifts address 1 1))
    ((northwest) (add-address-by-shifts address -1 1))
    ((south) (add-address-by-shifts address 0 -1))
    ((southeast) (add-address-by-shifts address 1 -1))
    ((southwest) (add-address-by-shifts address -1 -1))
    ((west) (add-address-by-shifts address -1 0))
    (else (error (list "unknown dir" step))))
  )
(define zero-address (make-address 0 0))
;; 0. similar to rewrite-path-edges
;; 1. IGNORE: Here we only consider the *basic* path not including something like Castling.
(define (path->coordinate-shift path)
  (let ((normal-shift (make-address 0 0)) (*-shift (make-address 0 0)))
    (define (transformer path)
      ;; modified
      (transform-elts (cdr path)))

    (define (transform-elts path-elts)
      (cond ((and (pair? path-elts)
                  (symbol? (car path-elts))
                  (pair? (cdr path-elts))
                  (match:var? (cadr path-elts)))
             ;; modified
             (let ((normal-shift* (step-address normal-shift (car path-elts))))
              (if (not (address= zero-address *-shift))
                (if (not (address= normal-shift* *-shift))
                  (error (list "unknown path" path))
                  'skip-duplicate-shift)
                (set! normal-shift normal-shift*)))
             (transform-tail (cddr path-elts)))
            ;; SDF_exercises TODO when happens
            ((and (pair? path-elts)
                  (pair? (car path-elts))
                  (memq (caar path-elts) '(*)))
             ;; modified
             (if (not (address= zero-address *-shift))
              (error (list "unknown path" path))
              (set! *-shift (step-address *-shift (cadar path-elts))))
             (transform-tail (cdr path-elts)))
            ;; modified
            ((and (pair? path-elts)
                  (pair? (car path-elts))
                  (memq (caar path-elts) '(and or + opt)))
             (error (list path "isn't considered")))
            (else
            (error "Unknown path elements:" path-elts))))

    ;; not combined into transform-elts since path-elts is not allowed to be empty.
    ;; For simple graph https://math.stackexchange.com/a/3260124/1059606, the mere vertex can't be one edge, also for path https://en.wikipedia.org/wiki/Path_(graph_theory).
    (define (transform-tail tail)
      (if (pair? tail)
          (transform-elts tail)
          ;; modified
          (list normal-shift *-shift)
          ))

    ;; modified
    (transformer path))
  )
(define get-normal-shift car)
(define get-*-shift cadr)

(path->coordinate-shift basic-knight-move)
(path->coordinate-shift basic-queen-move)

(define (in-board? address)
  (and (<= 0 (address-x address) chess-board-last-index)
       (<= 0 (address-y address) chess-board-last-index)))
(define (valid-path-origin-end? path from to)
  (let ((shifts (path->coordinate-shift path)))
    (and
      (in-board? to)
      (or 
        (address= (address-sum from (get-normal-shift shifts)) to)
        (address-divisible (address-diff to from) (get-*-shift shifts))
        ))
    )
  )
(define (check-move-for-type type from to)
  (any 
    (lambda (path) (valid-path-origin-end? path from to)) 
    (get-moves type)))

;; tests
(assert (check-move-for-type 'queen '(0 0) '(1 1)))
; (trace valid-path-origin-end?)
; (trace address-divisible)
; (check-move-for-type 'queen '(3 4) '(5 4))
; (untrace valid-path-origin-end?)
; (untrace address-divisible)
(assert (check-move-for-type 'queen '(3 4) '(5 4)))

(define (assert-not pred)
  (assert (not pred)))
(assert-not (check-move-for-type 'queen '(3 4) '(5 5)))

(assert (check-move-for-type 'knight '(3 4) '(5 5)))
(assert-not (check-move-for-type 'knight '(3 4) '(5 4)))

;; castling has the same structure as basic-knight-move.
;; Also for all-pawn-moves.

;; bishop, rook simple moves are just same as queen.
;; king simple moves are just similar to knight.
