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

;; Here the exercise seems to let us not use graph-match
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

(define (make-address-shift x y)
  (guarantee shift? x 'make-address)
  (guarantee shift? y 'make-address)
  (list x y))
(define (shift? object)
  (and (integer? object)
       (exact? object)
       ))
(register-predicate! shift? 'shift)

(define (shift-address address x-shift y-shift)
  (make-address-shift (n:+ (address-x address) x-shift) (n:+ (address-y address) y-shift)))
(define (shift-address* address shift op)
  (make-address-shift 
    (op (address-x address) (address-x shift)) 
    (op (address-y address) (address-y shift))))
(define (add-address address shift)
  (shift-address* address shift n:+)
  )
(define (step-address address step)
  ;; similar to rotate-45
  (case step
    ((east) (shift-address address 1 0))
    ((north) (shift-address address 0 1))
    ((northeast) (shift-address address 1 1))
    ((northwest) (shift-address address -1 1))
    ((south) (shift-address address 0 -1))
    ((southeast) (shift-address address 1 -1))
    ((southwest) (shift-address address -1 -1))
    ((west) (shift-address address -1 0))
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

;; https://en.wikipedia.org/wiki/Subtraction#Notation_and_terminology
(define (address-diff minuend subtrahend)
  (shift-address* minuend subtrahend n:-))
(define (list-op lst1 lst2 op)
  (list
    (op (car lst1) (car lst2)) 
    (op (cadr lst1) (cadr lst2))))
(define (address-division dividend divisor)
  ;; no need to be still one address
  (list-op dividend divisor
    (lambda (a b)
      (if (n:= b 0)
        (if (n:= a 0)
          1
          +inf.0
          )
        (n:/ a b))
      )
    ))
(define (address-divisible dividend divisor)
  (let ((division (address-division dividend divisor)))
    (and 
      (apply n:= division)
      (every exact? division)))
  )

(define (in-board? address)
  (and (<= 0 (address-x address) chess-board-last-index)
       (<= 0 (address-y address) chess-board-last-index)))
(define (valid-path-origin-end? path from to)
  (let ((shifts (path->coordinate-shift path)))
    (and
      (in-board? to)
      (or 
        (address= (add-address from (get-normal-shift shifts)) to)
        (address-divisible (address-diff to from) (get-*-shift shifts))
        ))
    )
  )
(define (check-move-for-type type from to)
  (any 
    (lambda (path) (valid-path-origin-end? path from to)) 
    (get-moves type)))

;; tests
(check-move-for-type 'queen '(0 0) '(1 1))
(check-move-for-type 'queen '(3 4) '(5 4))
; (trace valid-path-origin-end?)
; (trace address-divisible)
(check-move-for-type 'queen '(3 4) '(5 5))

(check-move-for-type 'knight '(3 4) '(5 5))
(check-move-for-type 'knight '(3 4) '(5 4))

;; castling has the same structure as basic-knight-move.
;; Also for all-pawn-moves.

;; bishop, rook simple moves are just same as queen.
;; king simple moves are just similar to knight.
