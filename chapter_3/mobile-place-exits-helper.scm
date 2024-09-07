;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bidirectional-exit
;; As SICP says, abstraction using constructor and selector.
(define (create-bidirectional-exit outside outside-to-mobile-place-direction mobile-place)
  (cons 
    (create-mobile-exit
      outside 
      outside-to-mobile-place-direction 
      mobile-place)
    (create-mobile-exit
      mobile-place
      (reverse-direction outside-to-mobile-place-direction)
      outside)))

(define (outside-to-mobile-place bidirectional-exit)
  (car bidirectional-exit))

(define (mobile-place-to-outside bidirectional-exit)
  (cdr bidirectional-exit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; 
(define mobile-exit?
  (make-type 'mobile-exit '()))
(set-predicate<=! mobile-exit? exit?)

(define make-mobile-exit
  (type-instantiator mobile-exit?))

(define (create-mobile-exit from direction to)
  (make-mobile-exit 'name 'mobile-exit
             'from from
             'direction direction
             'to to))

;; to explicitly add by add-mobile-place-bidirectional-exit.
(define-generic-procedure-handler set-up! (match-args mobile-exit?)
  (lambda (super mobile-exit)
    (super mobile-exit)
    (remove-exit! (get-from mobile-exit) mobile-exit)))

;; TODO how to define one bidirectional relation more elegantly?
(define (reverse-direction direction)
  (case direction
    ((up) 'down)
    ((down) 'up)
    ((east) 'west)
    ((west) 'east)
    ))

(define (fpde-outward-direction fpde)
  (guarantee fpde? fpde)
  (reverse-direction (fpde-inward-direction fpde)))

;; searcher
(define (find-outward-exit-by-floor-direction floor-num direction mobile-place person)
  (let ((fpde-cand 
          (find
            (lambda (fpde) (equal? direction (reverse-direction (fpde-inward-direction fpde))))
            (floor-fpdes floor-num mobile-place))))
    (if fpde-cand
      (mobile-place-to-outside (fpde-bidirectional-exit fpde-cand))
      ;; same as `(go direction)`.
      (narrate! (list "No exit in" direction "direction") person))))