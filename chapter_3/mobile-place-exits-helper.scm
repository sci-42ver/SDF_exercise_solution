;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bidirectional-exit
;; As SICP says, abstraction using constructor and selector.
(define (create-bidirectional-exit outside outside-to-mobile-place-direction mobile-place)
  (cons 
    (create-exit 
      outside 
      outside-to-mobile-place-direction 
      mobile-place)
    (create-exit 
      mobile-place
      (reverse-direction outside-to-mobile-place-direction)
      outside)))

(define (outside-to-mobile-place bidirectional-exit)
  (car bidirectional-exit))

(define (mobile-place-to-outside bidirectional-exit)
  (cdr bidirectional-exit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

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