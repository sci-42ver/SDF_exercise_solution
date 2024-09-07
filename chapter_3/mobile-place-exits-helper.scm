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

(define-record-type <person-direction>
  (%make-person-direction person direction)
  person-direction?
  (direction pd-direction)
  (person pd-person))

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

;; state updater
(define (init-exits mobile-place)
  (for-each
    (lambda (fpde) 
      (set-fpde-bidirectional-exit!
        fpde
        (create-bidirectional-exit (fpde-place fpde) (fpde-inward-direction fpde) mobile-place)))
    (all-possible-dests mobile-place)))

;; Trivially, mobile-place like elevator should allow both inward and outward operations..
; (define (add-mobile-place-entrance-exits mobile-place)
;   (for-each
;     (lambda (fpde)
;       ;; notice this uses property-adder -> (lset-adjoin eqv? ...), so no duplicity.
;       (add-exit! (fpde-place fpde) (outside-to-mobile-place (fpde-bidirectional-exit fpde))))
;     (get-mobile-entrances mobile-place)))

(define (add-mobile-place-bidirectional-exit floor-num mobile-place)
  (for-each
    (lambda (fpde) 
      (let ((outside (fpde-place fpde))
            (bidirectional-exit-pair (fpde-bidirectional-exit fpde))
            )
        (add-exit! outside (outside-to-mobile-place bidirectional-exit-pair))
        (add-exit! mobile-place (mobile-place-to-outside bidirectional-exit-pair)))
    (floor-fpdes floor-num mobile-place))))

(define remove-exit!
  (property-remover place:exits place? exit?))

(define (remove-mobile-place-inward-exits floor-num mobile-place)
  (for-each
    (lambda (fpde)
      (remove-exit! (fpde-place fpde) (outside-to-mobile-place (fpde-bidirectional-exit fpde))))
    (floor-fpdes floor-num mobile-place)))

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