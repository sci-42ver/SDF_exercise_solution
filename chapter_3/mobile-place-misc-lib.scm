;; misc helper
; (define (get-cur-floor mobile-place)
;   (fpde-floor-num (car (get-mobile-entrances mobile-place))))

; (define (all-possible-dests mobile-place)
;   (append (get-mobile-entrances mobile-place) (get-mobile-destinations mobile-place)))

; (define (get-fpde-by-entrance-exit-place-name place-name mobile-place find-entrance?)
;   (find 
;     (lambda (fpde) 
;       (equal? place-name (get-name (fpde-place fpde))))
;     ((if find-entrance?
;       get-mobile-entrances
;       get-mobile-destinations) mobile-place)))

(define (random-floor mobile-place)
  (random-choice (delv (get-current-floor mobile-place) (get-accessible-floors mobile-place))))

(define (floor-fpdes floor-num mobile-place)
  (filter
    (lambda (fpde) (= floor-num (fpde-floor-num fpde)))
    (all-possible-dests mobile-place)))

(define (random-floor-outward-direction floor-num mobile-place)
  (fpde-outward-direction
    (random-choice 
      (floor-fpdes floor-num mobile-place))))

(define (max-floor mobile-place)
  (apply max (get-accessible-floors mobile-place)))

(define (min-floor mobile-place)
  (apply min (get-accessible-floors mobile-place)))

(define (in-range elem range)
  (n:< (car range) elem (cdr range)))

;; state updater
(define (close-mobile-place mobile-place)
  (remove-mobile-place-inward-exits (get-current-floor mobile-place) mobile-place)
  (set-mobile-entrances! mobile-place '()))

(define (update-floor mobile-place)
  (let ((max-floor-num (max-floor mobile-place))
        (min-floor-num (min-floor mobile-place)))
    (define (update-rest-handler mobile-place)
      (cond 
        ((> (get-current-floor mobile-place) max-floor-num) 
          (set-current-floor! mobile-place max-floor-num)
          (set-moving-direction! mobile-place 'down))
        ((< (get-current-floor mobile-place) min-floor-num) 
          (set-current-floor! mobile-place min-floor-num)
          (set-moving-direction! mobile-place 'up))))
    (let ((speed (get-speed mobile-place)))
      (increment-decrement-property! 
        mobile-place? 
        mobile-place 
        get-current-floor 
        set-current-floor! 
        update-rest-handler
        speed
        (if (up? (get-moving-direction mobile-place)) n:+ n:-)
        (if (up? (get-moving-direction mobile-place)) max-floor-num min-floor-num))
      (add-mobile-place-bidirectional-exit floor-num mobile-place))))

(define (people-go-out floor-range mobile-place)
  (set-floor-pds-lst!
    mobile-place
    (remove
      (lambda (floor-pds)
        (let ((pds (fp-pds floor-pds))
              (floor-num (fp-floor-num floor-pds)))
          (if (in-range floor-num floor-range)
            (begin
              (for-each
                (lambda (pd) 
                  (let ((person (pd-person pd))
                        (direction (pd-direction pd)))
                    (pure-take-exit! (find-outward-exit-by-floor-direction floor-num direction mobile-place person) person)))
                pds)
              #t)
            #f)))
      (get-floor-pds-lst mobile-place))))