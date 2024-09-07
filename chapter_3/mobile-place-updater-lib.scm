;; state updater
(define (close-mobile-place mobile-place)
  (remove-mobile-place-inward-exits! (get-current-floor mobile-place) mobile-place)
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
      ;; > are perhaps controllable by persons
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

(define (set-go-dest-in-mobile-place person dest-place-name mobile-place-name)
  (let ((mobile-place (find-place-name mobile-place-name all-places)))
    (if mobile-place
      (let ((fpde (find-fpde-for-mobile-place dest-place-name mobile-place)))
        (if fpde
          (set-dest-floor-direction! 
            person 
            (%make-floor-direction
              (fpde-floor-num fpde)
              (reverse-direction (fpde-inward-direction fpde))))
          (tell! (list dest-place-name "doesn't exist for" mobile-place) person)))
      (tell! (list mobile-place-name "doesn't exist") person))))