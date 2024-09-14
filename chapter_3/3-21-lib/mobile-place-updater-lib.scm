;; state updater
(define (close-mobile-place mobile-place)
  (remove-mobile-place-bidirectional-exits! (get-current-floor mobile-place) mobile-place))

(define (create-range val-1 val-2)
  (if (n:< val-1 val-2)
    (cons val-1 val-2)
    (cons val-2 val-1)))

(define (update-floor mobile-place)
  (let ((max-floor-num (max-floor mobile-place))
        (min-floor-num (min-floor mobile-place)))
    (define (update-rest-handler mobile-place)
      (let ((cur-floor (get-current-floor mobile-place)))
        (display "update-rest-handler")
        (cond 
          ((n:= (get-current-floor mobile-place) max-floor-num)
           (set-moving-direction! mobile-place 'down))
          ((n:= (get-current-floor mobile-place) min-floor-num)
           (set-moving-direction! mobile-place 'up)))
        ))
    (let ((speed (get-speed mobile-place))
          (old-floor (get-current-floor mobile-place)))
      (increment-decrement-property! 
        mobile-place? 
        mobile-place 
        get-current-floor 
        set-current-floor! 
        (if (up? (get-moving-direction mobile-place)) #t #f)
        speed
        (if (up? (get-moving-direction mobile-place)) max-floor-num min-floor-num)
        (if (up? (get-moving-direction mobile-place)) max-floor-num min-floor-num)
        update-rest-handler)
      ;; > are perhaps controllable by persons
      (let ((new-floor (get-current-floor mobile-place)))
        (narrate! (list mobile-place "will add exit with current length" (number->string (length (get-exits mobile-place)))) mobile-place)
        (add-mobile-place-bidirectional-exit new-floor mobile-place)
        (narrate! (list mobile-place "current length:" (number->string (length (get-exits mobile-place)))) mobile-place)
        (create-range old-floor new-floor)))))

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
                    ; (displayln "pure-take-exit!")
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
