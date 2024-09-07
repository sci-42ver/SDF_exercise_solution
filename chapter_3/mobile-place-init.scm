(define-generic-procedure-handler set-up! (match-args mobile-place?)
  (lambda (super mobile-place)
    (super mobile-place)
    (init-exits mobile-place)
    ;; Here initially the game may have person in mobile-place.
    ;; > entrances and exits that change with time
    (add-mobile-place-bidirectional-exit (get-current-floor mobile-place) mobile-place)
    (register-with-clock! mobile-place (get-clock))
    ))

;; See start-adventure where all-places are set up before all-people and my-avatar.
;; Based on register-with-clock!, (clock-things (get-clock)) will be (append (list my-avatar) all-people all-places).
;; See (tick! clock) where people will do before places.
;; So here it is fine for mobile-place to move since all people has moved, otherwise it needs to wait for people to go in.
(define (mobile-place-move! mobile-place)
  (if (n:pair? (get-floor-pds-lst mobile-place))
    (let (
          ; (old-mobile-entrances (get-mobile-entrances mobile-place))
          (mobile-place-speed (get-speed mobile-place)))
      (close-mobile-place mobile-place)
      (narrate! (list mobile-place "is closed and will move")
            mobile-place)
      ;; Here assume tick is the minimal unit. So if mobile-place moves multiple floors in one tick, then it can't let people go in for the intermediate floors.
      (update-floor mobile-place)
      (narrate! (list mobile-place "has moved with exits updated")
            mobile-place)
      (people-go-out floor-range mobile-place)
      (narrate! (list "People have left" mobile-place "when necessary")
            mobile-place)
      )))
(define-clock-handler mobile-place? mobile-place-move!)