(cd "~/SICP_SDF/SDF_exercises/chapter_3")
(load "../software/sdf/manager/load.scm")
(manage 'new 'user-defined-types)
(load "adventure-lib.scm")

;; Here I only implement "elevator" as one demo.
;; We need to update mobile-place properties (mobile-entrances, mobile-destinations, floor-pds-lst)
;; when necessary.
(load "mobile-place-type-lib.scm")
(load "mobile-place-misc-lib.scm")
(load "mobile-place-exits-helper.scm")
(load "3-21-person-lib.scm")
(load "person-lib.scm")

;; main part
(define (things-handler all-places)
  (create-mobile-place 
    'elevator
    (list
      ;; Here '() needs to be reset by `init-exits`.
      (%make-fpde 1 (find-place-name 'student-street all-places) 'east '())
      (%make-fpde 1 (find-place-name 'gates-tower all-places) 'west '())
      (%make-fpde 4 (find-place-name 'gates-tower-skywalk all-places) 'east '())
      (%make-fpde 4 (find-place-name 'gates-tower-library all-places) 'west '())
      (%make-fpde -1 (find-place-name 'gates-tower-basement-car-park all-places) 'west '())
      )))

(define-generic-procedure-handler set-up! (match-args mobile-place?)
  (lambda (super mobile-place)
    (super mobile-place)
    ; (for-each
    ;   (lambda (place) (add-thing! place mobile-place))
    ;   (map 
    ;     fpde-place 
    ;     (all-possible-dests mobile-place)))
    (init-exits mobile-place)
    ;; Here initially the game may have person in mobile-place.
    (add-mobile-place-bidirectional-exit (get-current-floor mobile-place) mobile-place)))

(define (press-moving-button-of-mobile-place person fd mobile-place)
  (let* ((pd (%make-person-direction person (fd-direction fd)))
         (fp-lst (get-floor-pds-lst mobile-place))
         (floor-num (fd-floor-num fd))
         (floor-pds-entry
          (find 
            (lambda (entry) (= floor-num (fp-floor-num entry))) 
            fp-lst)))
    (if floor-pds-entry
      (set-cdr! floor-pds-entry (cons pd (cdr floor-pds-entry)))
      (set-floor-pds-lst! mobile-place (cons (%make-floor-pds floor-num (list pd)) fp-lst)))))

;; notice avatar needs to set-dest-floor-direction! explicitly if avatar wants to go to one specific place.
(define-generic-procedure-handler enter-place!
  (match-args person?)
  (lambda (super person)
    (super person)
    (let ((cur-loc (get-location person)))
      (narrate! (list person "enters" cur-loc)
                person)
      (if (mobile-place? cur-loc)
        (let* ((dest-fd (get-dest-floor-direction person))
               (dest-floor (fd-floor-num dest-fd)))
          (if (= 0 dest-floor)
            (let* ((rand-floor (random-floor cur-loc))
                   (dir (random-floor-outward-direction rand-floor cur-loc))
                   (fd (%make-floor-direction rand-floor dir))
              (set-dest-floor-direction! person fd))))
          (press-moving-button-of-mobile-place person fd cur-loc))))
    (let ((people (people-here person)))
      (if (n:pair? people)
          (say! person (cons "Hi" people))))))

; (define (take-mobile-place person mobile-place-name destination-name)
;   (let* ((mobile-place (find-object-by-name mobile-place-name (things-here person)))
;         (destination 
;           (find-object-by-name 
;             destination-name
;             (map fpde-place (get-mobile-destinations mobile-place))))
;         (cur-loc (get-location person))
;         (cur-floor (fpde-floor-num (get-fpde-by-entrance-exit-place-name (get-name (get-location person)) mobile-place #t)))
;         (dest-floor (fpde-floor-num (get-fpde-by-entrance-exit-place-name destination-name mobile-place #f)))
;         (old-mobile-entrances (get-mobile-entrances mobile-place))
;         )
;     (if 
;       (and
;         mobile-place 
;         cur-floor
;         dest-floor)
;       (let ((move-time 
;               (ceiling->exact 
;                 (/ (abs (- dest-floor cur-floor)) (get-speed mobile-place)))))
;         ;; mimic take-exit! -> generic-move! -> move-internal!
;         (begin
;           (generic-move!
;             person
;             cur-loc
;             mobile-place
;             person)
;           ()
;           (set-mobile-mobile-entrances! mobile-place '())
;           ;; for simplicity, assume person just waits until getting to destination.
;           (hang-out move-time)
;           )))))

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
      (narrate! (list mobile-place "has moved with entrances, destinations and exits updated")
            mobile-place)
      (people-go-out floor-range mobile-place)
      (narrate! (list "People have left" mobile-place "when necessary")
            mobile-place)
      )
    (begin
      )))

(define-generic-procedure-handler set-up!
  (match-args mobile-place?)
  (lambda (super thing)
    (super thing)
    (set-accessible-floors! 
      mobile-place 
      (map
        fpde-floor-num
        ; (append (get-mobile-entrances mobile-place) (get-mobile-destinations mobile-place))
        (get-possible-fpdes mobile-place)
        ))
    (register-with-clock! thing (get-clock))))
(define-clock-handler mobile-place? mobile-place-move!)

(start-adventure-with-troll-place-and-mine* 
  'anonymous 
  'bldg-26
  '10-250
  things-handler)