;; IMHO this exercise is more about how to define the logic of elevator.
;; It is more about the overall design where we need to consider what data types to use (IMHO this is the difficult part).

;; I won't deal with 3 TODO's here since that has little relation with programming strategies.
;; Also see mobile-place-exits-helper.scm.
(cd "~/SICP_SDF/SDF_exercises/chapter_3")
(load "../software/sdf/manager/load.scm")
(manage 'new 'user-defined-types)
(load "section-3-5-lib/adventure-lib.scm")

;; Here I only implement "elevator" as one demo.

;; See updater: We need to update mobile-place properties (moving-direction, current-floor, floor-pds-lst)
;; and exits (notice this is bidirectional including the book "entrances and exits") related when necessary.
(cd "3-21-lib")
(load "3-21-record-lib.scm")
(load "mobile-place-type-lib.scm")
(load "mobile-place-misc-lib.scm")
(load "mobile-place-updater-lib.scm")
(load "mobile-place-exits-helper.scm")
(load "mobile-place-exits-updater.scm")

;; put this before using the new person? to avoid adding 2 handlers.
;; notice avatar needs to set-dest-floor-direction! explicitly if avatar wants to go to one specific place.
(define-generic-procedure-handler enter-place!
  (match-args person?)
  (lambda (super person)
    (super person)
    (let ((cur-loc (get-location person)))
      (narrate! (list person "enters" cur-loc)
                person)
      ;; added
      (if (mobile-place? cur-loc)
        (let* ((dest-fd (get-dest-floor-direction person))
               (dest-floor (fd-floor-num dest-fd)))
          (if (= 0 dest-floor)
            (let* ((rand-floor (random-floor cur-loc))
                   (dir (random-floor-outward-direction rand-floor cur-loc))
                   (fd (%make-floor-direction rand-floor dir)))
              (set-dest-floor-direction! person fd)))
          (press-moving-button person)))
      )
    (let ((people (people-here person)))
      (if (n:pair? people)
          (say! person (cons "Hi" people))))))
(load "3-21-person-lib.scm")
(load "../section-3-5-lib/person-lib.scm")
(load "mobile-place-init.scm")
(cd "..")

(define (press-moving-button-of-mobile-place person mobile-place)
  (let* ((fd (get-dest-floor-direction person))
         (pd (%make-person-direction person (fd-direction fd)))
         (fp-lst (get-floor-pds-lst mobile-place))
         (floor-num (fd-floor-num fd))
         (floor-pds-entry
          (find 
            (lambda (entry) (= floor-num (fp-floor-num entry))) 
            fp-lst)))
    (if floor-pds-entry
      (set-fp-pds! floor-pds-entry (cons pd (fp-pds floor-pds-entry)))
      ;; TODO equal-floor-pds? no use here to differentiate.
      (set-floor-pds-lst! mobile-place (lset-adjoin equal-floor-pds? fp-lst (%make-floor-pds floor-num (list pd)))))
    ; (displayln (list "floor-pds-lst mobile-place:" (map length (map fp-pds (get-floor-pds-lst mobile-place)))))
    ))

(define (press-moving-button person)
  (if (mobile-place? (get-location person))
    (press-moving-button-of-mobile-place person (get-location person))
    (tell! (list "You need to go into mobile-place first") person)))

;; main part
(define (places-handler all-places)
  (list
    (create-mobile-place 
      'elevator
      (list
        ;; Here '() needs to be reset by `init-exits`.
        (%make-fpde 1 (find-place-name 'student-street all-places) 'east '())
        (%make-fpde 1 (find-place-name 'gates-tower all-places) 'west '())
        (%make-fpde 4 (find-place-name 'gates-tower-skywalk all-places) 'east '())
        (%make-fpde 4 (find-place-name 'gates-tower-library all-places) 'west '())
        ;; TODO better make floor continuous, so we can make base floor 0.
        (%make-fpde -1 (find-place-name 'gates-tower-basement-car-park all-places) 'west '())
        ))))

(start-adventure-with-troll-place-and-mine**
  'anonymous 
  'bldg-26
  'elevator
  create-mit-3-21
  places-handler)

(set-go-dest-in-mobile-place my-avatar 'gates-tower-skywalk 'elevator)
(press-moving-button my-avatar)
;; TODO order should be person->elevator.
;; > have ... exits that change with time
;; only after lift can anonymous go to "gates-tower-skywalk".
(hang-out 3)
; anonymous goes from elevator to gates-tower-skywalk

(set-go-dest-in-mobile-place my-avatar 'student-street 'elevator)
;; > have entrances ... that change with time
(go 'east)
; anonymous leaves via the east exit from gates-tower-skywalk
; anonymous enters elevator

; (press-moving-button my-avatar) ; already done in go->tick!.
(hang-out 3)