(cd "~/SICP_SDF/SDF_exercises/chapter_3")
(load "../software/sdf/manager/load.scm")
(manage 'new 'user-defined-types)
(load "adventure-lib.scm")

;; See "medical-center" for what is added.
(define (create-mit)
  (let ((great-dome (create-place 'great-dome))
        (little-dome (create-place 'little-dome))
        (lobby-10 (create-place 'lobby-10))
        (10-250 (create-place '10-250))
        (barker-library (create-place 'barker-library))
        (lobby-7 (create-place 'lobby-7))
        (infinite (create-place 'infinite-corridor))

        (bldg-26 (create-place 'bldg-26))
        (cp32 (create-place 'bldg-32-cp-hq))
        (tunnel (create-place 'lab-supplies))

        (medical-center (create-place 'medical-center)) ; added

        (32-123 (create-place '32-123))
        (32G (create-place 'gates-tower))
        (32D (create-place 'dreyfoos-tower))
        (student-street (create-place 'student-street))
        (great-court (create-place 'great-court))
        (bldg-54 (create-place 'green-building))
        (the-dot (create-place 'the-dot))
        (dorm-row (create-place 'dorm-row)))

    (can-go-both-ways lobby-10 'up 'down 10-250)
    (can-go-both-ways 10-250 'up 'down barker-library)
    (can-go-both-ways barker-library 'up 'down great-dome)
    (can-go-both-ways lobby-10 'west 'east lobby-7)
    (can-go-both-ways lobby-7 'west 'east dorm-row)
    (can-go-both-ways lobby-7 'up 'down little-dome)
    (can-go-both-ways lobby-10 'south 'north great-court)
    (can-go-both-ways lobby-10 'east 'west infinite)
    (can-go-both-ways infinite 'north 'south bldg-26)
    (can-go-both-ways infinite 'east 'west bldg-54)
    (can-go-both-ways bldg-26 'east 'west student-street)
    (can-go-both-ways student-street 'down 'up cp32)
    (can-go-both-ways cp32 'south 'north tunnel)
    (can-go-both-ways tunnel 'up 'down bldg-54)
    (can-go-both-ways bldg-54 'south 'north the-dot)
    (can-go-both-ways the-dot 'west 'east great-court)
    (can-go-both-ways student-street 'in 'out 32-123)
    (can-go-both-ways student-street 'up 'down 32G)
    (can-go-both-ways student-street 'skew 'down 32D)

    ;; > Make it easily accessible from the Green building and the Gates tower.
    ;; bldg-54 has west, down, south  already with accessible places.
    ;; 32G: down
    (can-go-both-ways medical-center 'east 'west 32G)
    (can-go-both-ways bldg-54 'east 'west medical-center)

    ; Add line-of-sight into the mix
    (can-see bldg-54 32G)
    (can-see bldg-54 32D)
    (can-see bldg-54 great-dome)
    (can-see bldg-54 little-dome)
    (can-see bldg-54 great-court)
    (can-see bldg-54 the-dot)
    (can-see lobby-10 great-court)
    (can-see great-dome great-court)
    (can-see-both-ways 32D 32G)
    (can-see-both-ways great-dome little-dome)
    (can-see-both-ways lobby-10 infinite)
    (can-see-both-ways lobby-7 infinite)
    (can-see-both-ways infinite bldg-26)
    (can-see-both-ways lobby-10 lobby-7)

    ; Create some things
    (create-thing 'blackboard 10-250)
    (create-thing 'lovely-trees great-court)
    (create-thing 'flag-pole great-court)
    (create-thing 'calder-sculpture the-dot)
    (create-mobile-thing 'problem-set 32-123)
    (create-mobile-thing 'recitation-problem 32-123)
    (create-mobile-thing 'sicp student-street)
    (create-mobile-thing 'engineering-book barker-library)

    (list great-dome little-dome lobby-10
          10-250 barker-library lobby-7
          infinite bldg-26 cp32
          tunnel 32-123 32D 32G
          student-street bldg-54 the-dot
          medical-center
          dorm-row)))

;; This will be called in (tick! (get-clock)) -> move! -> generic-move! (person? place? place? person?) -> move-internal!.
;; or by go of avatar -> take-exit! -> generic-move! ...
(define *max-health* 3)
(define-generic-procedure-handler enter-place!
  (match-args person?)
  (lambda (super person)
    (super person)
    (narrate! (list person "enters" (get-location person))
              person)
    (if (equal? 'medical-center (get-name (get-location person)))
      (begin
        (narrate! (list (local-possessive person) "health is restored.")
              person)
        (set-health! person *max-health*)
        (tell-health person)))
    (let ((people (people-here person)))
      (if (n:pair? people)
          (say! person (cons "Hi" people))))))

(load "troll-bite-lib.scm")

(define (what-to-do-when-troll-bite person)
  (go (get-direction
    (find-exit 
      (get-location person) 
      (find (lambda (place) (equal? (get-name place) 'medical-center)) all-places))))
  ;; Here restore is done before tick! -> eat-people!.
  (if (n:> (get-health person) 0)
    (begin
      (tell-health person)
      (set! failure-to-survive #f))
    (begin
      (tell! (list person "is unfortunately died after restore.") person)
      (set! failure-to-survive #t))))

(define (what-to-do person)
  (wait-for-troll-bite person what-to-do-when-troll-bite)
  )

(define failure-to-survive #t)
(start-adventure-with-troll-place-and-mine 'anonymous 'green-building 'green-building)
(what-to-do my-avatar)