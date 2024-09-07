(load "../common-lib/utils.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; init func
(define (restart-game-until-pred pred my-name what-to-do)
  (start-adventure my-name)
  
  (do ()
    ;; see `(look-around avatar)` -> `(display-message message port)` where `(get-name item)` is to get the string name
    ;; and `(create-mit)`.
    ((pred)
      (what-to-do my-avatar)
      )
    (displayln "start one new adventure")
    (start-adventure my-name)))

(define (start-adventure-with-troll-place-and-mine my-name troll-place-name my-place-name)
  (define (create-specific-trolls troll-place)
    (assert troll-place)
    (map (lambda (name)
          (create-troll name
                        troll-place
                        (random-bias 3)
                        0))
        '(registrar)))
  (define (create-people places)
    (displayln "call local create-people")
    (append (create-students places)
            (create-house-masters places)
            (create-specific-trolls 
              (find-place-name troll-place-name places))))
  (set! the-clock (make-clock))
  (set! all-places (create-mit))
  ; (display (map get-name all-places))
  (set! heaven (create-place 'heaven))
  (set! all-people (create-people all-places))
  (set! my-avatar
        (create-avatar my-name
                       (find-place-name my-place-name all-places)))
  (whats-here))

(define (start-adventure-with-troll-place-and-mine* my-name troll-place-name my-place-name create-things-handler)
  (define (create-specific-trolls troll-place)
    (assert troll-place)
    (map (lambda (name)
          (create-troll name
                        troll-place
                        (random-bias 3)
                        0))
        '(registrar)))
  (define (create-people places)
    (displayln "call local create-people")
    (append (create-students places)
            (create-house-masters places)
            (create-specific-trolls 
              (find-place-name troll-place-name places))))
  (set! the-clock (make-clock))
  (set! all-places (create-mit-mod))
  (create-things-handler all-places)
  ; (display (map get-name all-places))
  (set! heaven (create-place 'heaven))
  (set! all-people (create-people all-places))
  (set! my-avatar
        (create-avatar my-name
                       (find-place-name my-place-name all-places)))
  (whats-here))

(define (start-adventure-with-troll-place-and-mine** my-name troll-place-name my-place-name create-mit-handler create-places-handler)
  (define (create-specific-trolls troll-place)
    (assert troll-place)
    (map (lambda (name)
          (create-troll name
                        troll-place
                        (random-bias 3)
                        0))
        '(registrar)))
  (define (create-people places)
    (displayln "call local create-people")
    (append (create-students places)
            (create-house-masters places)
            (create-specific-trolls 
              (find-place-name troll-place-name places))))
  (set! the-clock (make-clock))
  (set! all-places (create-mit-handler))
  (set! all-places (append all-places (create-places-handler all-places)))
  (set! heaven (create-place 'heaven))
  (set! all-people (create-people all-places))
  (set! my-avatar
        (create-avatar my-name
                       (find-place-name my-place-name all-places)))
  (whats-here))

(define (create-mit-mod)
  (displayln "call local (create-mit)")
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
          great-court ; weird not included in the original code.
          dorm-row)))

(define (create-mit-3-21)
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

        (32-123 (create-place '32-123))
        (32G (create-place 'gates-tower))
        (32G-library (create-place 'gates-tower-library))
        (32G-skywalk (create-place 'gates-tower-skywalk))
        (32G-bcp (create-place 'gates-tower-basement-car-park)) ; added

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
          great-court ; weird not included in the original code.
          32G-library 32G-skywalk 32G-bcp ; added
          dorm-row)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; helper
(define (tell-health object)
  (tell! (list (local-possessive object) "health is" (get-health object)) object))

;; searcher
(define (find-place-name place-name places)
  (find 
    (lambda (place) (equal? (get-name place) place-name))
    places))

;; updater
(define (increment-decrement-property! object-pred object get-handler set-handler! update-rest-handler increment-step op #!optional max-value)
  (guarantee object-pred object)
  (set-handler! object (op increment-step (get-handler object)))
  (and
    (not (default-object? max-value))
    (n:>= (get-handler object) max-value)
    (set-handler! object 0)
    (update-rest-handler object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; copy of original with small modifications
(define (pure-take-exit! exit mobile-thing)
  (pure-move-internal! mobile-thing
                 (get-from exit)
                 (get-to exit)
                 mobile-thing))

(define (pure-move-internal! mobile-thing from to)
  (leave-place! mobile-thing)
  (remove-thing! from mobile-thing)
  (set-location! mobile-thing to)
  (add-thing! to mobile-thing))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;