(cd "~/SICP_SDF/SDF_exercises/chapter_3")
(load "../software/sdf/manager/load.scm")
(manage 'new 'user-defined-types)
(load "adventure-lib.scm")

(define (create-mit)
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

    ;; added
    ;; > Plant a few immovable palantiri in various parts of the campus
    (create-thing 'palantir 10-250)
    (create-thing 'palantir great-court)
    (create-thing 'palantir the-dot)

    (list great-dome little-dome lobby-10
          10-250 barker-library lobby-7
          infinite bldg-26 cp32
          tunnel 32-123 32D 32G
          student-street bldg-54 the-dot
          great-court ; weird not included in the original code.
          dorm-row)))

(define (find-thing-no-tell name person-or-place)
  (find-object-by-name
    name
    (person-or-place-things person-or-place)))

(define (all-palantir-places)
  (filter
    (lambda (place) (find-thing-no-tell 'palantir place))
    all-places))

;; see look-around
(define (look-around-palantir person)
  (define (view-place place)
    ;; here other places have things not including self. So output all things without using things-in-place and people-in-place.
    (let ((things (get-things place)))
      (if (n:pair? things)
          (tell! (cons "You see:" (append things (list "at" place)))
                person)))
    (let ((vistas (get-vistas place)))
      (if (n:pair? vistas)
          (tell! (cons "You can see:" (append vistas (list "at" place)))
                person))))
  (general-look-around-palantir person view-place #f))
(define (general-look-around-palantir person place-handler allow-cur-place)
  (for-each
    ;; > communicate with any other instance;
    (lambda (place) (place-handler place))
    (if allow-cur-place
      (all-palantir-places)
      (delv (get-location person) (all-palantir-places)))))

(define (check-palantir! person)
  (if 
    (find-thing-no-tell 'palantir (get-location person))
    (look-around-palantir person)
    (tell! (list "No palantir here") person)))

;; > enable your avatar to use one
;; > Can you keep watch on the ... Of the trolls?
;; Yes since great-court has one palantir.
(start-adventure-with-troll-place-and-mine 'anonymous 'great-court '10-250)
(check-palantir! my-avatar)

;; > Can you make an autonomous person other than your avatar use a palantir for some interesting purpose?
;; Just tell all students where trolls are to help them survive.
(load "president-lib.scm")
(create-presidents 'the-dot)
(define (look-around-palantir-for-type! type-pred person)
  (define (notify-person-loc place)
    ;; here other places have things not including self. So output all things without using things-in-place and people-in-place.
    (displayln (list "run notify-person-loc at " (get-name place)))
    (let ((things (filter type-pred (get-things place))))
      (if (n:pair? things)
          (begin
            (displayln "find troll")
            (for-each 
              (lambda (person) 
                (tell! 
                  (list 
                    "There is a" 
                    (symbol->string (tag-name (predicate->tag type-pred))) 
                    "at" 
                    place) 
                  person))
              (filter 
                (lambda (person) (or (student? person) (avatar? person)))
                (cons my-avatar all-people)))))))
  (general-look-around-palantir person notify-person-loc #t))
(define (check-palantir-for-troll! person)
  (displayln (list "check-palantir-for-troll!" (get-name (get-location person))))
  (if 
    (find-thing-no-tell 'palantir (get-location person))
    (look-around-palantir-for-type! troll? person)))
(define-clock-handler president? check-palantir-for-troll!)

(go 'up)
; (let ((cur-exits (exits-here my-avatar)))
;   (if (not (null? cur-exits))
;     (go (get-direction (random-choice cur-exits)))))
