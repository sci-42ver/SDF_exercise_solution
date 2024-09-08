;; By searching "palantir" with "*.rkt,*.scm" in VSCode, no sample implementation.
(cd "~/SICP_SDF/SDF_exercises/chapter_3")
(load "../software/sdf/manager/load.scm")
(manage 'new 'user-defined-types)
(load "section-3-5-lib/adventure-lib.scm")

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

(define (things-handler all-places)
  ;; > Plant a few immovable palantiri in various parts of the campus
  (for-each 
    (lambda (name) (create-thing 'palantir (find-place-name name all-places)))
    '(10-250 great-court the-dot)))

;; > enable your avatar to use one
;; > Can you keep watch on the ... Of the trolls?
;; Yes since great-court has one palantir.
(start-adventure-with-troll-place-and-mine* 'anonymous 'great-court '10-250 things-handler)
(check-palantir! my-avatar)

;; > Can you make an autonomous person other than your avatar use a palantir for some interesting purpose?
;; Just tell all students where trolls are to help them survive.
(load "section-3-5-lib/president-lib.scm")
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

(let ((cur-exits (exits-here my-avatar)))
  (if (not (null? cur-exits))
    (go (get-direction (random-choice cur-exits)))))
