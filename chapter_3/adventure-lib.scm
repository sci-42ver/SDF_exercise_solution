(load "../common-lib/utils.scm")
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

(define (tell-health person)
  (tell! (list (local-possessive person) "health is" (get-health person)) person))

(define (start-adventure-with-troll-place-and-mine my-name troll-place-name my-place-name)
  (define (create-specific-trolls troll-place)
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
              (find 
                (lambda (place) (equal? (get-name place) troll-place-name))
                places))))
  (set! the-clock (make-clock))
  (set! all-places (create-mit))
  (set! heaven (create-place 'heaven))
  (set! all-people (create-people all-places))
  (set! my-avatar
        (create-avatar my-name
                       (find 
                        (lambda (place) (equal? (get-name place) my-place-name))
                        all-places)))
  (whats-here))