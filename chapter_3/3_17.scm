(cd "~/SICP_SDF/SDF_exercises/chapter_3")
(load "../software/sdf/manager/load.scm")
(manage 'new 'user-defined-types)

(define *max-health* 9)

;; Since here avatar? will <= both old person? and the new one.
;; So the old person:health is still avatar's property.
;; renaming to be more readable.
(define person:scaled-health
  (make-property 'scaled-health
                 'predicate n:exact-integer?
                 ;; >  have more possible values
                 'default-value *max-health*))

(define get-health
  (property-getter person:scaled-health person?))

(define set-health!
  (property-setter person:scaled-health person? any-object?))

(define *max-rest-cycle* 3)

(define person:rest-cycle
  (make-property 'rest-cycle
                 'predicate (lambda (cycle) (n:<= 0 cycle (- *max-rest-cycle* 1)))
                 'default-value 0))

(define get-rest-cycle
  (property-getter person:rest-cycle person?))

(define set-rest-cycle!
  (property-setter person:rest-cycle person? number?))

(define (increment-rest-cycle! person)
  (guarantee person? person)
  (set-rest-cycle! person (n:+ 1 (get-rest-cycle person)))
  (if (n:= (get-rest-cycle person) *max-rest-cycle*)
    (begin
      (set-health! person (n:+ 1 (get-health person)))
      (set-rest-cycle! person 0)
      (narrate!
        (list person "health is added by 1")
        person))))

(define person?
  (make-type 'person (list person:scaled-health person:bag person:rest-cycle)))

(load "person-lib.scm")
; ((type-instantiator person?)
;   'name 'anonymous
;   'location (car (create-mit))
;   'screen (make-screen 'name 'the-screen))
; (make-avatar 
;   'name 'anonymous
;   'location (car (create-mit))
;   'screen (make-screen 'name 'the-screen))

(define (eat-people! troll)
  (if (flip-coin (get-hunger troll))
    (let ((people (people-here troll)))
      (if (n:pair? people)
        (let ((victim (random-choice people)))
          (narrate! (list troll "takes a bite out of" victim)
                    troll)
          ;; only do this change
          ;; > the probability of death from a troll bite is the same 
          (suffer! (random-number *max-health*) victim))
        (narrate! (list (possessive troll) "belly rumbles")
                  troll)))))

;; Here we don't need to reload `(define-clock-handler troll? eat-people!)` since the handler is `(lambda (super object) ...)` which will use the new `eat-people!`.

;; > make it possible to recover from a nonfatal troll bite, or other loss of health, by some cycles of rest.
;; mimic `enter-place!` with (match-args avatar?).
(define rest!
  (most-specific-generic-procedure 'rest! 1
    (constant-generic-procedure-handler #f)))
(define-generic-procedure-handler rest!
  (match-args person?)
  (lambda (person)
    (narrate! (list person "rests at" (get-location person))
              person)
    (increment-rest-cycle! person)))

;; test
; (for-each
;   (lambda (rule) 
;     (pp (cdr rule)))
;   (generic-procedure-rules enter-place!))
(assert (n:= 2 (length (generic-procedure-rules enter-place!))))

(define (create-trolls places)
  (map (lambda (name)
         (create-troll name
                       (random-choice places)
                       (random-bias 3)
                       0 ; Only change. To accelerate eat-people!.
                       ))
       '(grendel registrar)))

(define (what-to-do person)
  (define (tell-health)
    (tell! (list (local-possessive person) "health is" (get-health person)) person))
  (tell-health)
  (do ()
    ((n:< (get-health person) *max-health*)
      (displayln "got eaten")
      (tell-health)
      (go (get-direction (random-choice (exits-here person))))
      (loop-cnt (lambda () (rest! person)) 3)
      (tell-health)
      )
    (displayln "wait to be eaten")
    (hang-out 1))
  )
(define (pred)
  (not (null? (filter troll? (people-here my-avatar)))))
(load "adventure-lib.scm")
(restart-game-until-pred pred 'anonymous what-to-do)