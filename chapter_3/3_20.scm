;; By searching "Cloak" with "*.rkt,*.scm" in VSCode, no sample implementation.
(cd "~/SICP_SDF/SDF_exercises/chapter_3")
(load "../software/sdf/manager/load.scm")
(manage 'new 'user-defined-types)
(load "section-3-5-lib/adventure-lib.scm")

;; mimic invisibility-cloak?
(define invisibility-cloak:held-time
  (make-property 'held-time
                 'predicate number?
                 'default-value 0))

(define invisibility-cloak:holder
  (make-property 'holder
                 'predicate
                 (lambda (x) (or (not x) (person? x)))
                 'default-value #f))

(define invisibility-cloak?
  (make-type 'invisibility-cloak (list invisibility-cloak:held-time invisibility-cloak:holder)))
(set-predicate<=! invisibility-cloak? mobile-thing?)

(define make-invisibility-cloak
  (type-instantiator invisibility-cloak?))

(define get-held-time
  (property-getter invisibility-cloak:held-time invisibility-cloak?))

(define set-held-time!
  (property-setter invisibility-cloak:held-time invisibility-cloak? number?))

(define get-cloak-holder
  (property-getter invisibility-cloak:holder invisibility-cloak?))

(define set-cloak-holder!
  (property-setter 
    invisibility-cloak:holder 
    invisibility-cloak? 
    (lambda (object) (or (person? object) (boolean? object)))))

(define (create-invisibility-cloak name location)
  (make-invisibility-cloak 
    'name name
    'location location))

;; 
(define (have-invisibility-cloak? person)
  (find-object-by-name 'invisibility-cloak (get-things person)))

;; main part
(define (things-handler all-places)
  (for-each
    (lambda (name) (create-invisibility-cloak 'invisibility-cloak (find-place-name name all-places)))
    '(10-250 great-court the-dot)))

;; > become invisible, thus invulnerable to attacks
;; Since both look-around and eat-people! uses people-here, I change it to reflect invisibility-cloak.
(define (people-here person)
  (remove
    (lambda (person) (have-invisibility-cloak? person))
    (delv person (people-in-place (get-location person)))))

(define *degrade-time* 3)

(define-generic-procedure-handler enter-place!
                                  (match-args invisibility-cloak?)
                                  (lambda (super cloak)
                                    (super cloak)
                                    (let ((cur-loc (get-location cloak)))
                                      (cond
                                        ((bag? cur-loc) 
                                         (set-cloak-holder! cloak (get-holder cur-loc)))
                                        ((place? cur-loc) 
                                         (set-cloak-holder! cloak #f)
                                         (set-held-time! cloak 0))))))

(define (invisibility-cloak-degrade! invisibility-cloak)
  (define (update-rest-handler invisibility-cloak)
    (let ((holder (get-cloak-holder invisibility-cloak)))
      (set-health! holder (n:- (get-health holder) 1))
      (tell!
        (list (local-possessive holder) "health is degraded by 1")
        holder)
      (tell-health holder)))
  (if (get-cloak-holder invisibility-cloak)
    (increment-decrement-property! 
      invisibility-cloak? 
      invisibility-cloak 
      get-held-time 
      set-held-time! 
      #t
      1
      *degrade-time*
      0
      update-rest-handler
      )))

;; > must be discarded (dropped) after a short *time*
(define-generic-procedure-handler set-up!
                                  (match-args invisibility-cloak?)
                                  (lambda (super thing)
                                    (super thing)
                                    (register-with-clock! thing (get-clock))))
(define-clock-handler invisibility-cloak? invisibility-cloak-degrade!)

(start-adventure-with-troll-place-and-mine* 
  'anonymous 
  '10-250
  '10-250
  things-handler)

(take-thing 'invisibility-cloak)
(hang-out 1)
; > lambda-man says: Hi registrar
; So anonymous is invisible.
(hang-out 2)
(drop-thing 'invisibility-cloak)
(hang-out 3)
(take-thing 'invisibility-cloak)
(hang-out 3)
; > registrar enters |10-250|
; > registrar says: Hi dr-evil
