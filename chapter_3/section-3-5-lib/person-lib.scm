;; just used to reload.
;; We don't need to reload `when-alive` since that is one lambda func where `get-health` will auto use the latest func when called.
(set-predicate<=! person? mobile-thing?)

;; IMHO, here we don't change person:bag implementation, so for each person subset type, it can use the old  person handler.
; (define get-bag
;   (property-getter person:bag person?))

; (define-generic-procedure-handler set-up! (match-args person?)
;                                   (lambda (super person)
;                                     (super person)
;                                     (set-holder! (get-bag person) person)))

; (define-generic-procedure-handler get-things (match-args person?)
;                                   (lambda (person)
;                                     (get-things (get-bag person))))

; (define-generic-procedure-handler enter-place!
;                                   (match-args person?)
;                                   (lambda (super person)
;                                     (super person)
;                                     (narrate! (list person "enters" (get-location person))
;                                               person)
;                                     (let ((people (people-here person)))
;                                       (if (n:pair? people)
;                                         (say! person (cons "Hi" people))))))

;; to make other subtypes work by inheriting modified "property"s.
(set-predicate<=! autonomous-agent? person?)
(set-predicate<=! avatar? person?)

;; reload type-properties.
(define make-avatar
  (type-instantiator avatar?))
(define make-student
  (type-instantiator student?))
(define make-house-master
  (type-instantiator house-master?))
(define make-troll
  (type-instantiator troll?))