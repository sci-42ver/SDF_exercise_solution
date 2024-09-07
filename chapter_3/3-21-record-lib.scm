;; used by mobile-place-type-lib.scm
(define-record-type <fpde>
  (%make-fpde floor-num place inward-direction exit)
  fpde?
  (floor-num fpde-floor-num)
  (place fpde-place)
  (inward-direction fpde-inward-direction)
  (exit fpde-bidirectional-exit set-fpde-bidirectional-exit!))

(define-record-type <floor-pds>
  (%make-floor-pds floor-num people-directions)
  floor-pds?
  (people-directions fp-pds set-fp-pds!)
  (floor-num fp-floor-num))

(define (equal-floor-pds? floor-pds-1 floor-pds-2)
  (and
    (floor-pds? floor-pds-1)
    (floor-pds? floor-pds-2)
    (apply = (map floor-num (list floor-pds-1 floor-pds-2)))
    (every equal-person-direction? (map fp-pds (list floor-pds-1 floor-pds-2)))
    ))

;; used by mobile-place-exits-helper.scm
(define-record-type <person-direction>
  (%make-person-direction person direction)
  person-direction?
  (direction pd-direction)
  (person pd-person))

(define (equal-person-direction? pd-1 pd-2)
  (and
    (person-direction? pd-1)
    (person-direction? pd-2)
    (apply equal? (map direction (list pd-1 pd-2)))
    (apply equal? (map person (list pd-1 pd-2)))
    ))

;; used by 3-21-person-lib.scm
(define-record-type <floor-direction>
  (%make-floor-direction floor-num direction)
  floor-direction?
  (direction fd-direction)
  (floor-num fd-floor-num))