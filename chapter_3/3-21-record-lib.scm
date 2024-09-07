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
  (people-directions fp-pds)
  (floor-num fp-floor-num))

;; used by mobile-place-exits-helper.scm
(define-record-type <person-direction>
  (%make-person-direction person direction)
  person-direction?
  (direction pd-direction)
  (person pd-person))

;; used by 3-21-person-lib.scm
(define-record-type <floor-direction>
  (%make-floor-direction floor-num direction)
  floor-direction?
  (direction fd-direction)
  (floor-num fd-floor-num))