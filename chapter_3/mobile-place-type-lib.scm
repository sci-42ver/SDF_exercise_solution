;; mimic bag?
;; fpde -> floor-place-direction-exit

;; record
(define-record-type <fpde>
  (%make-fpde floor-num place inward-direction exit)
  fpde?
  (floor-num fpde-floor-num)
  (place fpde-place)
  (inward-direction fpde-inward-direction)
  (exit fpde-bidirectional-exit set-fpde-bidirectional-exit!))

;; 
(define-record-type <floor-pds>
  (%make-floor-pds floor-num people-directions)
  floor-pds?
  (people-directions fp-pds)
  (floor-num fp-floor-num))

;; misc
(define (up? x)
  (equal? x 'up))

(define (down? x)
  (equal? x 'down))

(define (up-down? x)
  (or (up? x) (down? x)))

;; list helper
(define (list-of-fpde? x)
  (and (n:list? x) (every fpde? x)))

(define (list-of-number? x)
  (and (n:list? x) (every number? x)))

(define (list-of-floor-pds? x)
  (and (n:list? x) (every floor-pds? x)))

;; type definition
; (define mobile-place:mobile-entrances
;   (make-property 'mobile-entrances
;                  'predicate list-of-fpde?
;                  'default-value '()))

; (define get-mobile-entrances
;   (property-getter mobile-place:mobile-entrances mobile-place?))

; (define set-mobile-entrances!
;   (property-setter mobile-place:mobile-entrances mobile-place? list-of-fpde?))

; (define mobile-place:mobile-destinations
;   (make-property 'mobile-destinations
;                  'predicate list-of-fpde?
;                  'default-value '()))

; (define get-mobile-destinations
;   (property-getter mobile-place:mobile-destinations mobile-place?))

; (define set-mobile-destinations!
;   (property-setter mobile-place:mobile-destinations mobile-place? list-of-fpde?))

(define mobile-place:possible-fpdes
  (make-property 'possible-fpdes
                 'predicate list-of-fpde?
                 'default-value '()))

(define get-possible-fpdes
  (property-getter mobile-place:possible-fpdes mobile-place?))

(define set-possible-fpdes!
  (property-setter mobile-place:possible-fpdes mobile-place? list-of-fpde?))

; floors/tick
(define mobile-place:speed
  (make-property 'speed
                 'predicate number?
                 'default-value 1))

(define get-speed
  (property-getter mobile-place:speed mobile-place?))

(define set-speed!
  (property-setter mobile-place:speed mobile-place? number?))

(define mobile-place:current-floor
  (make-property 'current-floor
                 'predicate number?
                 'default-value 1))

(define get-current-floor
  (property-getter mobile-place:current-floor mobile-place?))

(define set-current-floor!
  (property-setter mobile-place:current-floor mobile-place? number?))

(define mobile-place:moving-direction
  (make-property 'moving-direction
                 'predicate up-down?
                 'default-value 'up))

(define get-moving-direction
  (property-getter mobile-place:moving-direction mobile-place?))

(define set-moving-direction!
  (property-setter mobile-place:moving-direction mobile-place? up-down?))

(define mobile-place:accessible-floors
  (make-property 'accessible-floors
                 'predicate list-of-number?
                 'default-value '()))

(define get-accessible-floors
  (property-getter mobile-place:accessible-floors mobile-place?))

(define set-accessible-floors!
  (property-setter mobile-place:accessible-floors mobile-place? list-of-number?))

(define mobile-place:floor-pds-lst
  (make-property 'floor-pds-lst
                 'predicate list-of-floor-pds?
                 'default-value '()))

(define get-floor-pds-lst
  (property-getter mobile-place:floor-pds-lst mobile-place?))

(define set-floor-pds-lst!
  (property-setter mobile-place:floor-pds-lst mobile-place? list-of-floor-pds?))

(define mobile-place?
  (make-type 'mobile-place 
    (list 
      ; mobile-place:mobile-entrances 
      ; mobile-place:mobile-destinations
      mobile-place:possible-fpdes
      mobile-place:current-floor
      mobile-place:moving-direction
      mobile-place:speed
      mobile-place:accessible-floors
      mobile-place:floor-pds-lst)))

;; 1. IGNORE: Here for simplicity, we assume elevator is one enclosed space although we can let it be open with changing vistas based on floor-num.
;; Here place:exits is static, so we don't use it.
;; 2. this is different from exits since we can't directly use this to get to another place by `take-exit!`. It will take some time.
(set-predicate<=! mobile-place? place?) ; to make `set-location!` work.

(define make-mobile-place
  (type-instantiator mobile-place?))

;; similar to create-place
(define (create-mobile-place name fpdes)
  (make-mobile-place 'name name
                     'possible-fpdes fpdes))