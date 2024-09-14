;; mimic bag?
;; fpde -> floor-place-direction-exit

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
(define mobile-place:possible-fpdes
  (make-property 'possible-fpdes
                 'predicate list-of-fpde?
                 'default-value '()))

; floors/tick
(define mobile-place:speed
  (make-property 'speed
                 'predicate number?
                 'default-value 1))

(define mobile-place:current-floor
  (make-property 'current-floor
                 'predicate number?
                 'default-value 1))

(define mobile-place:moving-direction
  (make-property 'moving-direction
                 'predicate up-down?
                 'default-value 'up))

(define mobile-place:floor-pds-lst
  (make-property 'floor-pds-lst
                 'predicate list-of-floor-pds?
                 'default-value '()))

(define mobile-place?
  (make-type 'mobile-place 
             (list 
               ; mobile-place:mobile-entrances 
               ; mobile-place:mobile-destinations
               mobile-place:possible-fpdes
               mobile-place:current-floor
               mobile-place:moving-direction
               mobile-place:speed
               ; mobile-place:accessible-floors
               mobile-place:floor-pds-lst)))

(define get-possible-fpdes
  (property-getter mobile-place:possible-fpdes mobile-place?))

(define set-possible-fpdes!
  (property-setter mobile-place:possible-fpdes mobile-place? list-of-fpde?))

(define get-speed
  (property-getter mobile-place:speed mobile-place?))

(define set-speed!
  (property-setter mobile-place:speed mobile-place? number?))

(define get-current-floor
  (property-getter mobile-place:current-floor mobile-place?))

(define set-current-floor!
  (property-setter mobile-place:current-floor mobile-place? number?))

(define get-moving-direction
  (property-getter mobile-place:moving-direction mobile-place?))

(define set-moving-direction!
  (property-setter mobile-place:moving-direction mobile-place? up-down?))

(define get-floor-pds-lst
  (property-getter mobile-place:floor-pds-lst mobile-place?))

(define set-floor-pds-lst!
  (property-setter mobile-place:floor-pds-lst mobile-place? list-of-floor-pds?))

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
