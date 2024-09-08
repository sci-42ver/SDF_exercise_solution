;; see 3_17.scm
(define person:dest-floor-direction
  (make-property 'dest-floor-direction
                 'predicate floor-direction?
                 'default-value (%make-floor-direction 0 'east)))

(define get-dest-floor-direction
  (property-getter person:dest-floor-direction person?))

(define set-dest-floor-direction!
  (property-setter person:dest-floor-direction person? floor-direction?))

(define person?
  (make-type 'person (list person:bag person:dest-floor-direction)))