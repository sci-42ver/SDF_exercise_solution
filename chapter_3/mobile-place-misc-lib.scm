;; misc helper
; (define (get-cur-floor mobile-place)
;   (fpde-floor-num (car (get-mobile-entrances mobile-place))))

; (define (all-possible-dests mobile-place)
;   (append (get-mobile-entrances mobile-place) (get-mobile-destinations mobile-place)))

; (define (get-fpde-by-entrance-exit-place-name place-name mobile-place find-entrance?)
;   (find 
;     (lambda (fpde) 
;       (equal? place-name (get-name (fpde-place fpde))))
;     ((if find-entrance?
;       get-mobile-entrances
;       get-mobile-destinations) mobile-place)))

(define (all-floors mobile-place)
  (map fpde-floor-num (get-possible-fpdes mobile-place)))

(define (random-floor mobile-place)
  (random-choice (delv (get-current-floor mobile-place) (all-floors mobile-place))))

(define (floor-fpdes floor-num mobile-place)
  (filter
    (lambda (fpde) (= floor-num (fpde-floor-num fpde)))
    (get-possible-fpdes mobile-place)))

(define (random-floor-outward-direction floor-num mobile-place)
  (fpde-outward-direction
    (random-choice 
      (floor-fpdes floor-num mobile-place))))

(define (max-floor mobile-place)
  (apply max (all-floors mobile-place)))

(define (min-floor mobile-place)
  (apply min (all-floors mobile-place)))

(define (in-range elem range)
  (n:<= (car range) elem (cdr range)))

(define (find-fpde-for-mobile-place dest-name mobile-place)
  (find
    (lambda (fpde) (equal? (get-name (fpde-place fpde)) dest-name))
    (get-possible-fpdes mobile-place)))