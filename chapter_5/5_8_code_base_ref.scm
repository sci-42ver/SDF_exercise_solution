;; add kons? sentinel.
(define (map-stream proc (items lazy memo))
  (cond ((empty-stream? items) items)
        ((kons? items)
         (kons (proc (kar items))
            (map-stream proc (kdr items))))
        (else 
         (error "Not a stream -- map" items))))

;; add lazy for the possible delayed *stream*.
(define (scale-stream (items lazy memo) factor)
  (map-stream (lambda (x) (* x factor))
              items))

;; Again TODO I don't know why code base doesn't do lazy here but for others.
(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        ((kons? s1)
         (if (kons? s2)
             ;; 2 kons? are to ensure kar etc to work.
             (kons (+ (kar s1) (kar s2))
                   (add-streams (kdr s1) (kdr s2)))
             (error "Not a stream -- add" s1 s2)))
        (else
         (error "Not a stream -- add" s1 s2))))

;; add kons? sentinel
;; and add lazy for the possible delayed *stream*.
(define (ref-stream (stream lazy memo) n)
  (if (kons? stream)
      (if (= n 0)
          (kar stream)
          (ref-stream (kdr stream) (- n 1)))
      (error "Not a stream -- ref" stream)))

(define (integral (integrand lazy memo) initial-value dt)
  (define int
    (kons initial-value
          (add-streams (scale-stream integrand dt)
                       int)))
  int)

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map-stream f y))
  y)