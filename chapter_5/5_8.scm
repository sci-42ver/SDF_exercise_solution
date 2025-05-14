(define (map-stream proc (items lazy memo))
  (if (empty-stream? items)
    items
    (kons (proc (kar items))
          (map-stream proc (kdr items)))))
(define (scale-stream items factor)
  (map-stream (lambda (x) (* x factor))
              items))
(define (integral integrand initial-value dt)
  (define int
    (kons initial-value
          (add-streams (scale-stream integrand dt)
                       int)))
  int)
(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map-stream f y))
  y)
