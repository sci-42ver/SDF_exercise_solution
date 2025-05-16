; (define (scale-stream (items lazy memo) factor)
;   (map-stream (lambda (x) (* x factor))
;               items))
;; 0. kons will implicitly delay scale-stream, same for integrand used by that.
;; 1. Compared with 5_8_code_base_ref.scm, this is enough to just delay .
;; 
(define (integral (integrand lazy memo) initial-value dt)
  (define int
    (kons initial-value
          (add-streams (scale-stream integrand dt)
                       int)))
  int)