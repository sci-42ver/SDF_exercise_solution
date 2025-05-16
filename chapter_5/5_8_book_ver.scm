;; > For example, the difficulty alluded to in SICP [1] section 4.2.3 (p. 411) does not automatically dissipate.
;; See https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/full-text/book/book-Z-H-27.html#%_sec_4.2.3
;; > Lazy pairs also help with the problem that arose with streams in section 3.5.4
;; i.e. https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/full-text/book/book-Z-H-24.html#%_sec_3.5.4
;; "(solve " contexts.

(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load-library "../software/sdf/non-strict-arguments/kons.scm")

;; 0. TODO I don't know why book uses "lazy memo" here but not for others...
;; In SICP "define (stream-map" in 3.5.4 aren't lazy and there are no redefinition in 4.2.3.
;; 1. Similar to SICP 3.5.4.
;; Here I just give one interpretation based on modularity without caring much about the detailed implementation.
;; 1.a. integral does
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
  ;; See sicp_notes.md
  ;; > figure 3.34 ...
  ;; for dy meaning.
  ;; So this dy just means dy/dt i.e. y'(t).
  ;; Here y'(t)=y=((lambda (x) x) y)=(f y).
  (define dy (map-stream f y))
  y)
