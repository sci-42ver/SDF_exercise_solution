(define unify:internal-display-wrapper 
  (let ((n 0))
    (lambda (pattern1 pattern2 dict succeed) 
      (write-line (list "4_19_d_tests test" (begin (set! n (n:+ n 1)) n)))
      (unify:internal pattern1 pattern2 dict succeed)
      (write-line "---")
      )
    )
  )
;; Here y-internal will match partial of z.
;; So match:segment-var? can manipulate '??? well.
(unify:internal-display-wrapper '(((?? x) 3) ((?? x) 7) ((?? z)))
                '((4 (?? y)) (4 5 (?? z)) (6 7))
                (match:new-dict)
                (lambda (dict)
                  (pp (match:bindings dict))
                  #f))
; (y-internal:6 (5 6) ???) ... (y (5 6 3) ??)
; (z-internal:8 (6) ???) ... (z (6 7) ??)

;; unify:segment-var-var works for y-internal.
;; the 1st maybe-grab-segment part has been checked in test1.
(unify:internal-display-wrapper '(((?? x) 3) ((?? x) 7))
                '((4 (?? y)) (4 (?? z) 6 7))
                (match:new-dict)
                (lambda (dict)
                  (pp (match:bindings dict))
                  #f))
;; y-internal:10 grabs (?? z).
; ((y-internal:10 ((?? z) 6) ???) (y-internal:9 (3) ???) (y ((?? z) 6 3) ??) (x (4 (?? z) 6) ??))
;; (?? z) tries to grab y-internal:10 but fails.
; ((y-internal:10 (6) ???) (z () ??) (y-internal:9 (3) ???) (y (6 3) ??) (x (4 6) ??))

;; the 2nd maybe-grab-segment part
(unify:internal-display-wrapper '(((?? x) 3) (4 5))
                '((4 (?? y)) ((?? x)))
                (match:new-dict)
                (lambda (dict)
                  (pp (match:bindings dict))
                  #f))
;; results are same as before test1 in SDF_exercises/chapter_4/4_19.scm

;; ensure the original pattern can be matched, e.g. (?? x)->(4 (?? y)).
;; That is done in the 2nd slp in continue.
(unify:internal-display-wrapper '(((?? x) 3))
                '((4 (?? y) 3))
                (match:new-dict)
                (lambda (dict)
                  (pp (match:bindings dict))
                  #f))
;; the 2nd is actually same as the 3rd
; ((y () ??) (x (4) ??))
; ((y-internal:13 () ???) (y ((??? y-internal:14)) ??) (x (4 (??? y-internal:14)) ??))
; ((x (4 (?? y)) ??))

;; ensure the added term binding in dict** can be got appropriately, i.e. that done by check-car-term-binding.
(unify:internal-display-wrapper '(((?? x) 3 (?? z) 3))
                '((4 (?? y) (?? y)))
                (match:new-dict)
                (lambda (dict)
                  (pp (match:bindings dict))
                  #f))
;; Emm... this is much complexer...
;; 0. similar to before, (?? z)<->(?? y) is bidirectional.
; ((z () ??) (y (3) ??) (x (4) ??))
; ((z () ??) (y (3) ??) (x (4) ??))
;; 1. (?? y) tries to grab partial (?? z).
; ((z-internal:16 () ???) (z-internal:15 () ???) (z () ??) (y (3) ??) (x (4) ??))
; ((z-internal:16 (3) ???) (z-internal:15 (3) ???) (z (3 3) ??) (y (3 3) ??) (x (4) ??))
; ...
;; 2. term binding take effects
;; Here when y-internal:21-right tries to grab 3, it first tries ()
;; Then (?? y) expands to (y-internal:22-left y-internal:21-right) which gets substitution by match:single-substitution which append-map for its list value.
; ((y-internal:22-left (3 (?? z) 3) ???) (y-internal:21-right () ???) (y (3 (?? z) 3) ??) (x (4 3 (?? z) 3) ??))
; ...


;; ensure (null? initial) in add-term-to-initial to work.
(unify:internal-display-wrapper '(((?? x)))
                '(((?? y)))
                (match:new-dict)
                (lambda (dict)
                  (pp (match:bindings dict))
                  #f))
;; won't do unnecessary partial intersection match.
; ((y () ??) (x () ??))
; ((x ((?? y)) ??))
; ((x () ??) (y () ??))
; ((y ((?? x)) ??))

;; ensure else in add-term-to-initial to work.
(unify:internal-display-wrapper '(((?? x) 5) ((?? y)))
                '((3 4 (?? y)) (6 5))
                (match:new-dict)
                (lambda (dict)
                  (pp (match:bindings dict))
                  #f))
;; (?? x) grabs 4 expectedly.
; ((y-internal:40-left (6) ???) (y-internal:39-right (5) ???) (y (6 5) ??) (x (3 4 6) ??))
