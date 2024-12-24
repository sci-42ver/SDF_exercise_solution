(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'unification)

;; trace the former 2 ((w () ??) (y () ??) (x (b b b) ??))'s.
; (trace grab-segment)
; (trace maybe-grab-segment)
;; for tracing the 3rd ((w () ??) (y () ??) (x (b b b) ??))
(trace do-substitute)
(let ((p1 '(a (?? x) (?? y) (?? x) c))
(p2 '(a b b b (?? w) b b b c)))
(unify:internal p1 p2 (match:new-dict)
(lambda (dict)
(pp (match:bindings dict))
#f)))
;; Here 3 ((w () ??) (y () ??) (x (b b b) ??))'s occur.
;; ((?? y) (?? x) c)->((?? w) b b b c)
  ;; The 1st is by ((?? x) c)->((?? w) b b b c)
  ;; then ((?? w) b b b c)->(b b b c)
  ;; The 2nd is by ((?? w) b b b c)->((?? x) c) due to unify:segment-var-var
  ;; then ((?? x) c)->(b b b c) which directly matches.
  ;; Then (?? y)->(?? w)
  ;; Then we can't proceed
;; ((?? w) b b b c)->((?? y) (?? x) c)
  ;; So (y () ??) (w () ??) which is the reverse order of the before.
;; The 3rd is due to (fail) called by "("call grab-segment fail for" ((?? w) b b b c) ((?? y) (?? x) c) (dict (x (b b b) ??)))".
;; Then (?? x) is matched to new (b b b (?? w)).
;; Then "("in slp" ((?? y) (?? x) c) "with terms after grab" (b b b c) (dict (x (b b b (?? w)) ??)))".
;; Then (?? y)->() and then try (append (b b b (?? w)) (c))->(b b b c)
;; Then (?? w)->() which then makes (?? x)->(b b b) in do-substitute.
; ((y (b b b (?? w) b b b) ??) (x () ??))
; ((y (b b (?? w) b b) ??) (x (b) ??))
; ((y (b (?? w) b) ??) (x (b b) ??))
; ((w () ??) (y () ??) (x (b b b) ??))
; ((w () ??) (y () ??) (x (b b b) ??))
; ((y ((?? w)) ??) (x (b b b) ??))
; ((y () ??) (w () ??) (x (b b b) ??))
; ((w ((?? y)) ??) (x (b b b) ??))
; ((w () ??) (y () ??) (x (b b b) ??))
