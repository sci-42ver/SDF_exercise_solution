(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'term-rewriting)

;; This has been checked before that (or (succeed (match:extend-dict ...)) (lp (+ i 1))) does this Backtracking magic.
;; Anyway this is same as SICP amb.

; (run-matcher
; (match:compile-pattern '(a (?? x) (?? y) (?? x) c))
; '(a b b b b b b c)
; print-all-matches)

;; simpler with the same structure
(trace print-all-matches)
(run-matcher
  (match:compile-pattern '(a (?? x) (?? y) (?? x) c))
  '(a b b c)
  print-all-matches)

;; Use (trace lp), (trace succeed) and (trace segment-match) in match:segment
;; Notice (trace lp) can track lp in *another* segment-match instance ...

;; Here only gives one brief fragment of the whole process
;; Anyway by induction, the above or will work to try *all alternatives* when succeed always returns false.
; [#f
;       <== #[compound-procedure 14]
;     Args: (dict (y () ??) (x () ??))
;           0]
; [Entering #[compound-procedure lp]
;     Args: 1]
; ...
; [Entering #[compound-procedure lp]
;     Args: 2]
; [Entering #[compound-procedure print-all-matches]
;     Args: (dict (y (b b) ??) (x () ??))]
; ((y (b b) ??) (x () ??))
; ...


