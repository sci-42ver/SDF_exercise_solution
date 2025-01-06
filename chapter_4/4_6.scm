(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'term-rewriting)

(load "4_6_lib.scm")

;; results just as the book shows.
;; They arer also shown in SICP_SDF/SDF_exercises/software/sdf/design-of-the-matcher/text-examples.scm
(run-matcher
  (match:compile-pattern '(?:choice a b (? x) c))
  'z
  match:bindings)

(run-matcher
  (match:compile-pattern
    `((? y) (?:choice a b (? x ,string?) (? y ,symbol?) c)))
  '(z z)
  match:bindings)

(run-matcher
  (match:compile-pattern `(?:choice b (? x ,symbol?)))
  'b
  print-all-matches)

;;; test1
;; check (list? data) which is compatible with seg.
(run-matcher
  (match:compile-pattern '(z (?:choice a (?? x))))
  '(z)
  match:bindings)
; ((x () ??))
(run-matcher
  (match:compile-pattern '(z (?:choice a (?? x))))
  '(z a)
  print-all-matches)
; ()
; ((x (a) ??))
