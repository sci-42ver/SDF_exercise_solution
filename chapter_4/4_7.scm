(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'term-rewriting)

(load "4_7_lib.scm")

; (trace match:ref)
; (trace match:pletrec)
; (trace match:choice)

(define test-pat1
  `(?:pletrec ((odd-even-etc (?:choice () (1 (?:ref even-odd-etc))))
               (even-odd-etc (?:choice () (2 (?:ref odd-even-etc)))))
    ;; Here the mere (?:ref odd-even-etc) matches () etc instead of ((?:ref odd-even-etc)).
    (?:ref odd-even-etc))
  )
; (match:pletrec? test-pat1)
; (match:pletrec-bindings test-pat1)

;; 0. should return dict.
;; 1. '((odd-even-etc #[compound-procedure choices-match] ?:ref) (even-odd-etc #[compound-procedure choices-match] ?:ref))
;; can't be used due to #[...].
(run-matcher
  (match:compile-pattern test-pat1)
  '()
  match:bindings)
; ((odd-even-etc #[compound-procedure choices-match] ?:ref) (even-odd-etc #[compound-procedure choices-match] ?:ref))

;; fail
(run-matcher
  (match:compile-pattern test-pat1)
  '(1 (2 (1 (2 (1)))))
  match:bindings)

;; Here we don't match (1 2 1 ...) so we use cons instead of append for (1 (?:ref even-odd-etc)) etc.
(run-matcher
  (match:compile-pattern test-pat1)
  '(1 (2 (1 (2 (1 ())))))
  match:bindings)
; ((odd-even-etc #[compound-procedure choices-match] ?:ref) (even-odd-etc #[compound-procedure choices-match] ?:ref))

;; (?/?? x) in ?:choice are shown in 4.9.
;; Test that here may be inappropriate since all (? x) etc are the *same* thing...
;; Anyway we can show that
(define test-pat2
  `(?:pletrec ((odd-even-etc (?:choice () ((? x) (?:ref even-odd-etc))))
               (even-odd-etc (?:choice () ((? y) (?:ref odd-even-etc)))))
    ;; Here the mere (?:ref odd-even-etc) matches () etc instead of ((?:ref odd-even-etc)).
    (?:ref odd-even-etc))
  )
(run-matcher
  (match:compile-pattern test-pat2)
  '(1 (2 (1 (2 (1 ())))))
  match:bindings)
; ((y 2 ?) (x 1 ?) (odd-even-etc #[compound-procedure choices-match] ?:ref) (even-odd-etc #[compound-procedure choices-match] ?:ref))

;; use print-all-matches for ??.
(define test-pat3
  `(?:pletrec ((odd-even-etc (?:choice () ((? x) (?:ref even-odd-etc))))
               (even-odd-etc (?:choice () ((?? y) (?:ref odd-even-etc)))))
    ;; Here the mere (?:ref odd-even-etc) matches () etc instead of ((?:ref odd-even-etc)).
    (?:ref odd-even-etc))
  )
(run-matcher
  (match:compile-pattern test-pat3)
  '(1 (2 (1 (2 (1 ())))))
  print-all-matches)
; ((y (2) ??) (x 1 ?) (odd-even-etc #[compound-procedure choices-match] ?:ref) (even-odd-etc #[compound-procedure choices-match] ?:ref))

;; fail
(run-matcher
  (match:compile-pattern test-pat3)
  '(1 (2 (1 (4 (1 ())))))
  print-all-matches)

;; nested
(define test-pat4
  `(?:pletrec ((odd-even-etc* (?:choice () (5 (?:ref even-odd-etc*))))
               (even-odd-etc* (?:choice () (4 (?:ref odd-even-etc*)))))
    (,test-pat3
      (?:ref odd-even-etc*)))
  )
(run-matcher
  (match:compile-pattern test-pat4)
  '((1 (2 (1 (2 (1 ()))))) (5 (4 (5 ()))))
  print-all-matches)
; ((y (2) ??) (x 1 ?)
;   (odd-even-etc #[compound-procedure choices-match] ?:ref)
;   (even-odd-etc #[compound-procedure choices-match] ?:ref)
;   (odd-even-etc* #[compound-procedure choices-match] ?:ref)
;   (even-odd-etc* #[compound-procedure choices-match] ?:ref))

;; Here (?? y) is got from test-pat3, so really messed up due to "flat global namespace".
(define test-pat5
  `(?:pletrec ((odd-even-etc* (?:choice () (5 (?:ref even-odd-etc*))))
               (even-odd-etc* (?:choice () ((?? y) (?:ref odd-even-etc*)))))
    (,test-pat3
      (?:ref odd-even-etc*)))
  )
(run-matcher
  (match:compile-pattern test-pat5)
  '((1 (2 (1 (2 (1 ()))))) (5 (2 (5 ()))))
  print-all-matches)
; ((y (2) ??) (x 1 ?)
;   (odd-even-etc #[compound-procedure choices-match] ?:ref)
;   (even-odd-etc #[compound-procedure choices-match] ?:ref)
;   (odd-even-etc* #[compound-procedure choices-match] ?:ref)
;   (even-odd-etc* #[compound-procedure choices-match] ?:ref))

;; fail
(run-matcher
  (match:compile-pattern test-pat5)
  '((1 (2 (1 (2 (1 ()))))) (5 (4 (5 ()))))
  print-all-matches)

;; normal match for seg etc.
;; similar to test1 in SDF_exercises/chapter_4/4_6.scm
(define test-pat6
  `(?:pletrec ((seg (1 (?:choice 1 (?? x)))))
    (?:ref seg))
  )
(run-matcher
  (match:compile-pattern test-pat6)
  '(1 1)
  print-all-matches)
; ((seg #[compound-procedure list-match] ?:ref))
; ((x (1) ??) (seg #[compound-procedure list-match] ?:ref))
(run-matcher
  (match:compile-pattern test-pat6)
  '(1)
  print-all-matches)
; ((x () ??) (seg #[compound-procedure list-match] ?:ref))
