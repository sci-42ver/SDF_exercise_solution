(define (run-matcher-wrapper match-procedure datum succeed)
  (newline)
  (write-line '---)
  (pp (run-matcher match-procedure datum succeed))
  (write-line '---)
  )

;; test1
(define test-pat1
  `(?:pletrec ((palindrome
              (?:pnew (x)
                      (?:choice ()
                                ((? x ,symbol?)
                                 (?:ref palindrome)
                                 (? x))))))
           (?:ref palindrome)))
(run-matcher-wrapper
  (match:compile-pattern test-pat1)
  '(a (f (u (t () t) u) f) a)
  print-all-matches)

(define test-pat1*
  `(?:pletrec ((palindrome
              (?:pnew (x)
                      (?:choice ()
                                ((? x ,number?)
                                 (?:ref palindrome)
                                 (? x))))))
           (?:ref palindrome)))
(run-matcher-wrapper
  (match:compile-pattern test-pat1*)
  '(1 () 1)
  print-all-matches)

;; If we use the wrong match:pnew-add-reserved-init-bindings-to-pe, then we will the binding constructed from former run-matcher.
;; So maybe directly #f in match:element...
(run-matcher-wrapper
  (match:compile-pattern test-pat1*)
  '(2 (3 (2 (1 () 1) 2) 3) 2)
  print-all-matches)

(define test-pat1**
  `(?:pletrec ((palindrome
              (?:pnew (x)
                      (?:choice ()
                                ((?? x)
                                 (?:ref palindrome)
                                 (?? x))))))
           (?:ref palindrome)))
(run-matcher-wrapper
  (match:compile-pattern test-pat1**)
  '(a (f (u (t () t) u) f) a)
  print-all-matches)

;; test2
;; y is shared
(define test-pat2
  `(?:pletrec ((palindrome
              (?:pnew (x)
                      (?:choice ()
                                ((? y ,symbol?)
                                 (?:ref palindrome)
                                 (? y))))))
           (?:ref palindrome)))
(run-matcher-wrapper
  (match:compile-pattern test-pat2)
  '(a (f (u (t () t) u) f) a)
  print-all-matches)
; #f
(run-matcher-wrapper
  (match:compile-pattern test-pat2)
  '(a (a () a) a)
  print-all-matches)
; ("level" 3 "constructs pnew-dict" (pnew-dict (x *unassigned* ?/??)) "with new-pe" (pattern-environment (pnew-dict (x *unassigned* ?/??)) (pnew-dict (x *unassigned* ?/??)) (pnew-dict (x *unassigned* ?/??)) (dict (y a ?) (palindrome #[compound-procedure pnew-match] ?:ref))))
; ("level" 2 "constructs pnew-dict" (pnew-dict (x *unassigned* ?/??)) "with new-pe" (pattern-environment (pnew-dict (x *unassigned* ?/??)) (pnew-dict (x *unassigned* ?/??)) (dict (y a ?) (palindrome #[compound-procedure pnew-match] ?:ref))))
; ("level" 1 "constructs pnew-dict" (pnew-dict (x *unassigned* ?/??)) "with new-pe" (pattern-environment (pnew-dict (x *unassigned* ?/??)) (dict (y a ?) (palindrome #[compound-procedure pnew-match] ?:ref))))
; ((dict (y a ?) (palindrome #[compound-procedure pnew-match] ?:ref)))
; #f

;; test3
(define test-pat3
  `(?:pletrec ((palindrome
              (?:pnew (x)
                      (?:choice ()
                                ((? y ,symbol?)
                                 ;; here we will try searching palindrome1 for x.
                                 ;; So we need to differentiate this x from that in palindrome1.
                                 (?:ref palindrome1)
                                 (? y)))))
            (palindrome1
              (?:pnew (x)
                      (?:choice ()
                                ((? x ,symbol?)
                                 (?:ref palindrome1)
                                 (? x)))))
            )
           (?:ref palindrome)))
(run-matcher-wrapper
  (match:compile-pattern test-pat3)
  '(a (f (u (t () t) u) f) a)
  print-all-matches)
;; notice here level-1 has no binding for x as expected but binding for y.
; ("level" 5 "constructs pnew-dict" (pnew-dict (x *unassigned* ?/??)) "with new-pe" (pattern-environment (pnew-dict (x *unassigned* ?/??)) (pnew-dict (x t ?)) (pnew-dict (x u ?)) (pnew-dict (x f ?)) (pnew-dict (x *unassigned* ?/??)) (dict (y a ?) (palindrome #[compound-procedure pnew-match] ?:ref) (palindrome1 #[compound-procedure pnew-match] ?:ref))))
; ("level" 4 "constructs pnew-dict" (pnew-dict (x t ?)) "with new-pe" (pattern-environment (pnew-dict (x t ?)) (pnew-dict (x u ?)) (pnew-dict (x f ?)) (pnew-dict (x *unassigned* ?/??)) (dict (y a ?) (palindrome #[compound-procedure pnew-match] ?:ref) (palindrome1 #[compound-procedure pnew-match] ?:ref))))
; ("level" 3 "constructs pnew-dict" (pnew-dict (x u ?)) "with new-pe" (pattern-environment (pnew-dict (x u ?)) (pnew-dict (x f ?)) (pnew-dict (x *unassigned* ?/??)) (dict (y a ?) (palindrome #[compound-procedure pnew-match] ?:ref) (palindrome1 #[compound-procedure pnew-match] ?:ref))))
; ("level" 2 "constructs pnew-dict" (pnew-dict (x f ?)) "with new-pe" (pattern-environment (pnew-dict (x f ?)) (pnew-dict (x *unassigned* ?/??)) (dict (y a ?) (palindrome #[compound-procedure pnew-match] ?:ref) (palindrome1 #[compound-procedure pnew-match] ?:ref))))
; ("level" 1 "constructs pnew-dict" (pnew-dict (x *unassigned* ?/??)) "with new-pe" (pattern-environment (pnew-dict (x *unassigned* ?/??)) (dict (y a ?) (palindrome #[compound-procedure pnew-match] ?:ref) (palindrome1 #[compound-procedure pnew-match] ?:ref))))
; ((dict (y a ?) (palindrome #[compound-procedure pnew-match] ?:ref) (palindrome1 #[compound-procedure pnew-match] ?:ref)))
; #f

;; test4
;;; IGNORE: Based on lexically scope, here palindrome1 can't access the variable introduced by palindrome
;; Otherwise that is dynamic scope. See https://www.geeksforgeeks.org/static-and-dynamic-scoping/ where g uses x in the caller.
;; Similarly if using dynamic scope, palindrome1 will use x in palindrome.
;;; Here palindrome1 is one pattern instead of one procedure. So it is fine to define that to be able to access x in palindrome.
(define test-pat4
  `(?:pletrec ((palindrome
              (?:pnew (x)
                      (?:choice ()
                                ((? y ,symbol?)
                                 (?:ref palindrome1)
                                 (? y)))))
            ;; here x will refer to the x in palindrome
            (palindrome1
              (?:choice ()
                        ((? x ,symbol?)
                         (?:ref palindrome1)
                         (? x))))
            )
           (?:ref palindrome)))
(run-matcher-wrapper
  (match:compile-pattern test-pat4)
  '(a (f (f () f) f) a)
  print-all-matches)
;; 0. only one level since only one pnew call.
;; 1. Here x is passed along to palindrome1
;; 1.a. TODO (review when SDF chapter 5 p299) this exercise asks for "fresh lexically scoped pattern variables".
;; So (?:ref palindrome1) should be unable to get the value for `x` introduced by palindrome just like one procedure.
;; 1.a.0. But this exercise has been done a bit long time ago, since I won't spend long time to doc for each exercise codes, I won't do this TODO.
; ("level" 1 "constructs pnew-dict" (pnew-dict (x f ?)) "with new-pe" (pattern-environment (pnew-dict (x f ?)) (dict (y a ?) (palindrome #[compound-procedure pnew-match] ?:ref) (palindrome1 #[compound-procedure choices-match] ?:ref))))
; ((dict (y a ?) (palindrome #[compound-procedure pnew-match] ?:ref) (palindrome1 #[compound-procedure choices-match] ?:ref)))
; #f
(run-matcher-wrapper
  (match:compile-pattern test-pat4)
  '(a (f (e () e) f) a)
  print-all-matches)
; #f

;; test5
;; allow multiple parameters
(define test-pat5
  `(?:pletrec ((palindrome
                (?:pnew (x y)
                        (?:choice ()
                                  ((? y ,symbol?)
                                    (? y)
                                    (?:ref palindrome)
                                    (? x ,symbol?)
                                    (? x)
                                    ))))
                )
              (?:ref palindrome)))
(run-matcher-wrapper
  (match:compile-pattern test-pat5)
  '(a a (t t () d d) f f)
  print-all-matches)
; ("level" 3 "constructs pnew-dict" (pnew-dict (x *unassigned* ?/??) (y *unassigned* ?/??)) "with new-pe" (pattern-environment (pnew-dict (x *unassigned* ?/??) (y *unassigned* ?/??)) (pnew-dict (x *unassigned* ?/??) (y t ?)) (pnew-dict (x *unassigned* ?/??) (y a ?)) (dict (palindrome #[compound-procedure pnew-match] ?:ref))))
; ("level" 2 "constructs pnew-dict" (pnew-dict (x d ?) (y t ?)) "with new-pe" (pattern-environment (pnew-dict (x d ?) (y t ?)) (pnew-dict (x *unassigned* ?/??) (y a ?)) (dict (palindrome #[compound-procedure pnew-match] ?:ref))))
; ("level" 1 "constructs pnew-dict" (pnew-dict (x f ?) (y a ?)) "with new-pe" (pattern-environment (pnew-dict (x f ?) (y a ?)) (dict (palindrome #[compound-procedure pnew-match] ?:ref))))
; ((dict (palindrome #[compound-procedure pnew-match] ?:ref)))
; #f
