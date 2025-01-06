;; test1

(define test-pat1
  `(?:pletrec ((palindrome
              (?:pnew (x)
                      (?:choice ()
                                ((? x ,symbol?)
                                 (?:ref palindrome)
                                 (? x))))))
           (?:ref palindrome)))

(define test-pat1*
  `(?:pletrec ((palindrome
              (?:pnew (x)
                      (?:choice ()
                                ((? x ,number?)
                                 (?:ref palindrome)
                                 (? x))))))
           (?:ref palindrome)))
(define (run-matcher-wrapper match-procedure datum succeed)
  (newline)
  (write-line '---)
  (run-matcher match-procedure datum succeed)
  (write-line '---)
  )
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
