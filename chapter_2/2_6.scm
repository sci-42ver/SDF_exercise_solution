(load "../software/sdf/manager/load")
;; not manage testing files like `test-regexp.scm` and `testing.scm`.
(manage 'new 'regular-expressions)
;; This is not indexed in saved-total-index.
(load "../software/sdf/common/testing.scm")
(load "../software/sdf/regular-expressions/test-regexp.scm")
(load "../common-lib/utils.scm")
(load "regex-lib/regex_utils.scm")

; (find-scheme-libraries! ".")
; (define-library (main lib)
;   (import (scheme base)
;           (regex utils)))

;; same as nbardiuk, chebert (mbillingr has no such an implementation) and 6.945_assignment_solution
(define (r:* expr) (r:repeat 0 #f expr))
(define (r:+ expr) (r:repeat 1 #f expr))

;; book test
(define test_str (r:alt (r:quote "cat") (r:quote "dog")))
(r:repeat 3 5 test_str)

;; https://stackoverflow.com/a/33058598/21294350
(displayln (r:* test_str))
(r:+ test_str)

(run-tests tests-file #t)

;; tests
(string->list (r:* test_str))
(assert (= (length (r:grep (r:* test_str) tests-file)) 19))
;; [09]~[15].
(r:grep (r:+ test_str) tests-file)
;; equal*? diff equal?. See chapter 3.
;; equal*? default to be same as equal? (see tagged-data= and object-union=. The latter one is not called by others).
(assert (equal? '("[09]. catdogcat" "[10]. catcatdogdog" "[11]. dogdogcatdogdog" "[12]. catcatcatdogdogdog" "[13]. acatdogdogcats" "[14]. ifacatdogdogs" "[15]. acatdogdogsme")
                (r:grep (r:+ test_str) tests-file)))
