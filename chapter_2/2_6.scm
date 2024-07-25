;; tests
;; 1. https://groups.csail.mit.edu/mac/users/gjs/6.945/sdf/manager/software-manager.pdf
;; 2. As https://groups.csail.mit.edu/mac/users/gjs/6.945/psets/ps00/dh.pdf "Note:" says, we should do this first.
;; Same for manage
(load "../software/sdf/manager/load")
;; IGNORE: ...  Is there one file to tell me how to solve the dependency?
; (load "../software/sdf/common/collections.scm")
; (load "../software/sdf/common/generic-procedures.scm")

;; https://groups.csail.mit.edu/mac/users/gjs/6.945/psets/ps02/ps.pdf
;; I didn't dig into how "manage" works since it is beyond what this book intends to teach.
(manage 'new 'regular-expressions)

;; same as nbardiuk, chebert (mbillingr has no such an implementation).
(define (r:* expr) (r:repeat 0 #f expr))
(define (r:+ expr) (r:repeat 1 #f expr))

;; book test
(define test_str (r:alt (r:quote "cat") (r:quote "dog")))
(r:repeat 3 5 test_str)

(load "utils.scm")
;; https://stackoverflow.com/a/33058598/21294350
(displayln (r:* test_str))
(r:+ test_str)

(load "../software/sdf/common/testing.scm")
(load "../software/sdf/regular-expressions/test-regexp.scm")
(displayln tests-file)
(run-tests tests-file #t)

;; tests
(load "regex_utils.scm")
(string->list (r:* test_str))
(assert (= (length (r:grep (r:* test_str) tests-file)) 19))
;; [09]~[15].
(r:grep (r:+ test_str) tests-file)