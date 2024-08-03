;; > Edit our program to eliminate as much of the unnecessary nesting as you can.
;; just change r:seq as nbardiuk does, i.e. point 1 and 2 in 6.945_assignment_solution
;; Here "(0) (r:seq)", r:alt implies we better keeping inserting () in (r:seq) to capture the empty string.
;; > There are subtle cases here that you have to watch out for.
;; > You might consider using a different intermediate representation.
;; TODO I don't what it wants to say.
;; 1. I don't know.
;; See 6.945_assignment_solution
;; 2. nbardiuk, chebert and 6.945_assignment_solution just uses string. mbillingr doesn't have such a implementation.

(load "../software/sdf/manager/load")
(manage 'new 'regular-expressions)
(load "../software/sdf/common/testing.scm")
(load "../software/sdf/regular-expressions/test-regexp.scm")
(load "utils.scm")
(load "regex-lib/regex_utils.scm")

;; The following is wrong. See 6.945_assignment_solution
(define (r:seq . exprs)
  (apply string-append exprs))

;; test
(assert (equal? (r:seq (r:quote "a")
                        (r:dot)
                        (r:quote "c"))
                "a.c"))