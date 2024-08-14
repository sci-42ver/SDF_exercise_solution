(load "../software/sdf/manager/load")
(manage 'new 'regular-expressions)
(load "../software/sdf/common/testing.scm")
(load "../software/sdf/regular-expressions/test-regexp.scm")
(load "utils.scm")
(load "regex-lib/regex_utils.scm")

(load "2_10.scm") ; used as the lib

(define regex_type 'ere)
(define (test_str)
  (r:seq (r:quote "a")
         (r:dot)
         (r:quote "c")))
(define test_expected
  '("[00]. abc"
    "[01]. aac"
    "[02]. acc"
    "[03]. zzzaxcqqq"
    "[10]. catcatdogdog"
    "[12]. catcatcatdogdogdog"))
(test_str)
;; See `echo "(a.c)" | grep -e '(a.c)' -` diff from `echo "(a.c)" | grep -E '(a.c)' -`.
(single-assert-grep=
  test_expected
  (test_str))
(r:alt "a" "b")

(define regex_type 'bre)
(test_str)
;; both work "with equivalent results".
(single-assert-grep=
  test_expected
  (test_str))
(r:alt "a" "b")
