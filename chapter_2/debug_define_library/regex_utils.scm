(find-scheme-libraries! ".")
,(import (common displayln))
(define-library (regex single-assert-grep=)
  (export single-assert-grep=)
  ; (include 
  ;           "../../software/sdf/manager/load.scm"
  ;           "../../software/sdf/common/testing.scm"
  ;           "../../software/sdf/regular-expressions/test-regexp.scm")

  (begin
    ;; based on ../software/sdf/common/testing.scm and test-regexp.scm.
    (define (single-assert-grep= expected pattern)
      (assert (equal*? expected
                    (r:grep pattern tests-file)))))
  )