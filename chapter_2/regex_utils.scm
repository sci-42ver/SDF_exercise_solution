;; based on ../software/sdf/common/testing.scm and test-regexp.scm.
(define (single-assert-grep= expected pattern)
  (assert (equal*? expected
                (r:grep pattern tests-file))))