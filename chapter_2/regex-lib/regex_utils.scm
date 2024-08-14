; ;; tests
; ;; 1. https://groups.csail.mit.edu/mac/users/gjs/6.945/sdf/manager/software-manager.pdf
; ;; 2. As https://groups.csail.mit.edu/mac/users/gjs/6.945/psets/ps00/dh.pdf "Note:" says, we should do this first.
; ;; Same for manage
; (load "../software/sdf/manager/load")
; ;; IGNORE: ...  Is there one file to tell me how to solve the dependency?
; ; (load "../software/sdf/common/collections.scm")
; ; (load "../software/sdf/common/generic-procedures.scm")

; ;; https://groups.csail.mit.edu/mac/users/gjs/6.945/psets/ps02/ps.pdf
; ;; I didn't dig into how "manage" works since it is beyond what this book intends to teach.
; (manage 'new 'regular-expressions)

; (load "../software/sdf/common/testing.scm")
; (load "../software/sdf/regular-expressions/test-regexp.scm")
; (load "utils.scm")
; ; (include "utils.scm")

; (displayln tests-file)

; (find-scheme-libraries! ".")
; (define-library (regex utils)
;   ; (include 
;   ;           "../software/sdf/manager/load.scm"
;   ;           "../software/sdf/common/testing.scm"
;   ;           "../software/sdf/regular-expressions/test-regexp.scm")
;   (import (scheme base)
;           (common displayln))
;   )

;; based on ../software/sdf/common/testing.scm and test-regexp.scm.
(define (single-assert-grep= expected pattern)
  (assert (equal*? expected
                   (r:grep pattern tests-file))))
