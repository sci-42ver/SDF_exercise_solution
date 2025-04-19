(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "logic_lib.scm")
(define (check-pred pred elm)
  (and* pred (assert (pred elm)))
  elm
  )