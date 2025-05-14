(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "../software/sdf/manager/load.scm")
(manage 'new 'non-strict-arguments)

; (trace general-compound-procedure?)
; (trace make-compound-procedure)
; (trace define-variable!)
; (trace definition-value)

(init)

;; to allow parsing "(y lazy memo)" etc.
(cd "~/SICP_SDF/SDF_exercises/chapter_5")
;; this will use the underlying interpreter which is unexpected.
; (load "../software/sdf/non-strict-arguments/kons.scm")
(load-library "../software/sdf/non-strict-arguments/kons.scm")

(inexact
  (/ (ref-stream fibs 100)
     (ref-stream fibs 99)))

(/ (ref-stream fibs 100)
   (ref-stream fibs 99))
