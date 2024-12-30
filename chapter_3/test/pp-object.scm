(cd "~/SICP_SDF/SDF_exercises/chapter_3")
(load "../software/sdf/manager/load.scm")
(manage 'new 'user-defined-types)

(start-adventure 'foo)
(pp my-avatar)
;; value
; #[avatar #[instance-data #[compound-procedure 13]] foo]
;;; call define-pp-describer value.
;; value is one tagged data
; (screen #[screen #[instance-data #[compound-procedure 14]] the-screen])
;; value is just one quote data.
; (name foo)
; (description foo)
