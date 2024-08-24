(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'generic-procedures)

;; book
(let ((g
(make-generic-arithmetic make-simple-dispatch-store)))
(add-to-generic-arithmetic! g numeric-arithmetic)
(extend-generic-arithmetic! g function-extender)
(add-to-generic-arithmetic! g
(symbolic-extender numeric-arithmetic))
(install-arithmetic! g))

(* 'b ((+ 'c cos sin) (+ 3 'a)))

;; from 6.945_assignment_solution and the book
(let ((g
(make-generic-arithmetic make-simple-dispatch-store)))
(add-to-generic-arithmetic! g numeric-arithmetic)
(extend-generic-arithmetic! g symbolic-extender)
;; See SDF_notes "... prioritized over ..."
(extend-generic-arithmetic! g function-extender)
(install-arithmetic! g))

(* 'b ((+ 'c cos sin) (+ 3 'a)))

(let ((g
(make-generic-arithmetic make-simple-dispatch-store)))
(add-to-generic-arithmetic! g numeric-arithmetic)
(extend-generic-arithmetic! g function-extender) ;*
(extend-generic-arithmetic! g symbolic-extender) ;*
(install-arithmetic! g))

(* 'b ((+ 'c cos sin) (+ 3 'a)))