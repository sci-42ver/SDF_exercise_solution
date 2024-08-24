(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'generic-procedures)
(load "~/SICP_SDF/SDF_exercises/software/sdf/combining-arithmetics/vector-arith.scm")

;; avoid
;; > This only supports fixed-length vectors
;; since we have no (lambda (a b) ...)

(define n:vector vector)

(set! %arithmetic-operator-alist (cons '(vector (domain domain) domain) %arithmetic-operator-alist))
(load "~/SICP_SDF/SDF_exercises/software/sdf/common/numeric-arith.scm")

(load "general-vector-lib.scm")

(install-specific-generic-arithmetic-2)

((vector cos sin sqrt) 3)
;; `(+-like operator identity-name)` makes multiple arguments able to manipulate.
((+ cos sin sqrt) 3)