(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'combining-arithmetics)

(define symbolic-arithmetic-1
  (make-arithmetic-1 'symbolic
                     (lambda (operator)
                       (lambda args (cons operator args)))))

(install-arithmetic! symbolic-arithmetic-1)

(+ 'a 'b)
;; due `(pairwise binary args)` is one loop based on binary, the following outputs "(+ (+ a b) c)".
(+ 'a 'b 'c)
