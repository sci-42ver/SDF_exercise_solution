(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'automatic-differentiation)

(define (extract-dx-function fn dx)
  (lambda args
    (extract-dx-part (apply fn args) dx)))

(define-generic-procedure-handler extract-dx-part
  (match-args function? diff-factor?)
  extract-dx-function)

(((derivative
   (lambda (x)
     (derivative
      (lambda (y)
        (* x y)))))
  'u)
 'v)