(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'automatic-differentiation)

(assert
  =
  1
  ((((derivative
      (lambda (x)
        (derivative
          (lambda (y)
            (derivative
              (lambda (z) 
                (* x y z)))))))
      'u)
    'v)
  'w))