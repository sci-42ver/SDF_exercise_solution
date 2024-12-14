(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'unification)

(pp (noisy-infer-program-types
      '(begin (define id (lambda (x) x))
              (id 2)
              (id #t))))

(define test1 
  '(begin
     (define demo
       (lambda (x)
         (begin
           (define inner
             (lambda (y) (x y))
             )
           inner)
         )
       )
     (demo (demo (lambda (x) (* x x))))
     (define num 1)
     (demo num)
     (define codomain-known-proc (lambda (x) #f))
     (define demo2
       (lambda (x)
         (begin
           (define inner
             (lambda (y) (< (x y) 1))
             )
           inner)
         )
       )
     (demo2 codomain-known-proc)
     )
  )
(pp (noisy-infer-program-types test1))



