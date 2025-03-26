; works in siod but not in MIT/GNU Scheme 
(cond 
  (#t
    (define (id x)
      x)
    (id 1)
    )
  )
(cond 
  (#t
    (let ((id (lambda (x) x)))
      (id 1))
    )
  )