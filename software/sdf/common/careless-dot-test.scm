(define (test value)
  (vector-map 
    (lambda (elem) 
      (cond 
        ((number? elem) 
          (newline).  
          (expt 3 4))
        ))
    value))

(test (vector 3))