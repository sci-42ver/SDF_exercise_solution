(define (apply-logical-op lst op)
  (fold
    (lambda (elm res) (op elm res))
    #f
    lst
    )
  )
(define (apply-or lst)
  (apply-logical-op lst (lambda (exp1 exp2) (or exp1 exp2)))
  )
(define (apply-and lst)
  (apply-logical-op lst (lambda (exp1 exp2) (and exp1 exp2)))
  )
