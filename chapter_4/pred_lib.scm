(define (apply-logical-op lst op base)
  (fold
    (lambda (elm res) (op elm res))
    base
    lst
    )
  )
(define (apply-or lst)
  (apply-logical-op lst (lambda (exp1 exp2) (or exp1 exp2)) #f)
  )
(define (apply-and lst)
  (apply-logical-op lst (lambda (exp1 exp2) (and exp1 exp2)) #t)
  )
