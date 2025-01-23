(define (apply-or lst)
  (fold
    (lambda (elm res) (or elm res))
    #f
    lst
    )
  )