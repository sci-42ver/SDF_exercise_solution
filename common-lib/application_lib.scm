(define (apply-with-no-default-object-arg proc . args)
  (apply proc (remove default-object? args))
  )