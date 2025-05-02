(define (apply-with-no-default-object-arg proc . args)
  (apply proc (remove default-object? args))
  )

(define-syntax apply-with-ending-list
  (syntax-rules ()
    ((_ proc arg ... rest)
      (apply proc (cons* arg ... rest))
      )
    )
  )