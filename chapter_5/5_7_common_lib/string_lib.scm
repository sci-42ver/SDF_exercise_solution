(define (string-null? str)
  (assert (string? str))
  (n:= 0 (string-length str))
  )
(define (optional-arg-lst arg)
  (if (default-object? arg)
    '()
    (list arg))
  )
(define (substring-lst str #!optional start end)
  (let ((res 
          (apply 
            substring 
            (append (list str) (optional-arg-lst start) (optional-arg-lst end)))))
    (if (string-null? res)
      '()
      (list res))
    )
  )