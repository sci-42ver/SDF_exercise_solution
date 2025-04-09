(define (substring* str #!optional start end)
  (let* ((start (or* start 0))
         (end (or* end (string-length str)))
         (res (substring str start end)))
    (and
      (not (equal? "" res))
      res)
    )
  )