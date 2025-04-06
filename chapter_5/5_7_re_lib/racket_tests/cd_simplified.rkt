(module name racket
  (begin-for-syntax
    (printf "dir = ~a~n" (current-directory)))
  (current-directory "/tmp")
  )