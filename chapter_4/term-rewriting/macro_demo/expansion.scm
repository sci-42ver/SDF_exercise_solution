; https://stackoverflow.com/a/72475151/21294350
(define-syntax myif
  (syntax-rules ()
    ((_ condition a b)
     (if condition a b))))