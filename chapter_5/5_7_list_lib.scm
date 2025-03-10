;; https://stackoverflow.com/a/42559626/21294350
;; not used
(define positions
  (lambda (pred L)
    (let loop ((i 0)
               (r '())
               (L L))
      (if (null? L)
          (reverse r)
          (loop (+ i 1)
                (if (pred (car L)) ; modified
                    (cons i r)
                    r)
                (cdr L))))))