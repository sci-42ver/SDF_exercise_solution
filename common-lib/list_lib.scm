(define (->list obj)
  (cond 
    ((list? obj) obj)
    ((number? obj) (list obj))
    (else (error (list "->list can't recognize" obj))))
  )

(define-syntax cons*-wrapper
  (syntax-rules ()
    ((_ obj1 ... objn)
      (cons*
        obj1 ...
        (->list objn)
        )
      )
    )
  )

(define (get-the-only-elm lst)
  (assert (= 1 (length lst)))
  (car lst)
  )
