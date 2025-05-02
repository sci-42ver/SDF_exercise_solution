(define (->list obj)
  (cond 
    ((list? obj) obj)
    ((or (symbol? obj) (number? obj)) (list obj))
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

;; not same as map*
(define-syntax map-ordered
  (syntax-rules ()
    ((_ proc list1 list2 ...)
      (reverse
        (fold (lambda (e1 e2 ... acc)
                (cons (proc e1 e2 ...) acc))
              '()
              list1
              list2
              ...))
      )
    )
  )

; (define (filter-map-ordered proc . lists)
;   (filter
;     (lambda (obj) obj)
;     ;; ;Transformer may not be used as an expression: #[transformer-item 12]
;     (apply map-ordered (cons proc lists))
;     ))
(define-syntax filter-map-ordered
  (syntax-rules ()
    ((_ proc list1 list2 ...)
      (filter
        (lambda (obj) obj)
        (map-ordered proc list1 list2 ...)
        )
      )
    )
  )
