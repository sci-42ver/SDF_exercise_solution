; (define table (cons-array 10 'string))
(define table (cons-array 10))

(href table 'test)
(href #(()) 'test)

(if (href #(()) 'test)
  (writes nil "#t")
  (writes nil "#f")
  )
