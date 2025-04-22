(define (hash-table-ref* hash-table key)
  (hash-table-ref/default hash-table key #f))