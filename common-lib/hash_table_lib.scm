(define (hash-table-ref* hash-table key)
  (hash-table-ref/default hash-table key #f))

(define (basic-hash-table-constructor? constructor)
  (and 
    (procedure? constructor) 
    (equal? '(0 . 1) (procedure-arity constructor)))
  )

(define (get-val-lst hash-table)
  (assert (hash-table? hash-table))
  (map cdr (hash-table->alist hash-table))
  )

;; https://stackoverflow.com/a/61180123/21294350
;; TODO after CRLS: use the most efficient based on the needs here.
(define default-hash-table-constructor make-equal-hash-table)
