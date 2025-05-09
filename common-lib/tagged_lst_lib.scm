(define (empty-tagged-lst tag) (list tag))
(define (new-tagged-lst tag data) (cons tag data))
(define (new-tagged-lst* tag . data) (cons tag data))
(define (tagged-list-pred tag)
  (lambda (lst)
    (and (list? lst)
      (>= (length lst) 1)
      ;; assume interned symbols
      (eq? (car lst) tag)
      )
    )
  )

(define get-tagged-lst-tag car)
(define get-tagged-lst-data cdr)
(define (get-first-datum tagged-lst)
  (if (empty-tagged-lst? tagged-lst)
    (error "can't get-first-datum for one empty tagged-lst")
    (first (get-tagged-lst-data tagged-lst)))
  )

(define (find-var var tagged-lst) 
  (let ((data (get-tagged-lst-data tagged-lst)))
    (assert (every list? data))
    ;; allow var to be string.
    (assoc var data)
    )
  )

(define (insert-elem-to-data-end elem tagged-lst)
  (let ((data (get-tagged-lst-data tagged-lst)))
    (new-tagged-lst (get-tagged-lst-tag tagged-lst) (append data (list elem))))
  )

(define (insert-elem-to-data-beginning elem tagged-lst)
  (let ((data (get-tagged-lst-data tagged-lst)))
    (new-tagged-lst (get-tagged-lst-tag tagged-lst) (cons elem data)))
  )
(define (insert-elem-to-data-beginning! elem tagged-lst)
  (let ((data (get-tagged-lst-data tagged-lst)))
    (set-cdr! tagged-lst (cons elem data)))
  )

(define (empty-tagged-lst? tagged-lst)
  (null? (get-tagged-lst-data tagged-lst)))
