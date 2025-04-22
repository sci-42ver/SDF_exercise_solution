;; this space-efficient method needs pointer manipulation which is hard to do in Scheme https://stackoverflow.com/a/21917041/21294350
(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "hash_table_lib.scm")
(define (make-bidirectional-map hash-table-constructor)
  (assert (and (procedure? hash-table-constructor) (equal? '(0 . 0) (procedure-arity hash-table-constructor))))
  (define key-val-table (hash-table-constructor))
  (define val-key-table (hash-table-constructor))
  (define (insert key val)
    (hash-table-set! key-val-table key val)
    (let ((found-val (hash-table-ref* val-key-table val)))
      ;; keeping a list will make getKey much harder and less efficient...
      ; (if found-val
      ;   (hash-table-set! val-key-table val (cons key found-val))
      ;   (hash-table-set! val-key-table val (list key))
      ;   )
      (and found-val 
        (write-line 
          (list "WARNING" val "will have more than one keys to map. Here we just reset.")))
      (hash-table-set! val-key-table val key)
      )
    )
  ;; API borrowed from https://stackoverflow.com/a/74827910/21294350
  (define (getKey val)
    (hash-table-ref* val-key-table val)
    )
  (define (get key)
    (hash-table-ref* key-val-table key)
    )
  (bundle bidirectional-map? insert getKey get)
  )
(define bidirectional-map? (make-bundle-predicate 'bidirectional-map))
