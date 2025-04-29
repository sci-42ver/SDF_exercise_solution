;; this space-efficient method needs pointer manipulation which is hard to do in Scheme https://stackoverflow.com/a/21917041/21294350
(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "hash_table_lib.scm")
(define (make-bidirectional-map hash-table-constructor)
  (assert (basic-hash-table-constructor? hash-table-constructor))
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
          (list "WARNING" val 
            "will have more than one keys to map. Here we just reset.to key" key)))
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

(define (make-multi-bidirectional-map multi-hash-table-constructor)
  (assert (basic-multi-hash-table-constructor? multi-hash-table-constructor))
  (define keys-val-table (multi-hash-table-constructor))
  ;; This is 1d
  (define val-keys-table (multi-hash-table-constructor))
  
  (define (insert val . keys)
    (multi-hash-set! keys-val-table val keys)
    (let ((found-val (multi-hash-ref* val-keys-table val)))
      (and found-val 
        (write-line 
          (list "WARNING" val "will have more than one keys to map. Here we just reset.")))
      (hash-table-set! val-keys-table val keys)
      )
    )
  (define (getKeys val)
    (hash-table-ref val-keys-table val)
    )
  (define (getKeys* val)
    (hash-table-ref* val-keys-table val)
    )
  (define (get . keys)
    (multi-hash-ref keys-val-table keys)
    )
  ;; For convenient usage although a bit not good to expose private member...
  (define (get-keys-val-table)
    keys-val-table)
  (bundle multi-bidirectional-map? insert getKeys get getKeys* get-keys-val-table)
  )
(define multi-bidirectional-map? (make-bundle-predicate 'multi-bidirectional-map))
