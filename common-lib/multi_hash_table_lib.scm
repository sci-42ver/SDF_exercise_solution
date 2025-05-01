(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "hash_table_lib.scm")

(define basic-multi-hash-table-constructor? basic-hash-table-constructor?)
;; assume no final val is hash-table.
(define (get-val-lst-for-multi-hash-table multi-hash-table)
  (assert (multi-hash-table? multi-hash-table))
  (let ((possible-table-list (get-val-lst multi-hash-table)))
    (if (list-of-type? possible-table-list multi-hash-table?)
      (append-map
        get-val-lst-for-multi-hash-table
        possible-table-list
        )
      possible-table-list)
    )
  )

(define multi-hash-table? hash-table?)
;;; from https://stackoverflow.com/a/14350525/21294350
(define hash-table-exists? hash-table-ref*)
(define foldl fold)
;; Here hash-table-ref is expected to error if failure to lookup.
; create a new multidimensional hash table
(define (%make-multi-hash base-constructor)
  (assert (basic-hash-table-constructor? base-constructor))
  (base-constructor))
(define (make-multi-hash)
  (%make-multi-hash default-hash-table-constructor))

; set a value given a non-empty sequence of keys
(define (multi-hash-set! hash-table value . keys)
  (assert (not (null? keys)))
  (let loop ((hash hash-table)
             (keys keys))
    ; (write-line (list "multi-hash-set! has keys" keys))
    (cond ((null? (cdr keys))
           (hash-table-set! hash (car keys) value))
          (else
           (if (not (hash-table-exists? hash (car keys)))
             (hash-table-set! hash (car keys) (make-multi-hash)))
           (loop (hash-table-ref hash (car keys)) (cdr keys))))))

; retrieve a value given a non-empty sequence of keys
(define (multi-hash-ref hash-table . keys)
  ; (write-line (list "multi-hash-ref" hash-table keys))
  (foldl (lambda (k h) (hash-table-ref h k))
         hash-table
         keys))

;; will automatically throw errors if trying to ref about one key for one non-hash-table.
(define (multi-hash-ref* hash-table . keys)
  (foldl (lambda (k h) (hash-table-ref* h k))
         hash-table
         keys))
