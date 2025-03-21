(define new-pair list)
(define pair?* list)
(define get-left car)
(define get-right cadr)
(define (change-pair-right! pair target) 
  (assert (eq? (car target) (car pair)))
  (set-cdr! pair (cdr target)))
(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "../common-lib/tagged_lst_lib.scm")
(define get-pairs get-data)
(define (add-pair-to-tagged-pairs pair tagged-pairs)
  (let ((pairs (get-pairs tagged-pairs)))
    (let ((val (assq (get-left pair) pairs)))
      (if (not val)
        (set-cdr! tagged-pairs (cons pair pairs))
        (change-pair-right! val pair)))
    tagged-pairs
    ))
