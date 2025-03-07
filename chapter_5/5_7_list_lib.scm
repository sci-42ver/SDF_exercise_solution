;;; these 2 procedures aren't used as the base procedures elsewhere.

;; https://stackoverflow.com/a/42559626/21294350
(define positions
  (lambda (pred L)
    (let loop ((i 0)
               (r '())
               (L L))
      (if (null? L)
          (reverse r)
          (loop (+ i 1)
                (if (pred (car L)) ; modified
                    (cons i r)
                    r)
                (cdr L))))))

;; 0. https://stackoverflow.com/a/60325128/21294350
;; Search for matching parenthesis
;; DMIA and mcs gives one algorithm and doesn't offer one code example.
;; 1. For combine-parentheses, here idx is kept to allow back-ref and then check whether it is one application.
(define (match-parentheses? str-lst)
  (let ((str-cnt (length str-lst)))
    (let lp ((paren-cnt 0) (idx 0) (paren-to-match (list)))
      (if (n:<= str-cnt idx)
        (if (n:> paren-cnt 0)
          (error "redundant parentheses")
          'matched)
        (let ((cur (list-ref str-lst idx)))
          (let ((next-idx (n:+ idx 1)))
            (cond 
              ((left-parenthesis? cur)
                (lp (n:+ paren-cnt 1) next-idx (append paren-to-match (list left-parenthesis))))
              ((right-parenthesis? cur)
                (let ((paren-cnt* (n:- paren-cnt 1)))
                  (if 
                    (or 
                      (n:< paren-cnt* 0)
                      (not (equal? left-parenthesis (list-ref paren-to-match paren-cnt*))))
                    (begin
                      ; (write-line (list "match-parentheses?" paren-cnt* (list-ref paren-to-match paren-cnt*)))
                      (error "matched with the wrong right str"))
                    )
                  (lp paren-cnt* next-idx (drop-right paren-to-match 1))
                  )
                )
              (else
                (lp paren-cnt next-idx paren-to-match)
                )
              ))
          ))
      ))
  )
