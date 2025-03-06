(define partition-separtor-lst '((or "(" ")") (or "**" ":=" "--" "++" "==") (or "*" ":" "-" "+" "/" "=")))
;; no further partitions done for them.
(define primitive-symbol-re-lst '((or "**" ":=" "--" "++" "==")))
(define split-lst '((+ space)))

(define (make-or sre-lst)
  (assert (and (list? sre-lst) (every valid-sre? sre-lst)))
  (cons 'or sre-lst)
  )

(define (exp-split exp split-lst)
  (let lp ((res-lst (list exp)) (rest-split-lst split-lst))
    (if (null? rest-split-lst)
      res-lst
      (let ((res-lst*
              (append-map 
                (lambda (res)
                  (assert (string? res))
                  (regexp-split (car rest-split-lst) res)) 
                res-lst)
              ))
        (lp res-lst* (cdr rest-split-lst))))
    )
  )

(define (exp-partition exp separtor-lst skipped-re-lst)
  (let lp ((res-lst (list exp)) (rest-separtor-lst separtor-lst))
    (if (null? rest-separtor-lst)
      res-lst
      (let ((res-lst-cluster
              (map 
                (lambda (res)
                  (assert (string? res))
                  (if (any
                        ;; see https://srfi.schemers.org/srfi-115/srfi-115.html#proc-_2b
                        ;; > matches the entire string
                        (lambda (re) (regexp-matches? re res))    
                        skipped-re-lst)
                    (list res)
                    (regexp-partition (car rest-separtor-lst) res))) 
                res-lst)
              ))
        (lp (apply append res-lst-cluster) (cdr rest-separtor-lst))))
    )
  )

(regexp-partition '(or "*" "**") "b**2-4*a*c")

;; misc string lib
(define (empty-str? str)
  (assert (string? str))
  (n:= 0 (string-length str))
  )

(define test-exp3
  "fact := lambda n:
  if n == 0
  then 1
  else n*fact(n-1)"
  )

(exp-split test-exp3 split-lst)

