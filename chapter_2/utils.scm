;;; discard-arguments
(define (check_all_conds condition lst)
  (assert (fold 
            (lambda (x y) (and x y)) 
            #t 
            (map condition lst))))

(define (displayln x)
  (newline)
  (display x))