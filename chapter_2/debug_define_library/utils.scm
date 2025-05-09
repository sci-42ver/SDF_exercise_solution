;;; discard-arguments

; (define (displayln x)
;       (newline)
;       (display x))
; (define (check_all_conds condition lst)
;   (assert (fold 
;             (lambda (x y) (and x y)) 
;             #t 
;             (map condition lst))))

(define-library (common displayln)
  (export displayln check_all_conds)
  (begin
    (define (displayln x)
      (newline)
      (display x))
    (define (check_all_conds condition lst)
      (assert (fold 
                (lambda (x y) (and x y)) 
                #t 
                (map condition lst))))))