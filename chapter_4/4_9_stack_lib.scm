;;; not used
(define (empty-stack)
  (list 'stack))
(define (new-stack data)
  (cons 'stack data))

(define stack-data cdr)
(define (push-stack! data stack)
  (set-cdr! stack (cons data (stack-data stack))))
(define (push-stack data stack)
  (new-stack (cons data (stack-data stack))))

(define stack-top cadr)
