(define (make-address-shift x y)
  (guarantee shift? x 'make-address-shift)
  (guarantee shift? y 'make-address-shift)
  (list x y))
(define (shift? object)
  (and (integer? object)
       (exact? object)
       ))
(register-predicate! shift? 'shift)
(define shift-x address-x)
(define shift-y address-y)

(define (add-address-by-shifts address x-shift y-shift)
  (make-address-shift (n:+ (address-x address) x-shift) (n:+ (address-y address) y-shift)))
(define (shift-address* address shift op)
  (make-address-shift 
    (op (address-x address) (shift-x shift)) 
    (op (address-y address) (shift-y shift))))
(define (address-sum address1 address2)
  (shift-address* address1 address2 n:+)
  )

;; https://en.wikipedia.org/wiki/Subtraction#Notation_and_terminology
(define (address-diff minuend subtrahend)
  (shift-address* minuend subtrahend n:-))

(define (list-op op . lsts)
  (assert (apply n:= (map length lsts)))
  (map
    (lambda (idx)
      (apply op (map (lambda (lst) (list-ref lst idx)) lsts))
      )
    (iota (length (car lsts)))
    )
  )
(define (address-division dividend divisor)
  ;; no need to be still one address
  (list-op
    (lambda (a b)
      (if (n:= b 0)
        (if (n:= a 0)
          'ignored
          +inf.0
          )
        (n:/ a b))
      )
    dividend divisor
    ))
(define (address-divisible dividend divisor)
  (let* ((division (address-division dividend divisor))
        (filtered (remove (lambda (elm) (eq? 'ignored elm)) division)))
    (and
      (every exact? filtered)
      (apply n:= filtered)
      ))
  )
