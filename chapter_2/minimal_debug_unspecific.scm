;; https://stackoverflow.com/q/78806930/21294350
(define (displayln x)
      (newline)
      (display x))

(define unit-conversion-list '(((celsius . kelvin) . 1) ((tonne . kg) . 2)
                                ((tonne . g) . 3) ((celsius . fahrenheit) . 4)))
(define unit-conversion-pairs (map car unit-conversion-list))
(displayln unit-conversion-pairs)
; Outputs:
; ((celsius . kelvin) (tonne . kg) (tonne . g) (celsius . fahrenheit))

;; https://stackoverflow.com/a/7382392/21294350
(define (list-set! lst k val)
    (if (zero? k)
        (begin
          (displayln "set to")
          (displayln val)
          (set-car! lst val))
        (list-set! (cdr lst) (- k 1) val)))

;; https://cookbook.scheme.org/find-index-of-element-in-list/
(define (list-index fn lst)
  (displayln lst)
  (let iter ((lst lst) (index 0))
    (if (null? lst)
        -1
        (let ((item (car lst)))
          (if (fn item)
              index
              (iter (cdr lst) (+ index 1)))))))

(define (adjacency-pairs-to-adjacency-list adjacency-pairs)
  (fold 
    (lambda (adjacency-pair res) 
      (let* ((from (car adjacency-pair))
            (to (cdr adjacency-pair))
            (from-idx 
              (list-index 
                (lambda (adjacent-list-elem) (equal? from (car adjacent-list-elem))) 
                res)))
        (if (>= from-idx 0)
          (begin 
            (displayln from-idx) 
            (list-set! res from-idx (list from (list (cadr (list-ref res from-idx)) to)))
            (displayln res)
            (displayln "ending"))
          (cons (list from to) res))))
    '()
    adjacency-pairs))

(define (adjacency-pairs-to-adjacency-list adjacency-pairs)
  (let iter ((rest-adjacency-pairs adjacency-pairs)
              (res '()))
    (if (null? rest-adjacency-pairs)
      res
      (let* ((adjacency-pair (car rest-adjacency-pairs)))
        (let* ((from (car adjacency-pair))
              (to (cdr adjacency-pair))
              (from-idx 
                (list-index 
                  (lambda (adjacent-list-elem) (equal? from (car adjacent-list-elem))) 
                  res)))
          (let ((rest-adjacency-pairs (cdr rest-adjacency-pairs)))
            (if (>= from-idx 0)
              (begin 
                (displayln from-idx) 
                (list-set! res from-idx (list from (list (cadr (list-ref res from-idx)) to)))
                (displayln res)
                (displayln "ending")
                (iter rest-adjacency-pairs res))
              (iter rest-adjacency-pairs (cons (list from to) res))))))))
)

(define unit-conversion-key-graph (adjacency-pairs-to-adjacency-list unit-conversion-pairs))
(displayln unit-conversion-key-graph)