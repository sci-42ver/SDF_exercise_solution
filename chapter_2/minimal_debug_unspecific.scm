;; https://stackoverflow.com/q/78806930/21294350
(define (displayln x)
      (newline)
      (display x))

(define unit-conversion-list '((('celsius . 'kelvin) . 1) (('tonne . 'kg) . 2)
                                (('tonne . 'g) . 3) (('celsius . 'fahrenheit) . 4)))
(displayln unit-conversion-list)

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

; (define unit-conversion-key-graph 
;   (fold 
;     (lambda (unit-conversion res) 
;       (let ((key-pair (car unit-conversion)))
;         (let* ((from (car key-pair))
;               (to (cdr key-pair))
;               (from-idx 
;                 (list-index 
;                   (lambda (adjacent-node-pair) (equal? from (car adjacent-node-pair))) 
;                   res)))
;           (if (>= from-idx 0)
;             (begin 
;               (displayln from-idx) 
;               (set! res (list-with res from-idx (list from (list (cadr (list-ref res from-idx)) to))))
;               (displayln res)
;               (displayln "one iter ends"))
;             (cons (list from to) res)))))
;     '()
;     unit-conversion-list))

(define unit-conversion-key-graph 
  (let iter ((rest-unit-conversion-list unit-conversion-list)
              (res '()))
    (if (null? rest-unit-conversion-list)
      res
      (let* ((unit-conversion (car rest-unit-conversion-list))
              (key-pair (car unit-conversion)))
        (let* ((from (car key-pair))
              (to (cdr key-pair))
              (from-idx 
                (list-index 
                  (lambda (adjacent-node-pair) (equal? from (car adjacent-node-pair))) 
                  res)))
          (let ((rest-unit-conversion-list (cdr rest-unit-conversion-list)))
            (if (>= from-idx 0)
              (begin 
                (displayln from-idx) 
                (list-set! res from-idx (list from (list (cadr (list-ref res from-idx)) to)))
                (displayln res)
                (displayln "ending")
                (iter rest-unit-conversion-list res))
              (iter rest-unit-conversion-list (cons (list from to) res))))))))
)

(displayln unit-conversion-key-graph)