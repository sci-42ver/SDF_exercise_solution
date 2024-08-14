(define (displayln x)
  (newline)
  (display x))

;; https://stackoverflow.com/a/8387641/21294350
; (define (flatten x)
;   (cond ((null? x) '())
;         ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
;         (else (list x))))

;; This is used to construct big unit-conversion-list, but it will flatten too much making `('celsius . 'kelvin)` disappear.
; (define (duplicate x n)
;   (cond
;     ((= n 0) '())
;     (else
;      (flatten (cons x (duplicate x (- n 1)))))))

(define unit-conversion-list '((('celsius . 'kelvin) . 1) (('tonne . 'kg) . 2)
                                                          (('tonne . 'g) . 3) (('celsius . 'fahrenheit) . 3)))
(displayln unit-conversion-list)
;; https://stackoverflow.com/a/7382392/21294350
(define (list-set! lst k val)
  (if (zero? k)
    (begin
      (displayln "set to")
      (displayln val)
      (set-car! lst val))
    (list-set! (cdr lst) (- k 1) val)))

;; https://stackoverflow.com/a/7871106/21294350
(define (list-with lst idx val)
  (if (null? lst)
    lst
    (cons
      (if (zero? idx)
        val
        (car lst))
      (list-with (cdr lst) (- idx 1) val))))

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

(define unit-conversion-key-graph 
  ; (fold 
  ;   (lambda (unit-conversion res) 
  ;     (let ((key-pair (car unit-conversion)))
  ;       (let* ((from (car key-pair))
  ;             (to (cdr key-pair))
  ;             (from-idx 
  ;               (list-index 
  ;                 (lambda (adjacent-node-pair) (equal? from (car adjacent-node-pair))) 
  ;                 res)))
  ;         (if (>= from-idx 0)
  ;           ;; Here is based on no key duplicity in hash table. https://www.quora.com/Can-Hashtable-have-duplicate-keys-in-Java#:~:text=A%20Hashtable%20does%20not%20accept,does%20not%20accept%20duplicate%20keys.
  ;           (begin 
  ;             (displayln from-idx) 
  ;             ; (list-set! res from-idx (list from (list (list-ref res from-idx) to)))
  ;             (set! res (list-with res from-idx (list from (list (cadr (list-ref res from-idx)) to))))
  ;             (displayln res)
  ;             (displayln "ending"))
  ;           (cons (list from to) res)))))
  ;   '()
  ;   unit-conversion-list)

  ;; Here we can't use define inside `<expression>` since it is same as `(set!​ ​<variable> <expression>)`. https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-7.html#TAG:__tex2page_index_272
  ;; https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-6.html#TAG:__tex2page_index_190
  ;; > express recursion
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
(((quote tonne) ((quote kg) (quote g))) ((quote celsius) ((quote kelvin) (quote fahrenheit))))
