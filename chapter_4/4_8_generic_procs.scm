(define (sequence? data)
  (cond 
    ((or (list? data) (vector? data)) #t)
    ((or (symbol? data) (number? data)) #f)
    (else (error (list "wrong sequence data types" data)))
    ))

;; trivial settings for generic
(define match:general-null?
  (simple-generic-procedure 'match:general-null? 1
    (lambda (data)
      (error (list "wrong arg" data))
      )
    ))
(define-generic-procedure-handler match:general-null?
  (match-args list?)
  null?)
(define-generic-procedure-handler match:general-null?
  (match-args vector?)
  (lambda (data) (= 0 (vector-length data))))

(define match:general-have-data
  (simple-generic-procedure 'match:general-have-data 1
    (lambda (data)
      (error (list "wrong arg" data))
      )
    ))
(define-generic-procedure-handler match:general-have-data
  (match-args list?)
  pair?)
(define-generic-procedure-handler match:general-have-data
  (match-args vector?)
  (lambda (data) (< 0 (vector-length data))))

(define match:general-tail
  (simple-generic-procedure 'match:general-tail 2
    (lambda (data idx)
      (error (list "wrong arg" data))
      )
    ))
(define-generic-procedure-handler match:general-tail
  (match-args list? number?)
  list-tail)
(define-generic-procedure-handler match:general-tail
  (match-args vector? number?)
  vector-tail)

(define match:general-length
  (simple-generic-procedure 'match:general-length 1
    (lambda (data)
      (error (list "wrong arg" data))
      )
    ))
(define-generic-procedure-handler match:general-length
  (match-args list?)
  length)
(define-generic-procedure-handler match:general-length
  (match-args vector?)
  vector-length)

(define match:general-car
  (simple-generic-procedure 'match:general-car 1
    (lambda (data)
      (error (list "wrong arg" data))
      )
    ))
(define-generic-procedure-handler match:general-car
  (match-args list?)
  car)
(define-generic-procedure-handler match:general-car
  (match-args vector?)
  vector-first)

(define match:general-list
  (simple-generic-procedure 'match:general-list 1
    (lambda (ignore)
      ;; 0. same as what match:list returns
      ;; 1. default to fail to match.
      (lambda (data dictionary succeed) #f)
      )
    ))
(define-generic-procedure-handler match:general-list
  (match-args list?)
  match:list)
(define-generic-procedure-handler match:general-list
  (match-args vector?)
  match:vector)

(define match:unary-map
  (simple-generic-procedure 'match:unary-map 2
    (lambda args
      (error "can't map for this seq")
      )
    ))
(define (match:compile-pattern? proc)
  ;; (eq? + +) can work.
  (eq? proc match:compile-pattern)
  )
(define-generic-procedure-handler match:unary-map
  (match-args match:compile-pattern? list?)
  map)
(define-generic-procedure-handler match:unary-map
  (match-args match:compile-pattern? vector?)
  vector-map)
