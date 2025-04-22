(define (test)
  (define (FOO)
    (display 1))
  (define (foo)
    (display 2))
  ;; the above has error
  ;; > ;duplicate internal definitions for (#[uninterned-symbol 12 .foo.0]) in test
  ;; So no need

  (bundle test? FOO foo)
  )
(define test? (make-bundle-predicate 'test))
((test) (intern "FOO"))
((test) (intern "foo"))
;; The above 3 all throw errors.
(list (string->uninterned-symbol "FOO") (string->uninterned-symbol "foo"))
;Value: (#[uninterned-symbol 13 |FOO|] #[uninterned-symbol 14 foo])

(define (test2)
  (define (FOO)
    (display 1))
  (bundle test2? FOO)
  )
(define test2? (make-bundle-predicate 'test2))
(map
  (lambda (elm)
    (write-line (symbol->string elm))
    (eq? (intern "FOO") elm) 
    )
  `(,(intern "foo") FOO foo)
  )
(symbol->string (intern "FOO"))
;Value: (#t #t #t)
;; The above is due to all of them have the same *name*.
;; > any two interned symbols whose *names are the same*, in the sense of string=?, 
;; > are the *same object* (i.e. they are eq? to one another).

;; all output 1.
((test2) (intern "FOO"))
((test2) (intern "foo"))
((test2) 'FOO)
((test2) 'foo)
