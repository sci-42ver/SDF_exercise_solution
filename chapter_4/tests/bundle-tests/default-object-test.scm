(define (test x)
  (define (inner y #!optional z)
    (+ x y (if (default-object? z) (begin (write-line "default-object? z") 0) z)))
  (bundle test-obj? inner)
  )
(define test-obj? (make-bundle-predicate 'test-obj))

(define test-obj1 (test 3))
(test-obj1 'inner 2)
(test-obj1 'inner 2 (default-object))
(test-obj1 'inner 2 4)

(define (test2 x)
  (define (inner y z)
    (+ x y (if (default-object? z) (begin (write-line "default-object? z") 0) z)))
  (bundle test-obj? inner)
  )
(define test-obj2 (test2 3))
; (test-obj2 'inner 2)
;The procedure #[compound-procedure 12 inner] has been called with 1 argument; it requires exactly 2 arguments.
(test-obj2 'inner 2 (default-object))
(test-obj2 'inner 2 4)
