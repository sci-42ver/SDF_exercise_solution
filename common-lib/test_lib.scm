;; same behavior as https://docs.python.org/3/reference/simple_stmts.html#the-assert-statement
(define-syntax assert*
  (syntax-rules ()
    ((_ assertion)
      ; (assert assertion)
      (if (not assertion)
        (error (quote assertion))
        )
      )
    ((_ assertion error-msg)
      (if (not assertion)
        (error error-msg)
        )
      )
    )
  )

(define (assert-predicate pred x y)
  (assert (pred x y)))

(define (trace-wrapper thunk . trace-procs)
  (for-each
    (lambda (proc)
      (trace proc)
      )
    trace-procs
    )
  (thunk)
  (for-each
    (lambda (proc)
      (untrace proc)
      )
    trace-procs
    )
  )
