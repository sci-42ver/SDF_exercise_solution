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