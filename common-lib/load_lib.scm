(define load*
  (let ((loaded-table '()))
    (lambda (filename)
      (cond 
        ((assoc filename loaded-table)
          'have-loaded
          )
        (else
          (load filename)
          (set! loaded-table 
            (cons (list filename 'loaded) loaded-table))
          ))
      )
    )
  )