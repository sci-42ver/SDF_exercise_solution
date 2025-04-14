; (define-syntax feature-tests
;   (let ((idx 0))
;     (syntax-rules ()
;       ((_ feature1 feature2 ...)
;         (begin
;           (cond-expand
;             (feature1
;               (write-line "feature" idx "is supported"))
;             (else))
;           (set! idx (+ 1 idx))
;           (feature-tests feature2 ...)))
;       ((_)
;         (begin
;           (set! idx 0)
;           'finished))
;       ))
;   )
;; ;Keyword binding value must be a keyword

(define-syntax %feature-tests
  (syntax-rules ()
    ((_ idx feature1 feature2 ...)
      (begin
        (cond-expand
          (feature1
            (write-line (list "feature" idx "is supported")))
          (else))
        (%feature-tests (+ 1 idx) feature2 ...)))
    ((_ idx)
      'finished)
    )
  )
(define-syntax feature-tests
  (syntax-rules ()
    ((_ feature1 ...)
      (%feature-tests 0 feature1 ...)
      )
    )
  )

(feature-tests regexp-non-greedy regexp-look-around regexp-backrefs regexp-unicode)
