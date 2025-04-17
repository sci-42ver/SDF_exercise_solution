;; 0. p is parser
;; 1. Just like loop in prsmatch but not read the ending token before reverse.
(define (PrsNary token p)
  (let ((type (Token-type token)))
    (let lp ((l (list (p 'ParseUntil (get-left-rbp token)))))
      (if (p 'AtToken type)
        (begin 
          (p 'Eat type)
          (lp (cons (p 'ParseUntil (get-left-rbp token)) l)))
        (reverse l))
      )
    )
  )

(define (PrsNary* token p)
  (define (cons* a b)
    (if (null? a)
      b
      (cons a b))
    )
  ;; (p 'AtValidNud?) is more general than prsmatch-modified with just allowing other tokens. 
  (if (or (p 'AtToken "eof") (not (p 'AtValidNud?)))
    '()
    (let ((type (Token-type token)))
      (let lp ((l (list (p 'ParseUntil (get-left-rbp token)))))
        (if (p 'AtToken type)
          (begin 
            (p 'Eat type)
            (lp 
              (cons* 
                (if (or (p 'AtToken "eof") (not (p 'AtValidNud?)))
                  '()
                  (p 'ParseUntil (get-left-rbp token)))
                l)))
          (reverse l))
        )
      )
    )
  )

;; Just like the above loop.
; (define (PrsNaryAndEatEndingToken parameters)
;   (let ((elms (PrsNary splitter-token parser)))
;     (parser 'Eat (Token-type end-token))
;     elms
;     )
;   )

; (define (prsmatch end-token splitter-token parser)
  
;   )
