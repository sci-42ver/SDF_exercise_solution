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

(define (PrsNary* token p rbp)
  (define (cons* a b)
    (if (null? a)
      b
      (cons a b))
    )
  ;; (p 'AtValidNud?) is more general than prsmatch-modified with just allowing other tokens. 
  (if (or (p 'AtToken "eof") (not (p 'AtValidNud?)))
    '()
    (let ((type (Token-type token)))
      (let lp ((l (list (p 'ParseUntil rbp))))
        (if (p 'AtToken type)
          (begin 
            (p 'Eat type)
            (lp 
              (cons* 
                (if (or (p 'AtToken "eof") (not (p 'AtValidNud?)))
                  '()
                  (p 'ParseUntil rbp))
                l)))
          (reverse l))
        )
      )
    )
  )

(define (PrsSeq parser delimeter left rbp header)
  (cons header (cons left (PrsNary* delimeter parser rbp)))
  )

(define (PrsPossibleSeq parser delimeter rbp header delimeter-lbp)
  (let ((first-elm (parser 'ParseUntil delimeter-lbp)))
    (if (not (parser 'AtToken delimeter))
      first-elm
      (cons header (cons first-elm (PrsNary* delimeter parser rbp)))
      )
    )
  )
