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

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/pratt_new_compatible_with_MIT_GNU_Scheme/")
(load "compatible_lib.scm")
(define (consume-elems-and-the-ending-token p rbp ending-token-type header delimeter-token delimeter-prec)
  (declare (ignore rbp))
  (prog1
    ;; Better to explicitly use one delimeter for future extension
    ;; Otherwise, this may allow (a;b;c;d;) etc.
    ; (p 'ParseUntil rbp)
    (PrsPossibleSeq p delimeter-token delimeter-prec header delimeter-prec)
    (p 'Eat ending-token-type)))
;; 0. function as open-paren-nud based on prsmatch-modified.
;; 1. Different from oilshell (i.e. bash) to allow ()=>(tuple).
(define (consume-possible-elems-implicitly-and-the-ending-token p bp ending-token-type header delimeter-token delimeter-prec)
  (cond 
    ((p 'AtToken ending-token-type) (list header))
    (else
      ;; 0. We can implicitly use LeftComma implied by grammar rule
      ;; Parenthesized form https://docs.python.org/3/reference/expressions.html#parenthesized-forms
      ;; is based on Expression lists https://docs.python.org/3/reference/expressions.html#expression-lists
      ;; 1. see pratt-parsing-demo/tests.py
      ;; x[1,2] implies also considering tuple inherently.
      ;; 2. Here I choose ParseUntil to reuse the above LeftComma for modularity.
      ;; 3. Similar to loop in prsmatch-modified but with
      ;; 3.a. ending-token consumption at the end
      ;; 3.b. element list construction is implicitly done in ParseUntil.
      ;; 3.c. (error 'comma-or-match-not-found (token-read stream)) is implicitly done
      ;; by (p 'Eat ")") but more general to allow possible extension like (a;b;).
      (consume-elems-and-the-ending-token p bp ending-token-type header delimeter-token delimeter-prec)
      ))
  )
