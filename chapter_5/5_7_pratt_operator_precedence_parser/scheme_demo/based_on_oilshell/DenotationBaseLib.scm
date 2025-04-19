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

(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "pred_lib.scm")
(define (PrsNary* token p rbp #!optional elm-pred)
  (define (cons* a b)
    (if (null? a)
      b
      (cons a b))
    )
  ;; (p 'AtValidNud?) is more general than prsmatch-modified with just allowing other tokens. 
  (if (or (p 'AtToken "eof") (not (p 'AtValidNud?)))
    '()
    (let ((type (Token-type token)))
      (let lp ((l (list (check-pred elm-pred (p 'ParseUntil rbp)))))
        (if (p 'AtToken type)
          (begin 
            (p 'Eat type)
            (lp 
              (cons* 
                (if (or (p 'AtToken "eof") (not (p 'AtValidNud?)))
                  '()
                  (check-pred elm-pred (p 'ParseUntil rbp)))
                l)))
          (reverse l))
        )
      )
    )
  )

(define (PrsSeq parser delimeter-token left rbp header)
  (CompositeNode 
    delimeter-token
    (cons header (cons left (PrsNary* delimeter parser rbp))))
  )

(define (PrsPossibleSeq parser delimeter-token rbp header delimeter-lbp #!optional elm-pred)
  (let ((first-elm (parser 'ParseUntil delimeter-lbp)))
    (if (not (parser 'AtToken delimeter-token))
      first-elm
      (CompositeNode 
        delimeter-token
        (cons header (cons first-elm (PrsNary* delimeter-token parser rbp elm-pred))))
      )
    )
  )

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/pratt_new_compatible_with_MIT_GNU_Scheme/")
(load "compatible_lib.scm")
(define (consume-elems-and-the-ending-token p rbp ending-token-type header delimeter-token delimeter-prec #!optional elm-pred)
  (declare (ignore rbp))
  (prog1
    ;; Better to explicitly use one delimeter for future extension
    ;; Otherwise, this may allow (a;b;c;d;) etc.
    ; (p 'ParseUntil rbp)
    (PrsPossibleSeq p delimeter-token delimeter-prec header delimeter-prec elm-pred)
    (p 'Eat ending-token-type)))
;; 0. function as open-paren-nud based on prsmatch-modified.
;; 1. Different from oilshell (i.e. bash) to allow ()=>(tuple).
(define (consume-possible-elems-implicitly-and-the-ending-token p bp ending-token-type header delimeter-token delimeter-prec #!optional elm-pred)
  (cond 
    ((p 'AtToken ending-token-type) (CompositeNode (symbol->token header) (list header)))
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
      (consume-elems-and-the-ending-token p bp ending-token-type header delimeter-token delimeter-prec elm-pred)
      ))
  )

(define (LeftBinaryOp p token left rbp)
  (CompositeNode
    token
    (cons*
      ;; Here I assume to use ** etc even if it is not supported in original underlying interpreter.
      ;; Then it is that interpreter's duty to define ** before using **.
      (get-header-for-token token)
      left
      (p 'ParseUntil rbp)
      )
    )
  )
