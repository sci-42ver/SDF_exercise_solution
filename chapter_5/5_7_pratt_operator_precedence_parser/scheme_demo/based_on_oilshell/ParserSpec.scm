(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "pair_lib.scm")
(load "logic_lib.scm")

(define (NullError p token bp)
  (error (list token "can't be used in prefix position")))
(define (LeftError p token left rbp)
  (error (list token "can't be used in infix position")))

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "DataTypeLib.scm")

;;; I use bundle to mimic class.
(define (ParserSpec)
  ;; init
  ;; For simplicity, here I just uses alist since I haven't learnt about how to choose hash table.
  ;; Anyway based on abstraction, this will only influence APIs.
  (define null-lookup (empty-tagged-lst 'null-lookup))
  (define left-lookup (empty-tagged-lst 'left-lookup))

  ;; Null etc is much better than defsyntax since its interface is *fixed*.
  (define (Null bp nud tokens)
    (for-each
      (lambda (token)
        (set-tagged-pairs! token (NullInfo nud bp) null-lookup)
        (if (not (find-var token left-lookup))
          (set-tagged-pairs! token (LeftInfo) left-lookup)
          )
        )
      tokens
      )
    )
  (define (_RegisterLed lbp rbp led tokens)
    (for-each
      (lambda (token)
        (set-tagged-pairs! token (LeftInfo lbp rbp led) left-lookup)
        (if (not (find-var token null-lookup))
          (set-tagged-pairs! token (NullInfo) null-lookup)
          )
        )
      tokens
      )
    )
  ;; TODO SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/pratt_new_compatible_with_MIT_GNU_Scheme.scm
  ;; also considers non-arithmetic expression, so lbp=rbp may not always hold.
  (define (Left bp led tokens)
    (_RegisterLed bp bp led tokens)
    )
  (define (LeftRightAssoc bp led tokens)
    (_RegisterLed bp (- bp 1) led tokens)
    )
  (define (LookupNull token)
    (let ((res (find-var token null-lookup)))
      (if res
        (get-right res)
        (error (list "Unexpected token" token))))
    )
  (define (LookupLeft token)
    (let ((res (find-var token left-lookup)))
      (if res
        (get-right res)
        (error (list "Unexpected token" token))))
    )
  (bundle ParserSpec? Null Left LeftRightAssoc LookupNull LookupLeft)
  )

(define ParserSpec? (make-bundle-predicate 'ParserSpec))
