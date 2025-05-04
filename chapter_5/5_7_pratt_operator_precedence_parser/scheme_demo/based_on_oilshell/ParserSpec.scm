(cd "~/SICP_SDF/SDF_exercises/common-lib")
; (load "logic_lib.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
; (load "DataTypeLib.scm")
(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
; (load "ExceptionLib.scm")

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
        ; (bkpt 'Null null-lookup)
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
        (set-tagged-pairs! token (LeftInfo led lbp rbp) left-lookup)
        (if (not (find-var token null-lookup))
          (set-tagged-pairs! token (NullInfo) null-lookup)
          )
        )
      tokens
      )
    )
  ;; TODO pratt_new_compatible_with_MIT_GNU_Scheme.scm
  ;; also considers non-arithmetic expression, so lbp=rbp may not always hold.
  (define (Left bp led tokens)
    (_RegisterLed bp bp led tokens)
    )
  (define (LeftRightAssoc bp led tokens)
    (_RegisterLed bp (- bp RIGHT-ASSOC-MINUS) led tokens)
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
  (let ((res (bundle ParserSpec? Null Left LeftRightAssoc LookupNull LookupLeft)))
    ; (trace LookupNull)
    ; (trace Null)
    res
    )
  )

(define ParserSpec? (make-bundle-predicate 'ParserSpec))
