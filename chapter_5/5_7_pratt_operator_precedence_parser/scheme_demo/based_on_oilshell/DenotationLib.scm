(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "loop_lib.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell/")
(load "DataTypeLib.scm")
(load "Parser.scm")

;; p is parser
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
;; allow trailing which is not allowed in Shell (in Bash $((1,)) throws error).
;; TODO tests: 1,;1,2,;1,+2,;
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
(define (LeftComma p token left rbp)
  ;; 0. For SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/python_demo/pratt-parsing-demo/arith_parse.py
  ;; a,b,c is same as ((a,b),c).
  ;; But here (tuple (tuple a b) c) is obviously different from (tuple a b c),
  ;; so similar to SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/pratt_new_compatible_with_MIT_GNU_Scheme.scm
  ;; we use prsnary.
  ;; 1. For the trailing comma https://docs.python.org/3/reference/expressions.html#expression-lists,
  ;; we check whether we can get one new nud, see the above.
  ;; Emm... I won't dig into the complex syntax grammar rules to find the detailed examples where trailing "," is allowed...
  (cons 'tuple (cons left (PrsNary* token p)))
  ;;; IGNORE since tuple is returned and the 1st element may be also one tuple which should be concatenated,
  ;; we should not depend on the type of left.

  ;;; IGNORE Here I returned (tuple ...)
  )