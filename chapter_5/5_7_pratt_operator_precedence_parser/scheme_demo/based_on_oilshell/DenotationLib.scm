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
(define (LeftComma p token left rbp)
  ;; For SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/python_demo/pratt-parsing-demo/arith_parse.py
  ;; a,b,c is same as ((a,b),c).
  ;; But here (tuple (tuple a b) c) is obviously different from (tuple a b c),
  ;; so similar to SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/pratt_new_compatible_with_MIT_GNU_Scheme.scm
  ;; we use prsnary.
  (cons 'tuple (cons left (PrsNary token p)))
  ;;; IGNORE since tuple is returned and the 1st element may be also one tuple which should be concatenated,
  ;; we should not depend on the type of left.

  ;;; IGNORE Here I returned (tuple ...)
  )