;;; used by both Parser.scm and ParserSpec.scm.
;; Different from Python, MIT/GNU Scheme doesn't have one way to only offer bp val. But Racket can.
;; see SDF_exercises/chapter_5/5_7_naive_algorithm_for_operator_precedence_parser/5_7_precedence_lib.scm
(define (NullInfo #!optional nud bp)
  ;; Different from SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/pratt_new_compatible_with_MIT_GNU_Scheme.scm
  ;; Here it defaults to be minimal bp. Anyway we should not rely the default value but always explicit set one expected bp.
  (list (or* nud NullError) (or* bp 0))
  )
(define (LeftInfo #!optional led lbp rbp)
  (list (or* led LeftError) (or* lbp 0) (or* rbp 0))
  )
(define get-nud car)
(define get-null-bp cadr)
(define get-led car)
(define get-left-lbp cadr)
(define get-left-rbp caddr)

;;; used by SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell/Parser.scm
(define (Token type val #!optional loc)
  (declare (ignore loc))
  (list type val)
  )
(define Token-type car)
(define Token-val cadr)
(define Token-type=? string=?)

(define (Loc row col) (list row col))
