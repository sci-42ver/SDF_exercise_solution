;;; used by both Parser.scm and ParserSpec.scm.
;; Different from Python, MIT/GNU Scheme doesn't have one way to only offer bp val. But Racket can.
;; see SDF_exercises/chapter_5/5_7_naive_algorithm_for_operator_precedence_parser/5_7_precedence_lib.scm
(define (NullInfo #!optional nud bp)
  ;; Different from pratt_new_compatible_with_MIT_GNU_Scheme.scm
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
(cd "~/SICP_SDF/SDF_exercises/common-lib/")
(load "tagged_lst_lib.scm")
(define Token-tag 'token)
(define (Token type val #!optional loc)
  (declare (ignore loc))
  (new-tagged-lst* Token-tag type val)
  )
(define Token-type cadr)
(define set-Token-type! 
  (lambda (token type) 
    (assert (Token? token)) 
    (set-car! (cdr token) type)))
(define Token-val caddr)
(define Token-type=? string=?)
(define Token? (tagged-list? Token-tag))

(define (symbol->token sym)
  (assert (symbol? sym))
  (let ((str (symbol->string sym)))
    (Token str str))
  )

(define (Loc row col) (list row col))

(define NodeTag 'Node)
(define (Node token)
  (assert (Token? token))
  (new-tagged-lst* NodeTag token))
(define Node? (tagged-list? NodeTag))
;; To offer more info so that we can reject some corner cases like "lambda non-arg, ...: ...".
(define CompositeNodeTag 'CompositeNode)
(define (CompositeNode root-token expr)
  (assert (Token? root-token))
  (new-tagged-lst* CompositeNodeTag root-token expr)
  )
(define CompositeNode? (tagged-list? CompositeNodeTag))

(define (GeneralNode? node)
  (or (Node node) (CompositeNode? node))
  )
(define (get-GeneralNode-token general-node)
  (assert (GeneralNode? general-node))
  (cadr general-node)
  )

(define (arg-node? node)
  (assert (GeneralNode? node))
  (let ((type (Token-type (get-GeneralNode-token node))))
    (or
      (equal? "id" type)
      (equal? "star-arg" type)
      )
    )
  )

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "PrecNumberLib.scm")

(define Null-Error-List (list ")" "]" ":" "eof" ";" "}" "then" "else"))

(define comma-token (Token "," ","))
; (define tuple-token comma-token)
(define semicolon-token (Token ";" ";"))
(define right-paren-token (Token ")" ")"))

;; always return one list
(define (get-possible-tuple-contents possible-tuple)
  (cond
    ((tuple? possible-tuple) (cdr possible-tuple))
    (else (list possible-tuple)))
  )

;; see SDF_exercises/chapter_5/5_7_re_lib/5_7_regexp_lib_simplified_based_on_effbot_based_on_irregex.scm
(define var-types (list "id" "get"))
