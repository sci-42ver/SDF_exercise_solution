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
(define (Loc row col) (list row col))
(define Token-type cadr)
;; here token can be local due to we doesn't (set! token ...).
(define set-Token-type!
  (lambda (token type) 
    (assert (and (Token? token) (Token-type? type)))
    (set-car! (cdr token) type)))
(define (set-Token-type-same-as-val! token)
  (assert (Token? token))
  (let ((val (Token-val token)))
    (and
      (not (equal? (Token-type token) val))
      (set-Token-type! token val)
      )
    )
  )
(define Token-val caddr)
(define Token-type=? string=?)
(define Token-type? string?)
(define Token? (tagged-list-pred Token-tag))

(define (symbol->token sym)
  (assert (symbol? sym))
  (let ((str (symbol->string sym)))
    (Token str str))
  )

;; For simplicity, I just use cond instead of one table.
(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "string_lib.scm")
;; Better to be based on denotation type.
;; Here I just explicitly manipulate that inside denotation procedure.
(define (get-header obj)
  (assert (string? obj))
  (cond
    ((equal? obj ":=") 'define)
    ((member obj COMPARISON-OP-LST) 
      (symbol
        ;; chain behavior is implicitly done in PrsComparison.
        ; "chain-"
        (string-replace* obj " " "_")))
    ((equal? obj "|") 'bitwise-or)
    ((equal? obj "{") 'begin)
    ((equal? obj ",") 'tuple)
    (else
      (Token-val->Scheme-val obj)))
  )
(define (get-header-for-token token)
  (get-header (Token-val token))
  )

;;; Node
(define NodeTag 'Node)
(define (Node token)
  (assert (Token? token))
  (new-tagged-lst* NodeTag token))
(define Node? (tagged-list-pred NodeTag))
(define Node-Token cadr)
(define Token-val->Scheme-val string->symbol)
(define (get-Node-val node)
  (assert (Node? node))
  (Token-val->Scheme-val (Token-val (Node-Token node)))
  )
;; To offer more info so that we can reject some corner cases like "lambda non-arg, ...: ...".
(define CompositeNodeTag 'CompositeNode)
(define (CompositeNode root-token expr)
  (assert (Token? root-token))
  (new-tagged-lst* CompositeNodeTag root-token expr)
  )
(define CompositeNode? (tagged-list-pred CompositeNodeTag))
(define get-CompositeNode-expr caddr)

(define (GeneralNode? node)
  (or (Node node) (CompositeNode? node))
  )
;; Although here many possible duplicate assertions, it is safer.
(define (get-GeneralNode-token general-node)
  (assert (GeneralNode? general-node))
  (cadr general-node)
  )
(define (get-GeneralNode-token-type general-node)
  (assert (GeneralNode? general-node))
  (Token-type (get-GeneralNode-token general-node))
  )
(define (not-GeneralNode-with-token-type general-node . types)
  (assert (and (GeneralNode? general-node) (every Token-type? types)))
  (let ((type (get-GeneralNode-token-type general-node)))
    (not (member type types)))
  )
(define (get-GeneralNode-val general-node)
  (assert (GeneralNode? general-node))
  (cond 
    ((CompositeNode? general-node) (get-CompositeNode-expr general-node))
    ((Node? general-node) (get-Node-val general-node)))
  )

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "BpNumberLib.scm")

(define Null-Error-List (list ")" "]" ":" "eof" ";" "}" "then" "else"))

(define comma-token (Token "," ","))
; (define tuple-token comma-token)
(define semicolon-token (Token ";" ";"))
(define right-paren-token (Token ")" ")"))

;; always return one list
(define (get-possible-tuple-contents node-with-possible-tuple)
  (assert (GeneralNode? node-with-possible-tuple))
  (let ((node-val (get-GeneralNode-val node-with-possible-tuple)))
    (cond
      ((tuple? node-val) (cdr node-val))
      (else (list node-val)))
    )
  )

(define ALL-DENOTATION-TYPES '(Null Left LeftRightAssoc))

;; here set-Token-type! also works for local arg passed in. 
(define-syntax new-GeneralNode-simplified
  (syntax-rules ()
    ((_ possible-general-node token type)
      (begin
        (assert 
          (and
            (Token? token)
            (Token-type? type)))
        (set-Token-type! token type)
        (let ((intermediate possible-general-node))
          (CompositeNode
            token
            (cond 
              ((GeneralNode? intermediate) (get-GeneralNode-val intermediate))
              (else intermediate))
            )
          )
        )
      )
    ((_ possible-general-node token)
      (begin
        (assert (and (Token? token)))
        (let ((intermediate possible-general-node))  
          (CompositeNode
            token
            (cond 
              ((GeneralNode? intermediate) (get-GeneralNode-val intermediate))
              (else intermediate))
            )
          )
        )
      )
    )
  )

(define-syntax new-GeneralNode
  (syntax-rules ()
    ((_ possible-general-node token type)
      ;; This let is to avoid duplicate calculation
      ;; token is assumed to be identifier able to be set!.
      (let ((intermediate possible-general-node)
            (type* type)
            )
        (assert 
          (and
            (Token? token)
            (Token-type? type*)))
        ;; Use syntax here to ensure this work for the caller token instead of that local argument.
        (set-Token-type! token type*)
        (CompositeNode
          token
          (cond 
            ((GeneralNode? intermediate) (get-GeneralNode-val intermediate))
            (else intermediate))
          )
        )
      )
    )
  )

(define (new-GeneralNode-with-new-val general-node val-proc)
  (assert (GeneralNode? general-node))
  (CompositeNode
    (get-GeneralNode-token general-node)
    (val-proc (get-GeneralNode-val general-node))
    )
  )
