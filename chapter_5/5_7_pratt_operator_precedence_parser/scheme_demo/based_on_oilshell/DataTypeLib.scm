;;; used by both Parser.scm and ParserSpec.scm.
;; Different from Python, MIT/GNU Scheme doesn't have one way to only offer bp val. But Racket can.
;; see SDF_exercises/chapter_5/5_7_naive_algorithm_for_operator_precedence_parser/5_7_precedence_lib.scm
(define (NullInfo #!optional nud bp)
  ;; Different from pratt_new_compatible_with_MIT_GNU_Scheme.scm
  ;; Here it defaults to be minimal bp. Anyway we should not rely the default value but always explicit set one expected bp.
  ;; After all, here bp being 0 always means nud is NullError, so rbp won't be used.
  (list (or* nud NullError) (or* bp 0))
  )
;; IMHO default bp being 0 is appropriate, since those unset ones should not bind anything,
;; so its lbp is least.
;; Then led won't be called, so similar to the above, rbp won't be used.
(define (LeftInfo #!optional led lbp rbp)
  (list (or* led LeftError) (or* lbp 0) (or* rbp 0))
  )
(define get-nud car)
(define get-null-bp cadr)
(define get-led car)
(define get-left-lbp cadr)
(define get-left-rbp caddr)

;;; used by SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell/Parser.scm
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
(cd "~/SICP_SDF/SDF_exercises/common-lib/")
(load "tagged_lst_lib.scm")
(define Token? (tagged-list-pred Token-tag))

(define (symbol->token sym)
  (assert (symbol? sym))
  (let ((str (symbol->string sym)))
    (Token str str))
  )

(define denotation-type? symbol?)

;; For simplicity, I just use cond instead of one table.
(cd "~/SICP_SDF/SDF_exercises/common-lib")
; (load "string_lib.scm")
;; 0. Better to be based on denotation type.
;; Here I just explicitly manipulate that inside denotation procedure.
(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "multi_hash_table_lib.scm")
(define *header-table* (make-multi-hash))
(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/")
(load "based_on_oilshell/BpNumberLib.scm")
(define (init-header-table)
  (multi-hash-set! *header-table* 'define ":=")
  (for-each
    (lambda (op-str)
      (multi-hash-set! 
        *header-table* 
        (symbol
          ;; chain behavior is implicitly done in PrsComparison.
          ; "chain-"
          (string-replace* op-str " " "_"))
        op-str))
    COMPARISON-OP-LST
    )
  (multi-hash-set! *header-table* 'bitwise-or "|")
  (multi-hash-set! *header-table* 'begin "{")
  (multi-hash-set! *header-table* 'tuple ",")
  (multi-hash-set! *header-table* 'expt "**")
  )
(init-header-table)
(define (get-header op-str #!optional denotation-type)
  (assert (string? op-str))
  (and* denotation-type (assert (denotation-type? denotation-type)))
  (let ((val (apply-with-no-default-object-arg multi-hash-ref* *header-table* op-str denotation-type)))
    (cond 
      ((multi-hash-table? val)
       (assert (default-object? denotation-type))
       (get-the-only-elm
          (filter-map 
            (lambda (type) (multi-hash-ref* val type)) 
            ALL-DENOTATION-TYPES)
          )
       )
      (val val)
      (else (Token-val->Scheme-val op-str)))
    )
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

(define (CompositeNode-with-binary-expr root-token left-node right-node)
  (CompositeNode
    token
    (cons*-wrapper
      (get-header-for-token token)
      (get-GeneralNode-val left-node)
      (get-GeneralNode-val right-node)
      )
    )
  )

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
; (load "BpNumberLib.scm")

(define Null-Error-List (list ")" "]" ":" "eof" ";" "}" "then" "else"))
(define EOF-TOKEN (Token "eof" "eof"))

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
