(cd "~/SICP_SDF/SDF_exercises/common-lib")
; (load "multi_hash_table_lib.scm")
;; https://stackoverflow.com/a/61180123/21294350
;; TODO after CRLS: use the most efficient based on the needs here.
(define *prec-list* (make-multi-hash))

;; op-type-lst is based on how LookupNull etc are used.
(define (spec-with-implicit-prec spec denotation-type handler op-type-lst #!optional header-lst prec token-type-lst)
  (assert
    (and 
      (denotation-type? denotation-type) 
      (procedure? handler) 
      (list-of-type? op-type-lst string?)))
  (hash-table-set! *handler-type-list* handler denotation-type)
  (for-each
    (lambda (val-lst-with-table)
      (let ((table (car val-lst-with-table))
            (val-lst (cdr val-lst-with-table)))
        (and* val-lst
          (begin
            (assert* (= (length op-type-lst) (length val-lst)) (list op-type-lst val-lst))
            (for-each
              (lambda (op val) (multi-hash-set! table val op denotation-type))
              op-type-lst val-lst
              )
            ))
        )
      )
    (list 
      (cons *header-table* header-lst) 
      (cons *token-type-list* token-type-lst)
      ;; for consistency
      (cons *prec-list*
        (if (default-object? prec)
          (default-object)
          (map (lambda (ignore) prec) op-type-lst)
          )
        )
      )
    )
  ;; assume op-type-lst having the same prec.
  (spec denotation-type (%get-prec (car op-type-lst) denotation-type) handler op-type-lst)
  )

(cd "~/SICP_SDF/SDF_exercises/common-lib")
; (load "list_lib.scm")
;;; Similar to get-header.
;; Notice this is based on token-type already modified based on context, like null-if or left-if.
(define (%get-prec op-str #!optional denotation-type)
  (assert (string? op-str))
  (let ((possible-prec 
          (apply-with-no-default-object-arg multi-hash-ref
            *prec-list* op-str denotation-type
            )))
    (cond 
      ((multi-hash-table? possible-prec)
        (get-the-only-elm
          (filter-map 
            (lambda (type) 
              (multi-hash-ref* *prec-list* op-str type)) 
            ALL-DENOTATION-TYPES)
          )
        )
      ((number? possible-prec) possible-prec)
      (else (error (list op-str "has no prec")))
      )
    )
  )
(define (get-prec-key-for-token token)
  (or
    (*token-type-list* 'getKeys* (Token-type token))
    (list (->str (Token-val token))) ; with only one nud or led.
    )
  )
(define (get-prec token)
  (assert (Token? token))
  (apply %get-prec (get-prec-key-for-token token))
  )
;; one alternative for get-prec-key-for-token.
(define (get-prec-by-token-type token-type)
  (assert (string? token-type))
  (let ((prec-key (*token-type-list* 'getKeys* token-type)))
    (assert prec-key)
    (apply %get-prec prec-key)
    )
  )

;; From Python
;; 0. Here I choose to start from if instead of :=
;; since := is not directly based on if/lambda
;; but if is directly based on or_test.
(define COMPARISON-OP-LST '("in" "not in" "is" "is not" "<" "<=" ">" ">=" "!=" "=="))
(define SHIFT-OP-LST '("<<" ">>"))
;; PM means plus minus
(define BINARY-PM-OP-LST '("+" "-"))
(define OTHER-BINARY-OP-LST '("*" "@" "/" "//" "%"))
(define UNARY-OP-LST '("+" "-" "~"))
(define AWAIT-OP-LST '("await"))
;; Notice here we only put or-op which is used in (MakePythonParserSpec) instead of type-str.
(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "BpNumberBaseLib.scm")
(define prec-list-higher-than-or-op
  `(,LEFT-IF-BP
    .
    ((Left ("or"))
      (Left ("and"))
      (Null ("not"))
      ;; Add one total name since a<=b<c etc are thought as one whole object instead of (a<=b)<c.
      ;; For details, see Python doc.
      (Left (,@COMPARISON-OP-LST))
      (Left ("|"))
      ;; see SDF_exercises/scheme_primitive_tests/bundle.scm
      ;; 'left is same as 'Left.
      (left ("^"))
      (left ("&"))
      (left ,SHIFT-OP-LST)
      (left ,BINARY-PM-OP-LST)
      (left ,OTHER-BINARY-OP-LST)
      ;; > u_expr ::= power | "-" u_expr | "+" u_expr | "~" u_expr
      ;; here rhs-lbp>=self-rbp
      ;; it allows "-+1 => -1" etc.
      (null ,UNARY-OP-LST)
      ;; > power ::= (await_expr | primary) ["**" u_expr]
      ;; lhs-prec-level is higher than ** which is ensured by lhs-rbp>**-lbp
      ;; rhs allows *only* one nud u_expr or op's with prec >= self.
      (LeftRightAssoc ("**"))
      ;; > await_expr ::= "await" primary
      ;; no nud
      (null ,AWAIT-OP-LST)
      ))
  )
(define bp-val-list-higher-than-or-op '())
(define (init-prec-list-higher-than-or-op)
  (let ((base-bp (car prec-list-higher-than-or-op))
        (prec-lst (cdr prec-list-higher-than-or-op)))
    (let lp ((depth 1) (rest-lst prec-lst))
      (and
        (not (null? rest-lst))
        (let ((item (car rest-lst)) (bp (+ base-bp (* depth BP-STEP))))
          (let ((type (car item))
                (op-lst (cadr item)))
            (for-each
              (lambda (op)
                (multi-hash-set! *prec-list* bp op type)
                )
              op-lst
              )
            ;; For debug
            (set! bp-val-list-higher-than-or-op 
              (cons (list bp item) bp-val-list-higher-than-or-op))
            (lp (+ depth 1) (cdr rest-lst))
            )
          ))
      )
    )
  )

;;; Initialization
(define (init-prec-list)
  ;; regex: "\(make-prec-key ([^ ]+) ([^ ]+)\) ([^ ]+)\)" => "$3 $2 $1"
  (multi-hash-set! *prec-list* COMMA-BP "," 'Left)
  (multi-hash-set! *prec-list* LEFT-PAREN-BP "(" 'Left)
  (multi-hash-set! *prec-list* NULL-PAREN-BP "(" 'Null)
  (multi-hash-set! *prec-list* NULL-BRACE-BP "{" 'Null)
  (multi-hash-set! *prec-list* LEFT-SEMICOLON-BP ";" 'Left)
  (multi-hash-set! *prec-list* NULL-IF-BP "if" 'Null)

  (multi-hash-set! *prec-list* LAMBDA-RBP "lambda" 'Null)
  (multi-hash-set! *prec-list* LEFT-IF-BP "if" 'LeftRightAssoc)
  (multi-hash-set! *prec-list* :=-BP ":=" 'Left)
  (init-prec-list-higher-than-or-op)
  )
(init-prec-list)
(define *handler-type-list* (default-hash-table-constructor))
