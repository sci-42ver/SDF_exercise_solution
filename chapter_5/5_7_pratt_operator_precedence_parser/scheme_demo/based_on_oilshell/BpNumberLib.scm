(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "BpNumberBaseLib.scm")
(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "multi_hash_table_lib.scm")
;; https://stackoverflow.com/a/61180123/21294350
;; TODO after CRLS: use the most efficient based on the needs here.
(define *prec-list* (make-multi-hash make-equal-hash-table))
; (define prec-key-tag 'prec-key)
; (define (make-prec-key denotation-type op-str)
;   (assert (and (symbol? denotation-type) (string? op-str)))
;   (new-tagged-lst* prec-key-tag denotation-type op-str)
;   )
; (define prec-key? (tagged-list-pred prec-key-tag))

(define (spec-with-implicit-prec denotation-type handler op-lst)
  (assert
    (and 
      (symbol? denotation-type) 
      (procedure? handler) 
      (list-of-type? op-lst string?)))
  (hash-table-set! *handler-type-list* handler denotation-type)
  ;; assume op-lst having the same prec.
  (spec denotation-type (get-prec (car op-lst)) handler op-lst)
  )

;; Notice this is based on token-type already modified based on context, like null-if or left-if.
(define (%get-prec op-str #!optional denotation-type)
  (assert (string? op-str))
  (let ((possible-prec 
          (apply multi-hash-ref 
            (cons* *prec-list* op-str
              (if (default-object? denotation-type)
                '()
                (list denotation-type))
              ))))
    (cond 
      ((multi-hash-table? possible-prec)
        (let ((all-possible-precs 
                (filter-map 
                  (lambda (type) (multi-hash-ref* *prec-list* op-str type)) 
                  ALL-DENOTATION-TYPES)))
          (assert (= 1 (length all-possible-precs)))
          (car all-possible-precs)
          )
        )
      ((number? possible-prec) possible-prec)
      (else (error (list op-str "has no prec")))
      )
    )
  )
(define (get-prec-key-for-token-type token-type)
  (or
    (*token-type-list* 'getKeys token-type)
    (list token-type) ; with only one nud or led.
    )
  )
(define (get-prec token-type)
  (assert (string? token-type))
  (apply %get-prec (get-prec-key-for-token-type token-type))
  )

;; From Python
;; 0. Here I choose to start from if instead of :=
;; since := is not directly based on if/lambda
;; but if is directly based on or_test.
(define prec-list-higher-than-or-op
  `(,LEFT-IF-BP
    .
    ((Left ("or"))
      (Left ("and"))
      (Null ("not"))
      (Left ("in","<"))
      (Left ("|"))
      ;; see SDF_exercises/scheme_primitive_tests/bundle.scm
      ;; 'left is same as 'Left.
      (left ("^"))
      (left ("&"))
      (left ("<<" ">>"))
      (left ("+" "-"))
      (left ("*" "@" "/" "//" "%"))
      ;; > u_expr ::= power | "-" u_expr | "+" u_expr | "~" u_expr
      ;; here rhs-lbp>=self-rbp
      ;; it allows "-+1 => -1" etc.
      (null ("+" "-" "~"))
      ;; > power ::= (await_expr | primary) ["**" u_expr]
      ;; lhs-prec-level is higher than ** which is ensured by lhs-rbp>**-lbp
      ;; rhs allows *only* one nud u_expr or op's with prec >= self.
      (left ("**"))
      ;; > await_expr ::= "await" primary
      ;; no nud
      (null ("await"))
      ))
  )
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
                (hash-table-set! *prec-list* (make-prec-key type op) bp)
                )
              op-lst
              )
            (lp (+ depth 1) (cdr rest-lst))
            )
          ))
      )
    )
  )

;;; Initialization
(define (init-prec-list)
  (hash-table-set! *prec-list* (make-prec-key 'Left ",") COMMA-BP)
  (hash-table-set! *prec-list* (make-prec-key 'Left "(") LEFT-PAREN-BP)
  (hash-table-set! *prec-list* (make-prec-key 'Null "(") NULL-PAREN-BP)
  (hash-table-set! *prec-list* (make-prec-key 'Null "{") NULL-BRACE-BP)
  (hash-table-set! *prec-list* (make-prec-key 'Left ";") LEFT-SEMICOLON-BP)
  (hash-table-set! *prec-list* (make-prec-key 'Null "if") NULL-IF-BP)

  (hash-table-set! *prec-list* (make-prec-key 'Null "lambda") LAMBDA-RBP)
  (hash-table-set! *prec-list* (make-prec-key 'Left "if") LEFT-IF-BP)
  (hash-table-set! *prec-list* (make-prec-key 'Left ":=") :=-BP)
  (init-prec-list-higher-than-or-op)
  )
(init-prec-list)
(define *handler-type-list* (make-equal-hash-table))
