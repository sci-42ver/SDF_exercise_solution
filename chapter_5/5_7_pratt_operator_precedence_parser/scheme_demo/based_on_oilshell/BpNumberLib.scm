(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "BpNumberBaseLib.scm")
;; https://stackoverflow.com/a/61180123/21294350
;; TODO after CRLS: use the most efficient based on the needs here.
(define *prec-list* (make-equal-hash-table))
(define prec-key-tag 'prec-key)
(define (make-prec-key denotation-type op-str)
  (assert (and (symbol? denotation-type) (string? op-str)))
  (new-tagged-lst* prec-key-tag denotation-type op-str)
  )
(define prec-key? (tagged-list-pred prec-key-tag))

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
(define (%get-prec prec-key)
  (assert (prec-key? prec-key))
  (let ((prec (hash-table-ref* *prec-list* prec-key)))
    (and (not prec) (error (list "no prec is found for" prec-key)))
    prec
    )
  )
(define (get-prec token-type)
  (assert (string? token-type))
  (%get-prec (*token-type-list* 'getKey token-type))
  )

;;; Initialization
(define (init-prec-list)
  (hash-table-set! *prec-list* (make-prec-key 'Left ",") COMMA-BP)
  (hash-table-set! *prec-list* (make-prec-key 'Left "(") LEFT-PAREN-BP)
  (hash-table-set! *prec-list* (make-prec-key 'Null "(") NULL-PAREN-BP)
  (hash-table-set! *prec-list* (make-prec-key 'Null "{") NULL-BRACE-BP)
  (hash-table-set! *prec-list* (make-prec-key 'Left ";") LEFT-SEMICOLON-BP)

  (hash-table-set! *prec-list* (make-prec-key 'Null "lambda") LAMBDA-RBP)
  (hash-table-set! *prec-list* (make-prec-key 'Null "if") NULL-IF-BP)
  (hash-table-set! *prec-list* (make-prec-key 'Left ":=") :=-BP)
  )
(init-prec-list)
(define *handler-type-list* (make-equal-hash-table))
