(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
; (load "SentinelBaseLib.scm")

;;; Based on Python expr grammar
;; see https://docs.python.org/3/reference/expressions.html#conditional-expressions
;; to exclude "conditional_expression if conditional_expression else ..."
(define (ensure-or-test-expr . nodes)
  ;; list-of-type? is shown from code base.
  (assert (list-of-type? nodes GeneralNode?))
  (for-each
    (lambda (node)
      (assert* 
        (apply not-GeneralNode-with-token-type (cons node tag-with-precedence-lower-than-OR))
        (list node "has tag in" tag-with-precedence-lower-than-OR)
        )
      )
    nodes
    )
  )

;; to exclude "expression := expression"
(define (ensure-identifier . nodes)
  (assert (list-of-type? nodes GeneralNode?))
  (for-each
    (lambda (node)
      (assert* 
        (equal? ID-TAG-STR (get-GeneralNode-token-type node))
        (list node "isn't one identifier")
        )
      )
    nodes
    )
  )
;; Just for API compatibility.
(define (ensure-identifier* token return-handler . nodes)
  (declare (ignore token return-handler))
  (apply ensure-identifier nodes)
  )

(define (%ensure-var node)
  (and (not (%ensure-primary node))
    (ParseError (list node "can't be called"))
    )
  )
(define (ensure-var . nodes)
  (assert (list-of-type? nodes GeneralNode?))
  (for-each
    (lambda (node)
      (%ensure-var node)
      )
    nodes
    )
  )

;; to exclude "lambda expression-list: expression"
;; For simplicity, here I only consider identifier
;; NOT CONSIDERED:
;; ":" is also used in lambda, parameter_list_starargs https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-defparameter, "/" and "*" (all these can't be used independently)
;; "=" is also used in complexer https://docs.python.org/3/reference/simple_stmts.html#grammar-token-python-grammar-assignment_stmt
(define (arg-node? . nodes)
  (assert (list-of-type? nodes GeneralNode?))
  (every
    (lambda (elm) elm)
    (map
      (lambda (node)
        (let ((type (get-GeneralNode-token-type node)))
          (let ((res
                  (or
                    (equal? ID-TAG-STR type)
                    (equal? STAR-ARG-TAG-STR type)
                    ; (equal? "/" type)
                    ; (equal? "*" type)
                    )
                  ))
            ; (assert res)
            res
            )
          )
        
        )
      nodes
      )
    )
  )

(define (%ensure-type-in-list node lst)
  (assert (and (GeneralNode? node) (list-of-type? lst Token-type?)))
  (assert (member (get-GeneralNode-token-type node) lst))
  )
;; maybe not same as Python one https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-defparameter => https://peps.python.org/pep-0646/ => https://typing.python.org/en/latest/spec/generics.html#using-type-variable-tuples-in-functions
;; > x: Array[Height, Width] = Array()
(define (ensure-tuple . nodes)
  (assert (list-of-type? nodes GeneralNode?))
  (for-each
    (lambda (node)
      (%ensure-type-in-list node TUPLE-TYPES)
      )
    nodes
    )
  )

;; consistent means >= and other constraints (see ALL-NON-TOP-EXPR-TOKEN-TYPES)
(define (pred-ensuring-expr-with-consistent-precedence token)
  (lambda (node)
    (member 
      (get-GeneralNode-token-type node)
      (get-expr-token-types-with-consistent-prec token))
    )
  )
(define (ensure-consistent token return-handler . nodes)
  (let ((pred (pred-ensuring-expr-with-consistent-precedence token)))
    (for-each 
      (lambda (node) 
        (assert* (pred node) (list pred "based on" token "fails for" node "in ensure-consistent"))) 
      nodes)
    (return-handler nodes)
    )
  )

(define (relative-sentinel? sentinel)
  (and (procedure? sentinel)
    (equal? '(2 . #f) (procedure-arity sentinel))
    )
  )

(define (sentinel-for-one-node sentinel-proc token return-handler . nodes)
  (declare (ignore return-handler))
  (let ((ret (car nodes)))
    (assert* 
      (and 
        (null? (cdr nodes))
        (sentinel-proc token ret))
      (list "sentinel-for-one-node fails with" ret 
        "as the operand for op" token)
      )
    ret)
  )

;; Just check the Token-type similar to the above, e.g. ensure-identifier.
(define (%ensure-primary node)
  (let ((type (get-GeneralNode-token-type node)))
    (member
      type
      PRIMARY-TYPE-LIST
      )))
(define (ensure-primary . nodes)
  (assert (list-of-type? nodes GeneralNode?))
  (for-each
    (lambda (node)
      (assert*
        (%ensure-primary node)
        (list node "isn't one primary due to have no type in"
          PRIMARY-TYPE-LIST
          )  
        )
      )
    nodes
    )
  )
(define (ensure-primary* token return-handler . nodes)
  (declare (ignore token return-handler))
  (apply ensure-primary nodes)
  )
