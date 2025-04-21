;;;;;; Tag strings
(define LEFT-IF-TYPE-STR "left-if")
(define :=-TYPE-STR "define")
(define EXPR-LIST-TYPE-STR "expr-list")
(define STATEMENT-BLOCK-TYPE-STR "statement-block")
(define IF-STATEMENT-TYPE-STR "null-if")
;; see 5_7_regexp_lib_simplified_based_on_effbot_based_on_irregex.scm
(define VAR-TYPES (list ID-TAG-STR "get"))
(define TUPLE-TYPES `(,@VAR-TYPES "tuple"))

(define non-expr-tag 
  `(,:=-TYPE-STR 
    ,EXPR-LIST-TYPE-STR 
    ,STATEMENT-BLOCK-TYPE-STR 
    ,IF-STATEMENT-TYPE-STR))
;; implicitly consider expr precedence is higher than non-expr-tag.
(define tag-below-OR `("lambda" ,LEFT-IF-TYPE-STR ,@non-expr-tag))

;;; Based on Python expr grammar
;; see https://docs.python.org/3/reference/expressions.html#conditional-expressions
;; to exclude "conditional_expression if conditional_expression else ..."
(define (ensure-or-test-expr . nodes)
  ;; list-of-type? is shown from code base.
  (assert (list-of-type? nodes GeneralNode?))
  (for-each
    (lambda (node)
      (assert (apply not-GeneralNode-with-token-type (cons node tag-below-OR)))
      )
    nodes
    )
  )

;; to exclude "expression := expression"
(define (ensure-identifier . nodes)
  (assert (list-of-type? nodes GeneralNode?))
  (for-each
    (lambda (node)
      (assert (equal? ID-TAG-STR (get-GeneralNode-token-type node)))
      )
    nodes
    )
  )

(define (%ensure-type-in-list node lst)
  (assert (and (GeneralNode? node) (list-of-type? lst Token-type?)))
  (assert (member (get-GeneralNode-token-type node) lst))
  )
(define (%ensure-var node)
  (and (not (member (get-GeneralNode-token-type node) VAR-TYPES))
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
  (for-each
    (lambda (node)
      (let ((type (get-GeneralNode-token-type node)))
        (let ((res
                (or
                  (equal? ID-TAG-STR type)
                  ; (equal? STAR-ARG-TAG-STR type)
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

;; maybe not same as Python one https://typing.python.org/en/latest/spec/generics.html#using-type-variable-tuples-in-functions
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
