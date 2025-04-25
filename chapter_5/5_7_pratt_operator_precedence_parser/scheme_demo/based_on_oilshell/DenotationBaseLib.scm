;; 0. p is parser
;; 1. Just like loop in prsmatch but not read the ending token before reverse.
(cd "~/SICP_SDF/SDF_exercises/common-lib/")
(load "base_procedure_lib.scm")
(define (PrsNary token p rbp #!optional elm-relative-assertion)
  (or* elm-relative-assertion (set! elm-relative-assertion return-last))
  (let ((type (Token-type token)))
    (let lp ((l (list (get-GeneralNode-val (elm-relative-assertion token return-last (p 'ParseUntil rbp))))))
      (if (p 'AtToken type)
        (begin 
          (p 'Eat type)
          (lp (cons (get-GeneralNode-val (elm-relative-assertion token return-last (p 'ParseUntil rbp))) l)))
        (reverse l))
      )
    )
  )

(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "pred_lib.scm")
;; 0. returns a list of expr's instead of GeneralNode's.
(define (PrsNary* token p rbp #!optional elm-pred)
  (define (cons-with-possible-first-elm a b)
    (if (null? a)
      b
      (cons a b))
    )
  ;; (p 'AtValidNud?) is more general than prsmatch-modified with just allowing other tokens. 
  (if (or (p 'AtToken "eof") (not (p 'AtValidNud?)))
    '()
    (let ((type (Token-type token)))
      (let lp ((l 
                (list 
                  (get-GeneralNode-val 
                    (check-pred elm-pred (p 'ParseUntil rbp))))))
        (if (p 'AtToken type)
          (begin 
            (p 'Eat type)
            (lp 
              (cons-with-possible-first-elm
                (if (or (p 'AtToken "eof") (not (p 'AtValidNud?)))
                  '()
                  (get-GeneralNode-val
                    (check-pred elm-pred (p 'ParseUntil rbp)))
                  )
                l)))
          (reverse l))
        )
      )
    )
  )

(define (%PrsSeq base-parse-proc parser delimeter-token left rbp #!optional elm-pred header)
  (CompositeNode 
    delimeter-token
    (cons (or* header (get-header-for-token delimeter-token)) 
      (cons (get-GeneralNode-val left) (base-parse-proc delimeter parser rbp elm-pred)))))

;; 0. TODO better to make elm-relative-assertion and elm-pred have similar APIs.
;; Why I used 2 inconsistent ones is due to I didn't have one *unchanged* plan for this a bit big project at least for one programming beginner book SDF.
;; How to construct one *compiler* project (or sub-project here) from the ground can't be taught in that book...
;; 1. Here delimeter-token type may be used by elm-relative-assertion, e.g. ensure-consistent,
;; and to match delimeter tokens by type later.
;; This is the same case for PrsSeq*->PrsNary*.
;; So *NOT CHANGE* this delimeter-token type beforehand.
(define (PrsSeq parser delimeter-token left rbp #!optional elm-relative-assertion header)
  (use-possible-default-object-proc elm-relative-assertion delimeter-token return-last left)
  (%PrsSeq PrsNary parser delimeter-token left rbp elm-relative-assertion header)
  )

(define (PrsSeqWithOpBetweenOrAndAwait parser delimeter-token left rbp #!optional header)
  (PrsSeq parser delimeter-token left rbp ensure-consistent)
  )
(define (PrsSeqWithSentinel p token left rbp)
  (PrsSeqWithOpBetweenOrAndAwait p token left rbp))

;; similar to PrsSeq, but allow a<=b<c<d>e
;; If using PrsSeq, after having (<= a b), either continue comparison or not based on AtToken.
;; If continue, (ParseWithLeft still-same-rbp "b") => (< b c d).
;; Then we have (and (<= a b) (< b c d))
(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "tree_lib.scm")
(define (%PrsComparison p token left rbp #!optional elm-relative-assertion header)
  (let lp ((cur-node
            (new-GeneralNode-with-new-val
              (PrsSeq p token left rbp elm-relative-assertion header)
              (lambda (expr) (list 'and expr))
              )))
    ;; notice here we use PrsComparison instead of %PrsComparison
    (let ((final-node-type (get-token-type-from-caller-and-op PrsComparison token)))
      (if (any (lambda (type) (p 'AtToken type)) COMPARISON-OP-LST)
        ;; based on assumption that all op's in COMPARISON-OP-LST have the same rbp.
        (let* ((cur-expr (get-GeneralNode-val cur-node))
                (rest-expr 
                    (get-GeneralNode-val
                      (p
                        'ParseWithLeft 
                        (last-in-tree cur-expr) 
                        rbp))))
          (new-GeneralNode-simplified
            (append cur-expr (get-tagged-lst-data rest-expr))
            token
            final-node-type
            )
          )
        (new-GeneralNode-simplified
          cur-node
          token
          final-node-type
          )
      ))
  )
)

(define (PrsSeq* parser delimeter-token left rbp #!optional elm-pred header)
  (assert (elm-pred left))
  (%PrsSeq PrsNary* parser delimeter-token left rbp elm-pred header)
  )

(define (PrsPossibleSeq* parser delimeter-token rbp delimeter-lbp #!optional elm-pred header)
  (let ((first-elm (parser 'ParseUntil delimeter-lbp)))
    (assert (elm-pred first-elm))
    (if (not (parser 'AtToken delimeter-token))
      first-elm
      (PrsSeq* parser delimeter-token first-elm rbp elm-pred header)
      )
    )
  )

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/pratt_new_compatible_with_MIT_GNU_Scheme/")
(load "compatible_lib.scm")
(define (consume-elems-and-the-ending-token p rbp ending-token-type delimeter-token delimeter-prec #!optional elm-pred header)
  (declare (ignore rbp))
  (prog1
    ;; Better to explicitly use one delimeter for future extension
    ;; Otherwise, this may allow (a;b;c;d;) etc.
    ; (p 'ParseUntil rbp)
    (PrsPossibleSeq* p delimeter-token delimeter-prec delimeter-prec elm-pred header)
    (p 'Eat ending-token-type)))
;; 0. function as open-paren-nud based on prsmatch-modified.
;; 1. Different from oilshell (i.e. bash) to allow ()=>(tuple).
(define (consume-possible-elems-implicitly-and-the-ending-token p bp ending-token-type delimeter-token delimeter-prec #!optional elm-pred header)
  (cond 
    ((p 'AtToken ending-token-type) (CompositeNode (symbol->token header) (list header)))
    (else
      ;; 0. We can implicitly use LeftComma implied by grammar rule
      ;; Parenthesized form https://docs.python.org/3/reference/expressions.html#parenthesized-forms
      ;; is based on Expression lists https://docs.python.org/3/reference/expressions.html#expression-lists
      ;; 1. see pratt-parsing-demo/tests.py
      ;; x[1,2] implies also considering tuple inherently.
      ;; 2. Here I choose ParseUntil to reuse the above LeftComma for modularity.
      ;; 3. Similar to loop in prsmatch-modified but with
      ;; 3.a. ending-token consumption at the end
      ;; 3.b. element list construction is implicitly done in ParseUntil.
      ;; 3.c. (error 'comma-or-match-not-found (token-read stream)) is implicitly done
      ;; by (p 'Eat ")") but more general to allow possible extension like (a;b;).
      (consume-elems-and-the-ending-token p bp ending-token-type delimeter-token delimeter-prec elm-pred header)
      ))
  )

;; same as LeftBinaryOp in oilshell and parse-infix in pratt_new_compatible_with_MIT_GNU_Scheme.scm
;; except using different data structures.
(define (LeftBinaryOp p token left rbp)
  (CompositeNode-with-binary-expr
    token
    left 
    (p 'ParseUntil rbp))
  )
(define get-binary-left cadr)
(define get-binary-right caddr)
;; Just add Sentinels.
(define (%LeftBinaryOpWithSentinel p token left rbp #!optional right-relative-sentinel left-relative-sentinel)
  (for-each
    (lambda (sentinel) (and* sentinel (assert (relative-sentinel? sentinel))))
    (list right-relative-sentinel left-relative-sentinel)
    )
  ((or* left-relative-sentinel ensure-consistent) token return-last left)
  (let ((right (p 'ParseUntil rbp)))
    ((or* right-relative-sentinel ensure-consistent) token return-last right)
    (CompositeNode-with-binary-expr
      token
      left 
      right)
    )
  )
(define (LeftBinaryOpWithSentinel p token left rbp)
  (%LeftBinaryOpWithSentinel p token left rbp)
  )

;; same as oilshell and parse-prefix in pratt_new_compatible_with_MIT_GNU_Scheme.scm
;; except using different data structures; add Sentinel.
(define (%NullPrefixOp p token rbp #!optional elm-relative-assertion)
  (let ((right (p 'ParseUntil rbp)))
    (use-possible-default-object-proc elm-relative-assertion token return-last right)
    (CompositeNode
      token
      (cons*-wrapper
        (get-header-for-token token)
        (get-GeneralNode-val right))
      )
    )
  )
(define (NullPrefixOp p token rbp)
  (%NullPrefixOp p token rbp)
  )
(define (NullPrefixOpWithSentinel p token rbp)
  (%NullPrefixOp p token rbp ensure-consistent)
  )
(define (NullPrefixOpWithCustomSentinel p token rbp sentinel)
  (assert (relative-sentinel? sentinel))
  (%NullPrefixOp p token rbp sentinel)
  )
