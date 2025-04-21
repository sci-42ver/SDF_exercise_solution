;; So you can see Pratt may be limited for one actual programming language parser although with the powerful nud/led.

;;;;;;;; Only used in lambda (so not put this in led/nud)
;; This can be implemented in lambda nud, but that will make that procedure much complexer if with other ones.

;;;; BEHAVIOR
;; see ensure-tuple comment.
;; not in pratt_new_compatible_with_MIT_GNU_Scheme.scm and oilshell
;;;; TODO tests
;; 
;;;; IGNORE This is unpacking variadic arguments https://docs.python.org/3/tutorial/controlflow.html#arbitrary-argument-lists https://stackoverflow.com/a/36908/21294350
; (define (NullMul p token rbp)
;   (let ((right (p 'ParseUntil rbp)))
;     (ensure-tuple right)
;     (new-GeneralNode-simplified
;       (cons*-wrapper
;         "null-*"
;         (get-GeneralNode-val right))
;       token
;       "null-*"
;       )
;     )
;   )

;;;; BEHAVIOR
;; Here I only consider
;; > parameter                 ::= identifier [":" expression]
;; > star_parameter            ::= identifier [":" ["*"] expression]
;; for lambda implementation.
;;;; TODO tests
;; := ones
;; and a:*
; (define (LeftColon p token left unused-rbp)
;   (ensure-identifier left)
;   (LeftBinaryOp p token left EXPR-BASE-PREC)
;   )
;;;;;;;; finished
