;; full meaning
;; unrelated: https://patterns.eecs.berkeley.edu/?page_id=98 which is about *design* pattern.

(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'term-rewriting)

(load "4_7_lib.scm")
(load "4_9_pletrec_lib_using_pe.scm")

;;; from 4.7
;; > introduce distinct contour lines for scoping
;; > rewriting all the existing matcher interfaces to accept an extra pattern-environment parameter
;; The rest quotes are about the original behavior of the code base instead of what to be implemented here.

;;; from 4.9
;; > namespace scoping and parametric patterns
;; Similar to parametric type in Exercise 4.15, here parametric patterns just means it is composed of some parameter.
;; parametric pattern like (?:pnew (x) ...) has parameter (? x) or (?? x).

;; > A fully worked-out pattern language is a wonderful subsystem to have, but it is not easy to build.
;; > Flesh out this idea to produce a full pattern language.
;; Here I assume "this idea" implies "full". So I won't think about what "full" means *generally* for pattern language.

;;; what to do
;; > ?:pnew creates fresh
;; > lexically scoped pattern variables that can be referred to only in the
;; > body of the ?:pnew.
;; So we can change dict to "pattern-environment" which is just what SICP implements for env in chapter 4 (implicitly namespace scoping).
;; Maybe we can just use dict name and then modify the helper procedures about dict, then minimal modification.

;;; IGNORE: Alternative
;; We can also rewrite the expr like rule in SICP chapter 4, i.e. with make-new-variable.
;; Then ...
;;; The above is wrong since it is not easy to rewrite for ?:ref which may be *outside* ?:pnew.
;; e.g. test3.


;; pattern-environment is a bit different from that in SICP chapter 4 since ?:pnew just denotes x to be new, but not telling about its value.
;; So we should add one flag instead of one name-val binding.
;; 0. Here I won't choose the data structure like (pattern-environment (x) (dict ...)) since x may be added multiple times like in test3 although we will remove that flag when we add the binding, then we can't differentiate among them.
;; In test3, x is not bound in palindrome.
;; 1. I just choose to always adding one new level of binding *assuming to be reserved* (x *unassigned*) like SICP Exercise 4.18.
;; Then they are differentiated by level.
;; And the inner ref will always refer to the newly added binding by pnew.
;; 1.a. test3 can be solved by this.

(define match:pnew-new-vars cadr)
;; Here whether (?:choice ...) or ((?:choice ...)) matters. I assume the former.
;; Also see SDF_exercises/README.md.
(define match:pnew-val caddr)

(load "4_9_pe_lib.scm")
; (load "4_9_stack_lib.scm")

;; 0. to help debug like match:bindings etc
;; 0.a. Here one level may have more than one pnew's. 
;; If we have some debug helpers for tree, then we can differentiate between those in the same level when traversal.
;; I won't dig into that here.
(define *pnew-level* 0)
(define (do-op-on-pnew-level proc)
  (lambda ()
    ;; notice the order if proc is one op without commutative law.
    (set! *pnew-level* (proc *pnew-level* 1))
    )
  )
(define increment-pnew-level (do-op-on-pnew-level n:+))
(define decrement-pnew-level (do-op-on-pnew-level n:-))
(define (reset-pnew-level) (set! *pnew-level* 0))

(define (match:pnew? object)
  (and
    (tagged-list? object '?:pnew)
    ;; based on assumption about lacking begin-analogy ("no something like `begin`") as said in SDF_exercises/README.md.
    (n:= 3 (length object))
    (every symbol? (match:pnew-new-vars object))
    )
  )
(define (match:pnew pattern)
  (let* ((new-vars (match:pnew-new-vars pattern))
         (pe*-proc (match:pnew-add-reserved-init-bindings-to-pe new-vars))
         (body-proc (match:compile-pattern (match:pnew-val pattern)))
         )
    ;; not use something like pe-post-hook-stack since that doesn't show *when* we call the top proc in stack...
    (define (pnew-match data pattern-environment succeed)
      (let ((pe* (pe*-proc pattern-environment)))
        ; (write-line (list "pe*:" pe*))
        (increment-pnew-level)
        ; (write-line (list "pnew-match called with" data pattern-environment "and updates increment-pnew-level to" *pnew-level*))
        (body-proc data pe*
          ;; similar to match:list
          (lambda (new-pe n)
            ;; helper like print-all-matches
            (write-line (list "level" *pnew-level* "constructs pnew-dict" (recent-pnew-dict new-pe) "with new-pe" new-pe))
            (decrement-pnew-level)
            (succeed (pop-recent-pnew-dict new-pe) n)
            )
          ))
      )
    ; (trace pnew-match)
    pnew-match
    ))

(load "4_9_matcher_using_pe.scm")

(load "4_9_tests.scm")
