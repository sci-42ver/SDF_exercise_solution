;;; from 4_7_lib.scm

;;; Actual implementation
; (load "common_lib.scm")
;; similar to match:list, we don't need to change match:choice.
; (load "4_6_lib.scm")

;; match:compile-pattern will be overloaded later, so not define that here.

;; syntax procedures for match:ref & match:pletrec don't need changes.

(define (match:ref variable)
  (define (ref-match data pattern-environment succeed)
    (and (list? data)
         ;; similar to match:element changes in SDF_exercises/chapter_4/4_9_matcher_using_pe.scm
         (let ((binding (match:pe-lookup variable pattern-environment)))
           (if (and binding (not (reserved-init-binding? binding)))
              (begin
                ; (write-line (list "call ref-match of" variable "for data & dict" data pattern-environment))
                ;; ref is just one medium to call its value. So not change data, e.g. using (car data).
                ((match:binding-value binding) data pattern-environment succeed))
              (error (list "ref undefined for:" variable))
              ))
         ))
  ref-match)

;; no modification for dictionary helpers like match:extend-dict*.

(define (match:pletrec-add-bindings-to-base-dict bindings)
  (let ((bindings-with-proc-vals 
          (map 
            (lambda (elm) 
              (match:make-binding 
                (make-ref (match:pletrec-binding-name elm))
                (match:compile-pattern (match:pletrec-binding-val-pattern elm))
                )) 
            bindings)))
    ;; modified
    (lambda (pe)
      (pe-set-base-dict! pe (match:extend-dict* bindings-with-proc-vals (base-dict pe)))
      ))
  )

;; Here we do all match:compile-pattern outside the created matcher procedure as Exercise 4.13 says.
(define (match:pletrec pattern)
  (let* ((bindings (match:pletrec-bindings pattern))
         (pe*-proc (match:pletrec-add-bindings-to-base-dict bindings))
         (body-proc (match:compile-pattern (match:pletrec-body pattern)))
         )
    (define (pletrec-match data pattern-environment succeed)
      ;; (and (pair? data) ...) etc will be checked in body-proc
      (let ((pe* (pe*-proc pattern-environment)))
        ; (write-line (list "match:pletrec call with pe" pe*))
        (body-proc data pe* succeed))
      )
    pletrec-match
    ))
