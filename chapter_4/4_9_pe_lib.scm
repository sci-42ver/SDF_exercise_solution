;; different from exercise_codes/SICP/book-codes/ch4-mceval.scm
;; Here I always uses the tagged list, anyway this doesn't have much influence on the complexity (but may help in-place modification).
(define (match:pe-with-base-dict)
  (cons 'pattern-environment (list (match:new-dict))))
(define (match:pe? pe)
  (tagged-list? pe 'pattern-environment))
(define (match:pe-with-only-base-dict? pe)
  (and
    (match:pe? pe)
    (n:= 1 (length (match:pe-dicts pe)))
    ))

(define (match:new-pe dicts)
  (cons 'pattern-environment dicts))
(define match:pe-dicts cdr)
;; the new added is always prioritized.
(define (add-dict-to-pe dict pe)
  (match:new-pe (cons dict (match:pe-dicts pe)))
  )

;; This can help match:lookup-corrected
(define (make-?/??-var var-name)
  (list '?/?? var-name))
(define match:var-types (cons '?/?? match:var-types))

(define reserved-unassigned-symbol '*unassigned*)
(define (make-reserved-init-binding var)
  (match:make-binding (make-?/??-var var) reserved-unassigned-symbol))

;; This should be dropped when outside the pnew scope to avoid mess up with other pnew-dict's.
;; This is similar to the stack behavior.
(define (match:pnew-bindings dict bindings)
  (cons 'pnew-dict bindings))
;; pe means pattern-environment
(define (match:pnew-add-reserved-init-bindings-to-pe vars)
  ;; wrong
  (let* ((reserved-init-bindings (map make-reserved-init-binding vars))
         (new-level-dict (match:pnew-bindings 'ignore reserved-init-bindings)))
    (lambda (pe)
      (write-line (list "new-level-dict" new-level-dict))
      (add-dict-to-pe new-level-dict pe)
      )
    )
  )
(define (match:pnew-add-reserved-init-bindings-to-pe vars)
  (lambda (pe)
    ;; we must construct new bindings here since set! may be done on this bindings later.
    (add-dict-to-pe (match:pnew-bindings 'ignore (map make-reserved-init-binding vars)) pe)
    )
  )

(define (empty-pe? pe)
  (null? (cdr pe)))
(define (recent-pnew-dict pe)
  (if (empty-pe? pe)
    (error "pe is empty without recent-pnew-dict")
    (cadr pe)
    ))
(define (pop-recent-pnew-dict pe)
  (if (empty-pe? pe)
    (error "pe is empty without recent-pnew-dict")
    (match:new-pe (cdr (match:pe-dicts pe)))
    ))

;;; used in SDF_exercises/chapter_4/4_9_matcher_using_pe.scm
;; similar to lookup-variable-value in SICP exercise_codes/SICP/book-codes/ch4-mceval.scm
(define first-dict car)
(define rest-dicts cdr)
(define (match:pe-lookup var pe)
  (define (dicts-loop dicts)
    (define (scan dict)
      (let ((binding (match:lookup var dict)))
        (if binding
          binding
          (dicts-loop (rest-dicts dicts)))
        ))
    ;; same as match:lookup to return #f if failure.
    (and (not (null? dicts))
        (let ((dict (first-dict dicts)))
          (scan dict))))
  (dicts-loop (match:pe-dicts pe)))
;; maybe better to use let loop. Anyway the basic ideas are same.
(define (match:pe-lookup var pe)
  (let lp ((dicts (match:pe-dicts pe)))
     (and (not (null? dicts))
        (let ((dict (first-dict dicts)))
          (let ((binding (match:lookup var dict)))
            (if binding
              binding
              (lp (rest-dicts dicts)))
            )
          )
        )
    )
  )

(define (reserved-init-binding? binding)
  (eq? (match:binding-value binding) reserved-unassigned-symbol))
(define (set-binding-val! binding val)
  (set-car! (cdr binding) val))
(define (set-binding-type! binding type)
  (set-car! (cddr binding) type))
(define (update-binding! binding var val)
  (set-binding-val! binding val)
  (let ((type (match:binding-type binding)))
    (if (eq? '?/?? type)
      (set-binding-type! binding (match:var-type var))
      (if (eq? '?? type)
        (begin
          ; (write-line (list "segment tries new binding" (list var val)))
          ;; this cancels the before binding, so we need to restore something if necessary.
          (decrement-pnew-level)
          )
        (error "can't modify binding for non-pnew-init"))
      ))
  )

(define base-dict-pair last-pair)
(define base-dict last)
(define (pe-set-base-dict! pe dict)
  (set-car! (base-dict-pair pe) dict)
  pe
  )
(define (pe-add-binding-to-base-dict! pe var val)
  (let* ((base-pair (base-dict-pair pe))
         (base (car base-pair)))
    (set-car! 
      base-pair
      (match:extend-dict var val base)
      )
    pe
    )
  )
