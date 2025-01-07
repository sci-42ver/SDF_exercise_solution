;;; Primitive match procedures:

;; not needed for changes: 
;; 0. match:eqv

; (define (succeed-adding-var-binding-in-base-dict succeed)
;   body)

; (trace match:pe-lookup)
(define (match:element variable)
  (define (element-match data pattern-environment succeed)
    ; (write-line "call new element-match")
    (and (pair? data)
         (match:satisfies-restriction? variable (car data))
         (let ((binding (match:pe-lookup variable pattern-environment)))
           (if binding
               (if (reserved-init-binding? binding)
                (begin
                  (update-binding! binding variable (car data))
                  ; (write-line (list "update binding to" binding))
                  ;; always succeed at last.
                  (succeed pattern-environment 1)
                  )
                (and (equal? (match:binding-value binding)
                            (car data))
                    (succeed pattern-environment 1)))
               ;; We always add non-pnew-binding to the base dict. So that can be accessed anywhere.
               (succeed (pe-add-binding-to-base-dict! pattern-environment variable (car data)) 1)))))
  ; (trace element-match)
  element-match)

;; match:element-no-restriction is not used, so skipped.

(define (match:segment variable)
  (define (try-add-binding data succeed variable pattern-environment #!optional binding)
    (let ((n (length data)))
      (let lp ((i 0))
        (and (<= i n)
            (or (let ((val-to-bind (list-head data i)))
                  (if (default-object? binding)
                    ;; since "called by non-pnew", no restoration is needed.
                    (succeed 
                      (pe-add-binding-to-base-dict! 
                        pattern-environment 
                        variable 
                        val-to-bind) 
                      i)
                    (begin
                      ; (write-line (list "match:segment update binding" binding "to" (list variable val-to-bind)))
                      ;; restoration is done in update-binding! when necessary.
                      (update-binding! binding variable val-to-bind)
                      ;; always succeed at last.
                      (succeed pattern-environment i)
                      )))
                (lp (+ i 1)))))))
  (define (segment-match data pattern-environment succeed)
    (and (list? data) ; can match null list.
         (let ((binding (match:pe-lookup variable pattern-environment)))
           (if binding
               ;; similar to match:element
               (if (reserved-init-binding? binding)
                ;; called by pnew
                (try-add-binding data succeed variable pattern-environment binding)
                (match:segment-equal? data 
                                     (match:binding-value binding)
                                     (lambda (n)
                                       (succeed pattern-environment n))) )
               ;; called by non-pnew
               (try-add-binding data succeed variable pattern-environment)
               ))))
  ; (trace segment-match)
  segment-match)

;; match:segment-equal? same since it just compares segment *values* without caring about dict.

;; since match:list is just one wrapper to run matcher list, so no modification.

;;;; Pattern syntax
;; These are just wrapper outside matcher, so similar to match:list no modification mostly.

;; matcher same

(define (run-matcher match-procedure datum succeed)
  ; (write-line (list "use new run-matcher" (match:pe-with-base-dict)))
  (reset-pnew-level)
  (match-procedure (list datum)
                   ;; modified
                   (match:pe-with-base-dict)
                   (lambda (pe-with-base-dict n) 
                     (and (= n 1)
                          (match:pe-with-only-base-dict? pe-with-base-dict)
                          (succeed pe-with-base-dict)))))

;; print-all-matches same

;; based on 4.7
(define (match:compile-pattern pattern)
  (cond ((match:ref? pattern)
         ;; added before match:var? to avoid overload
         (match:ref pattern))
        ((match:var? pattern)
         (case (match:var-type pattern)
           ((?) (match:element pattern))
           ((??) (match:segment pattern))
           (else (error "Unknown var type:" pattern))))
        ((match:choice? pattern)
         (match:choice pattern))
        ;; added before general list
        ((match:pletrec? pattern)
         (match:pletrec pattern))
        ((match:pnew? pattern)
         (match:pnew pattern))
        ((list? pattern)
         (match:list (map match:compile-pattern pattern)))
        (else
          (match:eqv pattern))))


;;; Nice pattern inspection procedure that will be used by the
;;; pattern-directed invocation system.

(define (match:pattern-names pattern)
  (reverse
   (let loop ((pattern pattern) (names '()))
     (cond ((match:var? pattern)
            (let ((name (match:var-name pattern)))
              (if (memv name names)
                  names
                  (cons name names))))
           ((list? pattern)
            (let elt-loop ((elts pattern) (names names))
              (if (pair? elts)
                  (elt-loop (cdr elts)
                            (loop (car elts) names))
                  names)))
           (else names)))))