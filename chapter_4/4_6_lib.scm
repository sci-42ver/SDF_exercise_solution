;; just similar to or and also lp in match:segment.
;; see https://web.archive.org/web/20220605022418/http://community.schemewiki.org/?sicp-ex-4.4 dummy's.

;; I follow match:segment since that idea is more elegant.
(define (match:choice pattern)
  (let* ((choices (cdr pattern))
         (matchers (map match:compile-pattern choices))
         (n (length choices))
         )
    (define (choices-match data dictionary succeed)
      (and (list? data) ; can match null list.
           (let lp ((i 0) (matchers matchers))
             ;; > returning the first successful match, or #f if none match.
             (and (< i n)
                  ;; similar to match:list
                  (or ((car matchers)
                       data
                       dictionary
                       (lambda (new-dictionary n)
                         ;; IGNORE SDF_exercises TODO when happens
                         (if (> n (length data))
                           (error "Matcher ate too much."
                                  n))
                        ;  (write-line (list "choices-match returns" new-dictionary n))
                         (succeed new-dictionary n)
                         ))
                      (lp (+ i 1) (cdr matchers)))))
           )
      )
    ; (trace choices-match)
    choices-match
    )
  )

;; syntax
(define (match:choice? object)
  (and (pair? object)
       (eq? '?:choice (car object))
       ))

(define (match:compile-pattern pattern)
  (cond ((match:var? pattern)
         (case (match:var-type pattern)
           ((?) (match:element pattern))
           ((??) (match:segment pattern))
           (else (error "Unknown var type:" pattern))))
        ;; added before general list
        ((match:choice? pattern)
         (match:choice pattern)
         )
        ((list? pattern)
         (match:list (map match:compile-pattern pattern)))
        (else
          (match:eqv pattern))))
