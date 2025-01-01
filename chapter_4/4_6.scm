(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'term-rewriting)

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
                         ;; SDF_exercises TODO when happens
                         (if (> n (length data))
                           (error "Matcher ate too much."
                                  n))
                         (succeed new-dictionary n)
                         ))
                      (lp (+ i 1) (cdr matchers)))))
           )
      )
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

;; results just as the book shows.
;; They arer also shown in SICP_SDF/SDF_exercises/software/sdf/design-of-the-matcher/text-examples.scm
(run-matcher
  (match:compile-pattern '(?:choice a b (? x) c))
  'z
  match:bindings)

(run-matcher
  (match:compile-pattern
    `((? y) (?:choice a b (? x ,string?) (? y ,symbol?) c)))
  '(z z)
  match:bindings)

(run-matcher
  (match:compile-pattern `(?:choice b (? x ,symbol?)))
  'b
  print-all-matches)
