; (define (or* . lst)
;   ;; This has no short circuit.
;   ; (fold
;   ;   (lambda (elm res)
;   ;     (or res
;   ;       (if (default-object? elm)
;   ;         #f
;   ;         elm))
;   ;     )
;   ;   #f
;   ;   lst
;   ;   )
;   )

(define (logic-val val)
  ; (cond 
  ;   ((default-object? val) #f)
  ;   ; ((boolean? val) val)
  ;   (else 
  ;     ; (error (list "logic-val val" val "is unsupported"))
  ;     val
  ;     )
  ;   )
  (and (not (default-object? val)) val)
  )

(define-syntax or*
  (syntax-rules ()
    ;; TODO R7RS is not fully supported?
    ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/R7RS.html
    ;; > P is an underscore (_).
    ; (_ #f)
    ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Conditionals.html#index-or
    ;; ~~TODO why~~ this is not always matched since it is (P_1 … P_n).
    ;; > P is a non-literal identifier;
    ;; > The keyword at the beginning of the pattern in a syntax-rule is not involved in the matching and is not considered a pattern variable or literal identifier. 
    ((_) #f)
    ;; > If all expressions evaluate to false values, the value of the last expression is returned.
    ;; hinted by R7RS
    ((_ arg) (logic-val arg))
    ;; R7RS https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-6.html#TAG:__tex2page_sec_4.3.2
    ;; > Pattern variables that occur in subpatterns followed by one or more instances of the identifier <ellipsis> are allowed only in subtemplates that are followed by as many instances of <ellipsis>.
    ;; so we need arg2 to make arg1 able to be used alone.
    ((_ arg1 arg2 ...)
      ;; 0. short circuit is ensured where ... won't be evaluated if (logic-val arg1) is #t.
      ;; 1. > A subpattern followed by <ellipsis> can match zero or more elements of the input
      (or (logic-val arg1) (or* arg2 ...))
      )
    )
  )

(assert (not (or*)))
(assert (= 2 (or* (default-object) 2)))

(define-syntax define-basic-logic
  (syntax-rules ()
    ((_ op base base-op)
      (define-syntax op
        ;; to avoid the outer syntax-rules capture "...".
        (syntax-rules ellipsis... ()
          ((_*) base)
          ((_* arg) (logic-val arg))
          ((_* arg1 arg2 ellipsis...) (base-op (logic-val arg1) (op arg2 ellipsis...)))
          )
        )
      )
    )
  )
(define-basic-logic and* #t and)
(define-basic-logic or* #f or)

(and* (default-object) 2)
(and* 1 2)

(define (use-possible-default-object-proc possible-default-object-proc . args)
  (and* possible-default-object-proc (apply possible-default-object-proc args)))
