(define-syntax define*
  (syntax-rules ()
    ((_ var) (define var (default-object)))
    ((_ var val) (define var val))
    )
  )

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "DataTypeLib.scm")

(define EOF-TOKEN (Token "eof" "eof"))

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/common_lib/")
(load "common_lib_for_MIT_GNU_Scheme.scm")

(load-option 'format)
(define (Parser spec lexer)
  (define* token)
  ;; IMHO better to be like SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/python_demo/pratt-parsing-demo/tdop.py
  ;; see SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/orig/pratt_new.scm
  ;; for why '($) is not good since it will make (eof-val) usage redundant.
  ; (set! lexer (append lexer `($)))
  (set! lexer (append lexer `(,EOF-TOKEN)))
  (define (AtToken token_type)
    (Token-type=? (Token-type token) token_type))
  (define (Next)
    (set! token (pop lexer))
    )
  (define (Eat type)
    (and
      (not (AtToken type))
      ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Format.html#index-format
      ;; Here the ending ~ is to avoid put unnecessary newline and spaces inside that string.
      (error (format #f "expected ~S, ~
                     got ~S" type token))
                     )
    (Next)
    )
      (define (ParseUntil rbp)
        (and
          (AtToken "eof")
          (error "Unexpected end of input")
          )
        ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Top_002dLevel-Definitions.html
        ;; > If variable is not bound, however, define binds variable to a new location in the current environment *before performing the assignment*
        ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Assignments.html#index-set_0021-1
        ;; > If expression is specified, *evaluates* expression and stores the resulting value in the location to which variable is bound.
        (define cur-token token) ; set to the token val instead of passing ref val.
        (Next)
        (define null-info (spec 'LookupNull (Token-type cur-token)))
        (define node ((get-nud null-info) self t (get-null-bp null-info)))
        (define left-info)
        (while* #t
                (set! cur-token token)
                (set! left-info (spec 'LookupLeft (Token-type cur-token)))
                (and
                  (>= rbp (get-left-lbp left-info))
                  (break)
                  )
                (Next)
                (set! node ((get-led left-info) self t node (get-left-rbp null-info)))
                )
        ;; similar to SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/pratt_new_compatible_with_MIT_GNU_Scheme.scm to move assignment into predicate.
        ;; OK, this style is a bit too weird.
        ;; Anyway the above is imperative while pratt_new_compatible_with_MIT_GNU_Scheme is functional.
        ; (while 
        ;   (< rbp 
        ;     (get-lbp 
        ;       (begin 
        ;         (set! cur-token token)
        ;         (set! left-info (spec 'LookupLeft (Token-type cur-token)))
        ;         left-info)))
        ;   (Next)
        ;   ...
        ;   )
        node
        )
      (define (Parse)
        (Next)
        (ParseUntil 0)
        )
      (define self (bundle Parser? AtToken Next Eat ParseUntil Parse))
      (define Parser? (make-bundle-predicate 'Parser))
      self
      )
