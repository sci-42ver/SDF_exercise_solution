(define-syntax define*
  (syntax-rules ()
    ((_ var) (define var (default-object)))
    ((_ var val) (define var val))
    )
  )

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
; (load "DataTypeLib.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/common_lib/")
; (load "common_lib_for_MIT_GNU_Scheme.scm")

(load-option 'format)
;; Here self arg is not needed due to the lexical scope.
;; And that self isn't changed *normally* in Python https://stackoverflow.com/questions/1216356/is-it-safe-to-replace-a-self-object-by-another-object-of-the-same-type-in-a-meth#comment32152243_1216356 https://stackoverflow.com/a/1015602/21294350
;; except for class redefining __new__.
(define (Parser spec lexer)
  (assert (and (ParserSpec? spec) (lexer? lexer)))
  (define* token)
  ;; IMHO better to be like pratt-parsing-demo/tdop.py
  ;; see SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/orig/pratt_new.scm
  ;; for why '($) is not good since it will make (eof-val) usage redundant.
  ; (set! lexer (append lexer `($)))
  ; (set! lexer (append lexer `(,EOF-TOKEN)))
  ;; see 5_7_regexp_lib_simplified_based_on_effbot_based_on_irregex.scm
  ;; here lexer is one proc.

  (define (AtToken token_type)
    (Token-type=? (Token-type token) token_type))
  (define (Next)
    (let ((elm (pop lexer)))
      (set! token elm)
      ; (write-line "finish running Next")
      ; (bkpt 'Next-END)
      )
    )
  (define (Eat type)
    (and (not (AtToken type))
         ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Format.html#index-format
         ;; Here the ending ~ is to avoid put unnecessary newline and spaces inside that string.
         (error (format #f "expected ~S, got ~S" type token)))
    (Next))

  ;; added for PrsNary*
  (define (AtValidNud?)
    (let ((type (Token-type token)))
      (or
        ; (member type Null-Error-List)
        (not 
          (equal? 
            NullError 
            (get-nud (spec 'LookupNull type)))))
      ))
  (define (AtValidLed?)
    (let ((type (Token-type token)))
      (or
        ; (member type Null-Error-List)
        (not 
          (equal? 
            LeftError 
            (get-led (spec 'LookupLeft type)))))
      ))

  ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Top_002dLevel-Definitions.html
  ;; > If variable is not bound, however, define binds variable to a new location in the current environment *before performing the assignment*
  ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Assignments.html#index-set_0021-1
  ;; > If expression is specified, *evaluates* expression and stores the resulting value in the location to which variable is bound.
  (define token-to-manipulate token) ; set to the token val instead of passing ref val.

  (define (ParseWithLeft rbp left)
    (define left-info)
    (while* #t
            (set! token-to-manipulate token)
            (set! left-info (spec 'LookupLeft (Token-type token-to-manipulate)))
            (and
              (>= rbp (get-left-lbp left-info))
              (break)
              )
            (Next)
            (set! left ((get-led left-info) self token-to-manipulate left (get-left-rbp left-info)))
            )
    ;; similar to pratt_new_compatible_with_MIT_GNU_Scheme.scm to move assignment into predicate.
    ;; OK, this style is a bit too weird.
    ;; Anyway the above is imperative while pratt_new_compatible_with_MIT_GNU_Scheme is functional.
    ; (while 
    ;   (< rbp 
    ;     (get-lbp 
    ;       (begin 
    ;         (set! token-to-manipulate token)
    ;         (set! left-info (spec 'LookupLeft (Token-type token-to-manipulate)))
    ;         left-info)))
    ;   (Next)
    ;   ...
    ;   )
    left
    )
  (define (ParseUntil rbp)
    (and
      ;; Normally the end is done by break if (>= rbp (get-left-lbp left-info)).
      ;; So we use one token with -1 prec to mark ending which is not used by other nud/led's to avoid ambiguity.
      (AtToken "eof")
      (error "Unexpected end of input when we needs one nud")
      )
    (set! token-to-manipulate token)
    ; (write-line "to run Next")
    (Next)
    ; (write-line "finish running Next outside")
    (define null-info (spec 'LookupNull (Token-type token-to-manipulate)))
    ; (write-line (list "null-info" null-info))
    (define node ((get-nud null-info) self token-to-manipulate (get-null-bp null-info)))
    (prog1
      (ParseWithLeft rbp node)
      ;; Added by toplevel-parse in pratt_new_compatible_with_MIT_GNU_Scheme.scm
      (and (AtToken "eof") (Eat "eof")))
    )
  (define (Parse)
    (Next)
    (ParseUntil 0)
    )
  (define Parser? (make-bundle-predicate 'Parser))
  (define self (bundle Parser? AtToken Next Eat ParseUntil Parse))
  (trace ParseUntil)
  (trace Next)
  self
  )
