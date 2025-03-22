(define nil '())
(define (writes . lst)
  (write-line lst))
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Input-Procedures.html#index-eof_002dobject
;; > Returns an end-of-file object, not necessarily unique.
(define eof-val eof-object)
(define-syntax prog1
  (syntax-rules ()
    ;; Here the original form is (_ stack) here.
    ((_ elm1 elm2) 
      (let ((tmp elm1))
        elm2
        tmp)
      )
    ;; better general as siod does https://www.reddit.com/r/scheme/comments/4wz7lq/comment/d6b8ifo/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
    ((_ elm1 elm2 ...) 
      (let ((tmp elm1))
        ;; Here let has implicit begin
        elm2
        ...
        tmp)
      )
    ))
(define top-env (the-environment))
(define (symbol-value sym)
  ;; here access will lookup the mere sym... https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Assignments.html#index-access
  ; (access tmp top-env)
  (environment-lookup top-env sym)
  )

;; i.e. implement defsyntax-macro which is like taking the whole syntax object (defsyntax ...).
(define-syntax defsyntax
  (syntax-rules ()
    ;; Here the original form is (_ stack) here.
    ((_ sym arg1 ...)
      ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Quoting.html#index-_002c
      ;; Here (sym ...) is implicitly one list.
      ;; Also see "((a ...) (b ...) var (c ...))" in https://hipster.home.xs4all.nl/lib/scheme/gauche/define-syntax-primer.txt
      (if 
        (memq 
          (quote sym) 
          (quote 
            (QUOTE-SYMBOL COMMA SEMICOLON 
              OPEN-PAREN CLOSE-PAREN
              LEFT-BRACE RIGHT-BRACE)))
        ;; 0. This is used since (quote ' a b) etc can't work.
        ;; Then we use (pl `(,QUOTE ...)).
        ;; 1. See https://stackoverflow.com/q/79522809/21294350
        ;; Here (unquote if) is one syntactic error. 
        ;; I don't how to implement one thing like (_ (QUOTE-SYMBOL COMMA SEMICOLON) arg1 ...)
        ;; which will match QUOTE-SYMBOL or COMMA or SEMICOLON.
        ;; One way is just like here to not use if.
        ;; Or a bit routine work by writing the similar template for each inside QUOTE-SYMBOL, COMMA and SEMICOLON etc.
        (*defsyntax (quasiquote ((unquote sym) arg1 ...)))
        (*defsyntax (quote (sym arg1 ...))))
      )
    ))

; https://stackoverflow.com/a/42487783/21294350
(define-syntax while
  (syntax-rules ()
    ;; Here the original form is (_ stack) here.
    ((_ pred body ...) 
      (do () 
        ((not pred) (begin)) ; implicit "Unspecified return value"
        body ...
        )
      )
    ))

(define (href hash-table key)
  (hash-table-ref/default hash-table key #f))
(define (hset hash-table key datum) 
  (hash-table-set! hash-table key datum)
  datum)
(define empty-hash-table (make-strong-eq-hash-table 0))

;; Do replacement for SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/pratt_new_compatible_with_MIT_GNU_Scheme.scm:
;; Bash $ sed -i -f compatible.sed pratt_new_compatible_with_MIT_GNU_Scheme.scm
