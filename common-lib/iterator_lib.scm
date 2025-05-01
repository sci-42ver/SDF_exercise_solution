(cd "~/SICP_SDF/lecs/6.001_fall_2007_recitation/codes/rec20/coroutine/")
;; offer iterator implementation
(load "demo-implementation.scm")

;; This is implicitly implied in Python "for" compound statement.
(define (for-each-elm-in-iterator proc iterator)
  ;; better to add one predicate for iterator (see lecs/6.001_fall_2007_recitation/codes/rec20/coroutine/demo-implementation.scm)
  (assert (procedure? iterator))
  (let lp ()
    (if (iterator 'alive?)
      (begin
        (let ((elm (iterator 'next)))
          (and (iterator 'alive?) (proc elm)))
        (lp)
        )
      'finished)
    )
  )

(cd "~/SICP_SDF/SDF_exercises/common-lib/")
(load "re_lib.scm")
; (for-each-elm-in-iterator 
;   (lambda (elm) (write-line `(iter ,elm))) 
;   (regexp-finditer 'numeric "123"))
;; res
; (iter #(*irregex-match-tag* #(#[compound-procedure 12] #[compiled-procedure 13 ("list" #x1) #x1c #xe2d824] #[compiled-procedure 14 ("list" #x2a) #x1c #xe31c94] #[compiled-procedure 15 ("list" #x30) #x1c #xe3283c] #[compound-procedure 16] #f) () ("123" 0 3) 0 ("123" 0 3) 1 #f #f #f #f))
;; ...

(lambda (ignore) (begin))
(define-syntax coroutine*
  ;; > An identifier that appears in the pattern of a syntax-rule is a pattern-variable, unless it is the keyword that begins the pattern, is listed in literals
  ;; > A subform in the input matches a literal identifier if and only if it is an identifier ... the two identifiers are equal and both have no lexical binding.
  ;; So yield is literal here.
  ;; > If a macro transformer inserts a binding for an identifier (variable or keyword), the identifier will in effect be renamed throughout its scope to avoid conflicts with other identifiers. 
  ;; > If a macro transformer inserts a free reference to an identifier, the reference refers to the binding that was visible where the transformer was specified
  ;; literal won't introduce binding. So just use the "free reference".
  ;; > pattern variables that occur in the template are replaced by the subforms they match in the input.
  ;; So here yield won't be rewritten by macro hygiene.
  (syntax-rules (yield)
    ((_ stmt1 ...)
      ;; > Identifiers that appear in the template but are not pattern variables or the identifier ‘...’ are inserted into the output as literal identifiers.
      ;; This yield is literal
      (coroutine (lambda (yield)
        ;; wrap with begin to allow the empty body.
        (begin stmt1 ...)
        ))
      )
    )
  )

(define iterator-wrapped
  (coroutine*
    ;; yield state should be based on the whole for loop.
    (for-each-elm-in-iterator 
      (lambda (elm) 
        (yield `(iter ,elm))
        ) 
      (regexp-finditer 'numeric "123"))
    ;; Here we will implicitly return FINISH-MARK caused by coroutine.
    )
  )

; (iterator-wrapped 'next)
; (iterator-wrapped 'next)
; (iterator-wrapped 'next)
; ;Value: (iter #[regexp-match 19 "3"])
; (iterator-wrapped 'next)
; ;Value: (iter finish-routine)
; (iterator-wrapped 'next)
; ;Value: finish-routine
; (iterator-wrapped 'next)
; ;Value: coroutine-iteration-finished
; (iterator-wrapped 'next)
; ;Value: coroutine-iteration-finished
