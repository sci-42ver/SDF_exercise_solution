(define-syntax pop
  ;; 0. ellipsis usage https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-6.html#TAG:__tex2page_sec_4.3.2
  ;; > P is of the form (P1 … Pk Pe <ellipsis> Pm+1 … Pn) ...
  ;; 1. No need for literal since pop doesn't use any special characters like =>/else in cond etc.
  (syntax-rules ()
    ;; Here the original form is (_ stack) here.
    ((_ stack) 
      ;; prognify is implicitly done in template.
      (let ((tmp stack))
        ;; IMHO this is also ok.
        ; (set! stack (cdr stack))
        (set! stack (cdr tmp))
        (car tmp)
        )
      )))