(module compile-time-number racket/base
  (require (for-syntax racket/base))
  ;; TODO https://stackoverflow.com/questions/79549370/when-will-compile-time-expressions-be-reevaluated-in-racket#comment140289393_79549588
  (current-directory "/tmp")
  (begin-for-syntax                          
    (printf "dir = ~a~n" (current-directory)))
  (begin-for-syntax (printf "picked ~a\n" (random)))
  (printf "running\n"))

; https://docs.racket-lang.org/guide/macro-module.html#(part._.Visiting_.Modules)
  
; https://docs.racket-lang.org/reference/module.html#%28form._%28%28quote._~23~25kernel%29._module%29%29
;; For the above "(require (for-syntax racket/base))", the following is not enough to be used by begin-for-syntax.
; (require racket/base)
