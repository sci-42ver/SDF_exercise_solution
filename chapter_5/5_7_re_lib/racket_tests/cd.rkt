(module name racket
  ;; TODO why this will influence visit in racket-REPL => (require "cd.rkt").
  ;; https://stackoverflow.com/questions/79549370/when-will-compile-time-expressions-be-reevaluated-in-racket#comment140289393_79549588
  ; (current-directory "/tmp")
  (begin-for-syntax
    (printf "dir = ~a~n" (current-directory)))
  (current-directory "/tmp")
  (displayln "test")
  (printf "phase level 0 dir = ~a~n" (current-directory))

  ; (displayln (find-system-path 'exec-file))
  ; (displayln (find-system-path 'run-file))
  )
;; The above is based on https://stackoverflow.com/a/79543395/21294350.
; (dynamic-require 'name)
