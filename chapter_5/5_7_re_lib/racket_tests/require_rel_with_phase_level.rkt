; #lang racket
;; https://docs.racket-lang.org/guide/Module_Syntax.html#%28part._hash-lang%29

(module name racket
  ; https://stackoverflow.com/a/79543395/21294350
  (begin-for-syntax (current-directory "../racket_lib")) (current-directory) 
  (current-load-relative-directory) ; #f
  (current-module-declare-name) ; #f
  ; (require "tests_lib.rkt")
  )
