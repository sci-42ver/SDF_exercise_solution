; https://stackoverflow.com/a/39417364/21294350
#lang typed/racket
;; only influence this file https://docs.racket-lang.org/guide/Module_Syntax.html#(part._hash-lang)
(provide (all-defined-out))
(define (assert-equal a b)
  (assert (equal? a b)))