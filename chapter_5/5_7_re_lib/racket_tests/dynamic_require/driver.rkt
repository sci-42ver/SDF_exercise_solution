#lang racket

; https://stackoverflow.com/a/64447199/21294350
(define ((make-driver choice) method-name)
  (case choice
    [(1) ((dynamic-require "module-one.rkt" method-name))]
    [(2) ((dynamic-require "module-two.rkt" method-name))]
    ;; those will fail
    ; [(1) ((require "module-one.rkt"))]
    ; [(2) ((require "module-two.rkt"))]
    [else (void)]))

(define a-driver (make-driver 1))
(a-driver 'foo)
(a-driver 'bar)

(define b-driver (make-driver 2))
(b-driver 'foo)
(b-driver 'bar)