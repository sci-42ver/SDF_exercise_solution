#lang racket
(provide foo bar)
(define (foo) (displayln "called foo from module-one"))
(define (bar) (displayln "called bar from module-one"))