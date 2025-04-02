#lang racket
(provide foo bar)
(define (foo) (displayln "called foo from module-two"))
(define (bar) (displayln "called bar from module-two"))