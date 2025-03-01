(define test (list 1))
(set-cdr! test test)
;; see https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Output-Procedures.html#index-fresh_002dline
;; write can handle "cycles".
(write-line test)
; (display test)
