(define y 3)
(define proc1 (lambda (x) (* x y)))

(define y 4)
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Top_002dLevel-Definitions.html
;; > has essentially the same effect as this *assignment* expression, if variable is bound:
(proc1 3)
