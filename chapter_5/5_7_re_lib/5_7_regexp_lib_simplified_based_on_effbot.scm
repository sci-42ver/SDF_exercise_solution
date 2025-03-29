;; Skip this due to MIT/GNU Scheme lacking ?:< etc.

; https://srfi.schemers.org/srfi-115/srfi-115.html#proc-regexp-match-submatch
(define (group pat)
  (assert (valid-sre? pat))
  `($ ,pat)
  )
; (?: (\d+) | (\w+) | ( [\-\+\*/%!~<>=&^|?:,]+ ) | ([\(\)\[\]]) )
; (define pat (regexp `(: (* space) (w/nocapture ,(group pat)) )))

; (regexp '(: (* space) numeric))
(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "5_7_common_lib/re/general_re_lib.scm")
(define pat 
  (regexp 
    `(: 
      (* space) 
      (or
        ;; \w already has \d.
        ($ word-corrected)
        ;; default to be greedy, so ** will be matched instead of 2 *s.
        ($ (+ (or "-" "+" "*" "/" "<" ">" "=" "?" ":" ","))) 
        ($ (or "(" ")"))))))

(regexp-match-submatch (regexp-search pat "k*c+a*b") 1)
(define str-with-spaces 
  "fact := lambda n:
    if n == 0
    then 1
    else n*fact(n-1)")
(define match1 (regexp-search pat str-with-spaces))
; (define (regexp-match-count* )
;   body)
(regexp-match-count match1)
(for-each
  (lambda (idx)
    (write-line (regexp-match-submatch match1 idx))
    )
  (iota 6)
  )

;; compatible API as Python

