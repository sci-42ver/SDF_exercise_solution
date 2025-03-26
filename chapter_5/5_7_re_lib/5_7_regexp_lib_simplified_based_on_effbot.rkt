#lang racket
;; I use Racket here since MIT/GNU Scheme doesn't support ?<! (see SDF_exercises/chapter_5/5_7_common_lib/re/general_re_lib.scm)
;; Although we can do manual-defined ?<! first and then the following groups matching,
;; that is so inconvenient and less modular. (Anyway we can define ?<! pat, but that failed. See SDF_exercises/chapter_5/re_lib/regexp.scm)

(define simplified-pat0
  ;; 0. Not put unnecessary spaces inside (different from Python).
  ;; 1. Notice the order where the larger one is first.
  #px"\\s*(?:(\\w+)| ((?<!\\w)(?:\\*|\\*\\*)\\w+) |([\\(\\)\\[\\]]))"
  )
(define simplified-pat
  ;; Use ([^\\])(\\[^\\])=>$1\\$2 to do replacement from Python raw string.
  ;; 0. Not put unnecessary spaces inside.
  ;; 1. Notice the order where the larger one is first.
  #px"\\s*(?:(\\w+)|((?<!\\w)(?:\\*|\\*\\*)\\w+)|([\\-\\+\\*/%!~<>=&^|?:]+)|([\\(\\)\\[\\]~^!?:,]))"
  )
(define test-book-exp
  "fact := lambda n, a,**kwargs,*args:
  if n == 0
  then 1
  else n*fact(n-1)"
  )
;; https://stackoverflow.com/a/38712295/21294350
;; cdr corresponds to findall with group. Default car corresponds to findall without group.
(regexp-match* simplified-pat0 test-book-exp #:match-select cdr)
(newline)

(current-directory)
(current-directory
  (expand-user-path "~/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/racket_lib")
  )
(current-directory)
;; https://stackoverflow.com/q/79535844/21294350
; (require "tests_lib.rkt")
;; https://stackoverflow.com/a/6380648/21294350
; implied in `racket -h`.
(define dir-str "~/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/racket_lib/")

;; no use
; (require (file (string-append dir-str "tests_lib.rkt")))
; (define (require-file dir file)
;   (let ((abs-file (string-append dir file)))
;     (require (file abs-file)))
;   )
; (require-file dir-str "tests_lib.rkt")

(require (file "~/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/racket_lib/tests_lib.rkt"))
; (require "racket_lib/tests_lib.rkt")
(assert-equal
  (regexp-match* simplified-pat test-book-exp #:match-select cdr)
  '(("fact" #f #f #f)
    (#f #f ":=" #f)
    ("lambda" #f #f #f)
    ("n" #f #f #f)
    (#f #f #f ",")
    ("a" #f #f #f)
    (#f #f #f ",")
    (#f "**kwargs" #f #f)
    (#f #f #f ",")
    (#f "*args" #f #f)
    (#f #f ":" #f)
    ("if" #f #f #f)
    ("n" #f #f #f)
    (#f #f "==" #f)
    ("0" #f #f #f)
    ("then" #f #f #f)
    ("1" #f #f #f)
    ("else" #f #f #f)
    ("n" #f #f #f)
    (#f #f "*" #f)
    ("fact" #f #f #f)
    (#f #f #f "(")
    ("n" #f #f #f)
    (#f #f "-" #f)
    ("1" #f #f #f)
    (#f #f #f ")"))
  )
