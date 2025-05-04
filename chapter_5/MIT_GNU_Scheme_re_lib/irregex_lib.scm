(cd "~/SICP_SDF/SDF_exercises/chapter_5/MIT_GNU_Scheme_re_lib/")
(define old-every every)
(load "irregex/irregex.scm")
;; Just use MIT/GNU Scheme every which is more general. 
(define every old-every)

(cd "~/SICP_SDF/SDF_exercises/common-lib/")
(load "re_lib.scm")

(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "logic_lib.scm")
(define (regexp-fold re kons knil str #!optional finish start end) 
  (irregex-fold
    re
    (lambda (i m s) (kons i m str s))
    knil str
    (or* (and finish (lambda (i s) (finish i #f str s))) (lambda (i s) s))
    (or* start 0)
    (or* end (string-length str))
    ))

;; TODO closure, rewrite, syntax-rules all can't do modular rewrite from regexp-foo to irregex-foo.
; (define regexp-match->list)

(define regexp-match? irregex-match-data?)
(define regexp-match-submatch irregex-match-substring)
(define regexp-match-submatch-start irregex-match-start-index)
