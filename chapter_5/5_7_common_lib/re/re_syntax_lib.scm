;; https://srfi.schemers.org/srfi-115/srfi-115.html#Types-and-Naming-Conventions
;; > re: an SRE or pre-compiled regexp object
;; Here I just use SRE for simplicity without using (regexp re) to compile.
(define (sre-lst? sre-lst)
  (and 
    (list? sre-lst) 
    (every
      ;; See SDF_exercises/chapter_5/5_7_regexp_lib.scm: consider get-args-kwargs
      (lambda (elm) (or (procedure? elm) (valid-sre? elm)))
      sre-lst)))
(define (make-or sre-lst)
  (assert (sre-lst? sre-lst))
  (cons 'or sre-lst)
  )
