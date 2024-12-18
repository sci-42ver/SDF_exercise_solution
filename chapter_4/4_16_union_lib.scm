(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "common_lib.scm")
(define (union-term? term)
  (and
    (tagged-list? term '?:union)
    (n:> (length term) 1)
    ))
(define union-types cdr)
;; https://stackoverflow.com/a/8382387/21294350
(define (dedupe e eq-check?)
  (if (null? e) '()
      (cons (car e) (dedupe (filter (lambda (x) (not (eq-check? x (car e)))) 
                                    (cdr e)) eq-check?))))

(define (union-term . bases)
  (cons '?:union 
    (dedupe
      (append-map 
        (lambda (base) 
          (if (union-term? base)
            (union-types base)
            (list base))
          ) 
        bases)
      eq?
      ))
  )

(define (unify:left-with-union-term union-term-first terms2)
  (let ((first1 (car union-term-first)) (rest1 (cdr union-term-first))
        (first2 (car terms2)) (rest2 (cdr terms2)))
    (define (unify-constants dict succeed fail)
      (if (memv first2 (union-types first1))
          (succeed dict fail rest1 rest2)
          (begin
            (write-line (list "left-with-union-term error for" first1 first2))
            (fail))))
    unify-constants))
;; See SDF_exercises/chapter_4/4_15.scm
;; > generic get-handler will get the latter added procedure if possible.
(define-generic-procedure-handler unify:gdispatch
  (match-args (car-satisfies union-term?)
              (car-satisfies list-term?))
  unify:left-with-union-term)

;;; procedure definition tag


