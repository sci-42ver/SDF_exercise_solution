(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'unification)

;; > This could be fun,
;; > but you need to invent a syntactic mechanism for representing
;; > string variables inside a string. This is pretty delicate, because you
;; > may have to represent a string variable with a string expression.
;; 0. delicate  means subtle.
;; 1. Python uses {} like {num} to mean possible var *implicitly* https://stackoverflow.com/a/52155770/21294350.
;; https://stackoverflow.com/a/2962966/21294350 also shows % similarly like %(num).
;; So similarly we can use (?/?? ...) to *implicitly* denote that var.
;;;; > This gets into quotation problems
;;; TODO: I don't what the author means by "quotation problem". 
;; maybe this https://courses.grainger.illinois.edu/cs421/sp2023/exams/Sample-Midterm3-Sol-sp23.pdf
;; > So """" represents a string that has one character which is a double quotes.
;; In Scheme we use \" https://edoras.sdsu.edu/doc/mit-scheme-9.2/mit-scheme-ref/Strings.html
;;; IMHO just use string comparison with string.
;; > please try not to invent a baroque mechanism.
;; i.e. not too complex https://www.reddit.com/r/compsci/comments/1mli7h/it_is_wellknown_that_the_x86_instruction_set_is/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button.
;; the similar meaning as "*contrast*, movement, *exuberant* detail," https://en.wikipedia.org/wiki/Baroque\

;; IMHO string is just list of characters. So we can reuse the original implementation.
;; So element is character.
(regexp-partition '(: "(" (or "?" "??") space (+ alphanumeric) ")") "(?? seg1)(? seg1)")
;; only the 1st
(regexp-match->list
 (regexp-search '(: "(" (or "?" "??") space (+ alphanumeric) ")") "(?? seg1)(? seg2)"))
(regexp-extract '(: "(" (or "?" "??") space (+ alphanumeric) ")") "(?? seg1)(? seg2)")
(define (var-str->var var-str)
  ; (regexp-extract '(and (~ (or space "(" ")")) (or "?" "??" (+ alphanumeric))) "(?? seg1)")
  (let ((split (regexp-split '(or space "(" ")") var-str)))
    (assert (n:= 4 (length split)))
    (let ((res (map string->symbol (list (second split) (third split)))))
      (assert (match:var? res))
      res)
    )
  )
(var-str->var "(?? seg1)")
(define (string->str-list str)
  ; https://stackoverflow.com/a/29766822/21294350
  ;; not use char->name
  (map string (string->list str)))

;; keep char here to avoid unnecessary match with (car-satisfies string?)
(define (string->list-terms string)
  ; (let ((split-str-seq 
  ;         (regexp-partition 
  ;           '(: "(" (or "?" "??") space (+ alphanumeric) ")") 
  ;           string)))
  ;   (fold
  ;     (lambda (str res) 
  ;       (cond 
  ;         ((n:= 0 (string-length str)) res)
  ;         (() consequent2))
  ;       )
  ;     )
  ;   )
  (let ((last-match-end 0))
    (regexp-fold '(: "(" (or "?" "??") space (+ alphanumeric) ")")
                (lambda (i m str acc)
                  ; (write-line 
                  ;   (list i str 
                  ;     (regexp-match-submatch m 0)
                  ;     (regexp-match-submatch-start m 0)
                  ;     ))
                  (let ((sub-string-before (substring str last-match-end (regexp-match-submatch-start m 0))))
                    (set! last-match-end (regexp-match-submatch-end m 0))
                    (append acc (string->list sub-string-before) (list (var-str->var (regexp-match-submatch m 0))))
                    )
                  )
                '()
                string
                (lambda (i m str acc)
                  (append acc (string->list (substring str last-match-end)))
                  )
                ; 0
                ; (string-length string)
                )
    )
  )
(string->list-terms "(?? seg1)(?? seg1)")
(string->list-terms "aa")
(string->list-terms "gctgct")
;;; IGNORE: Here I assume "unification of strings" same as "unification of two expressions" before
;; so that we just pass 2 str's to unifier.
;; The case 
;;; Here we need do substitution on the transformed list, so do that transformation outside.
; (define (unify:string-terms terms1 terms2)
;   (let ((first1 (car terms1)) (rest1 (cdr terms1))
;         (first2 (car terms2)) (rest2 (cdr terms2)))
;     (let ((first1-lst (string->list-terms first1))
;           (first2-lst (string->list-terms first2)))
;       ;; similar to unify:list-terms
;       (define (unify-strs dict succeed fail)
;         ((unify:dispatch first1-lst first2-lst)
;           dict
;           (lambda (dict* fail* null1 null2)
;             (assert (null? null1))
;             (assert (null? null2))
;             (succeed dict* fail* rest1 rest2))
;           fail))
;       unify-strs
;       )))
; (define-generic-procedure-handler unify:gdispatch
;   (match-args (car-satisfies string?)
;               (car-satisfies string?))
;   unify:string-terms)

(define (char-val->str-val dict)
  (match:map-dict-values 
    (lambda (val)
      (if (list? val)
        (map string val)
        (string val))
      )
    dict))

(define (concatenate-segment-var-str-val dict)
  ;; similar to match:map-dict-values
  (match:map-bindings
    (lambda (binding)
      (if (eq? '?? (match:binding-type binding))
        (match:map-binding-value 
          (lambda (str-seq)
            (assert (every string? str-seq))
            ; (write-line (list "str-seq" str-seq))
            ;; keep list for compatibility
            (list (apply string-append str-seq))
            ) 
          binding)
        binding
        )
      )
    dict
    )
  )

(define (all-to-str lst)
  (map 
    (lambda (elm) 
      (if (not (string? elm))
        (string elm)
        elm
        )  
      ) 
    lst)
  )

(define (match:dict-substitution-for-str-transformed-lst dict lst)
  (apply 
    string-append
    (all-to-str
      ((match:dict-substitution 
        (concatenate-segment-var-str-val (char-val->str-val dict))) 
        lst)))
  )

(define (str-unifier pattern1 pattern2)
  (let ((lst1 (string->list-terms pattern1))
        (lst2 (string->list-terms pattern2)))
    (write-line (list "str-lsts:" lst1 lst2))
    (let ((dict (unify lst1 lst2)))
      (write-line (list "dict:" dict))
      (and dict
          ;; modified
          (match:dict-substitution-for-str-transformed-lst dict lst1)
          ))
    ))
(str-unifier "(? seg1)(? seg1)" "aa")
(str-unifier "(? seg1)(? seg1)" "gctgct")

; (trace unify:dispatch)
; (trace maybe-grab-segment)
(str-unifier "(?? seg1)(?? seg1)" "aa")
(str-unifier "(?? seg1)(?? seg1)" "gctgct")

(str-unifier
  "test1(?? seg1)abc(?? seg1)(?? seg2)bc(? var1)(?? seg3)"
  "test1gctabcgctpk(?? seg1)bca(?? seg1)"
  )
; ("dict:" (dict (seg3 (#\g #\c #\t) ??) (var1 #\a ?) (seg2 (#\p #\k #\g #\c #\t) ??) (seg1 (#\g #\c #\t) ??)))
