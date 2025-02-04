(define (match:lookup-corrected var dict)
  (let ((name
          (if (symbol? var)
            var
            (match:var-name var))))
    (find (lambda (binding)
            (and (eq? name (match:binding-name binding))
                 (eq? (match:var-type var) (match:binding-type binding))
                 ))
          (match:bindings dict))))
(define match:lookup match:lookup-corrected)

(define (binding-to-pair binding)
  (let ((name (match:binding-name binding)) (type (match:binding-type binding)))
    (list (list type name) (match:binding-value binding)))
  )
(define (dict->pair-dict dict)
  (match:map-bindings binding-to-pair dict))

;; 0. notice here we only do one level rename to check whether they can be same.
;; so with w->y, then y->w won't replace y with w first.
;; 1. Here we can think of each var as one *element var*, then rename can be done by unify.
;; 1.a. similar to unify:internal
(define (rename-var-to-idxed-element-var dict succeed)
  ((rename:dispatch (match:bindings dict))
   (match:new-dict)
   (lambda (dict* fail* rest1)
     (assert (list? rest1))
     (or (and (null? rest1)
              (succeed dict*))
         (begin
           ; (write-line (list "error for unify:internal" rest1))
           (fail)))
     )
   (lambda () #f)
   )
  )
;; similar to unify:dispatch
(define (rename:dispatch bindings)
  (assert (list? bindings))
  (define (rename-dispatcher dict succeed fail)
    (if (null? bindings)
      (succeed dict fail bindings)
      ((rename:gdispatch bindings)
       dict
       (lambda (dict* fail* rest1)
         ((rename:dispatch rest1)
          dict* succeed fail*))
       fail)))
  rename-dispatcher)
(define (rename:fail terms1)
  (define (rename-fail dict succeed fail)
    (begin
      ; (write-line (list "rename:fail " terms1))
      (fail)))
  rename-fail)
(define rename:gdispatch
  (simple-generic-procedure 'rename 1 rename:fail))
(define (rename:constant-terms terms1)
  (let ((first1 (car terms1)) (rest1 (cdr terms1)))
    (define (rename-constants dict succeed fail)
      (succeed dict fail rest1))
    rename-constants))
(define-generic-procedure-handler rename:gdispatch
                                  (match-args (car-satisfies constant-term?))
                                  rename:constant-terms)
(define (rename:list-terms terms1)
  (let ((first1 (car terms1)) (rest1 (cdr terms1))
                              )
    (define (rename-lists dict succeed fail)
      ((rename:dispatch first1)
       dict
       (lambda (dict* fail* null1)
         (assert (null? null1))
         (succeed dict* fail* rest1))
       fail))
    rename-lists))
;; coderef: rename-list-terms
(define-generic-procedure-handler rename:gdispatch
                                  (match-args (car-satisfies list-term?))
                                  rename:list-terms)
;; similar to type-variable
(define (type-variable-with-pred pred #!optional root)
  `(? ,(generate-unique-name
        (if (default-object? root)
            'type
            root))
      ,pred
      ))
;; similar to 
(define (seg-var-repr? var)
  (and
    (match:var? var)
    (match:var-has-restriction? var)
    (eq? seg-var-repr? (match:var-restriction var))
    )
  )
(define (rename-var var-first)
  (define (rename dict succeed fail)
    (let ((var (car var-first)) (rest1 (cdr var-first)))
      (cond ((match:has-binding? var dict)
             ;; IGNORE: will check consistency later.
             ;; Since we do renaming, we only need to bookkeep how var's are renamed.
             ;; *No conflict* will occur.
             (succeed dict fail rest1))
            (else
              ;; wrap list to be compatible with segment
              (let ((renamed-var 
                      (if (match:segment-var? var)
                        (list (type-variable-with-pred seg-var-repr? 'seg-var))
                        var)))
                (let ((dict* (do-substitute var renamed-var dict)))
                  (if dict*
                    (succeed dict* fail rest1)
                    (begin
                      (write-line (list "error for do-substitute" var term))
                      (fail))))
                )))))
  rename)
(define-generic-procedure-handler rename:gdispatch
                                  (match-args (car-satisfies match:var?))
                                  (lambda (var-first)
                                    (rename-var var-first)))

(define (same-dict? dict1 dict2)
  ;; Better to still use continuation since we need to check the next list term directly after digging into the former list term.
  ; (let lp ((rest-dict1 dict1) (rest-dict2 dict2) (rename-dict (match:new-dict)))
  ;   (let ((binding1 (car rest-dict1)) (binding2 (car rest-dict2)))
  ;     (let ((var1 ))
  ;       ())
  ;     )
  ;   )
  (let ((pair-dict1 (dict->pair-dict dict1))
        (pair-dict2 (dict->pair-dict dict2))
        )
    (let ((dict1-rename-dict (rename-var-to-idxed-element-var pair-dict1 (lambda (dict) dict)))
          (dict2-rename-dict (rename-var-to-idxed-element-var pair-dict2 (lambda (dict) dict)))
          )
      (let ((dict1* ((match:dict-substitution dict1-rename-dict) pair-dict1))
            (dict2* ((match:dict-substitution dict2-rename-dict) pair-dict2))
            )
        ; (pp (list "same-dict?" 
        ;   (list dict1 dict2) 
        ;   (list pair-dict1 pair-dict2)
        ;   (list dict1-rename-dict dict2-rename-dict)
        ;   (list dict1* dict2*)))
        (and (unify dict1* dict2*)
             #t
             )
        )
      )
    )
  )

;;; SKIPPED since it is not needed by SDF_exercises/chapter_4/4_19_lib_for_4_20.scm: TODO
(write-line "same-dict? test1")
(pp (same-dict? '(dict (y ((?? w)) ??)) '(dict (y ((? w)) ?))))
;; 0. wrong due to not checking type beforehand.
; ("same-dict?" ((dict (y ((?? w)) ??)) (dict (y ((? w)) ?)))
;               ((dict ((?? y) ((?? w)))) (dict ((? y) ((? w)))))
;               ((dict (w ((? w:4)) ??) (y ((? y:3)) ??)) (dict (w (? w:2) ?) (y (? y:1) ?)))
;               ((dict ((? y:3) ((? w:4)))) (dict ((? y:1) ((? w:2))))))
;; 1. What's more, the renaming check is much complexer with more bindings.
;; That is due to something like
;; (binding1 binding2 binding3 ... bindingN) matched with (binding1* binding2* binding3* ... bindingN*)
;; may have N! possible matches.
;; 1.a. So we should add one wrapper outside unify
;; so that the 1st binding can match any of other *seg-var*'s at the other side if seg-var.
;; This "any" mechanism can be done similar to unify:segment-var-var where we changes success continuation correspondingly.

;;;
(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/unification/unify-testing.scm")
(load "../software/sdf/design-of-the-matcher/matcher.scm")
(pp (unify:alpha-equivalent? '(?? w) '(? w)))

;; This should return #f since (? w) *can't match all* things (?? w) can match.
(pp (unify:alpha-equivalent? '(? w) '(?? w)))
;; same #f as the above shows.
(pp (unify-test '(? w) '(? w) '(dict (w (?? w) ?)) #t))
