(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'unification)

(load "4_19_lib_for_4_20.scm")
;; 
; (load "4_19_lib_for_4_20_possible_loop.scm")

;; 0. As SDF_exercises/chapter_4/4_19.scm shows, we should get all dicts and then select that most general.
;; 0.a. from main-test.
; (trace unify:collector-wrapper-with-substitution-unique-pairs)
; (trace substitution-instance?)
;; IGNORE: I temporarily use old-unify here to avoid unnecessary messy debug messages.
; (define old-unify unify)
; (define (substitution-instance? substitution1 substitution2)
;   (and (general>=? substitution1 substitution2)
;        (old-unify substitution1 substitution2)
;        )
;   )

;; I temporarily use one new unify here since many lib procs in 4.19 assumes using old unify.
(define (new-unify pattern1 pattern2)
  (let* ((all-pairs 
          (unify:collector-wrapper-with-substitution-unique-pairs
              pattern1 pattern2
                          (match:new-dict)
                          (lambda (dict)
                            ;; will show debug infos.
                            (pp (match:bindings dict))
                            #f)
                          substitution-instance?)))
    ; (write-line "finish all-pairs")
    (let ((general-pairs 
            (sort-and-remove-substitution-instance-for-pairs
              all-pairs
              substitution-instance?
              )))
      ; (write-line "tmp delimiter")
      (let* ((preferred-pair (most-general-pair general-pairs))
            (preferred-dict (result-dict-in-pair preferred-pair)))
        (write-line (list "use preferred-dict" preferred-dict))
        preferred-dict))
    )
  )
(unify:internal '((?? x) 3) '(4 (?? y))
                (match:new-dict)
                (lambda (dict)
                  (pp (match:bindings dict))
                  #f))
(define (unifier pattern1 pattern2)
  (let ((dict (new-unify pattern1 pattern2)))
    (and dict
         ((match:dict-substitution dict) pattern1))))
(unifier '((?? x) 3) '(4 (?? y)))

;;; from SICP_SDF/SDF_exercises/software/sdf/unification/gjs-test.scm
(load "../software/sdf/unification/unify-testing.scm")
(load "../software/sdf/design-of-the-matcher/matcher.scm")
;; IGNORE: Emm... I won't dig into unify-test. Anyway it just unify ((?? x)) with ((?? y) (?? x)) and expects something.
; (unify-test '((?? x))
;             '((?? y) (?? x))
;             '(dict (y () ??))
;             #t)
;; 0. use ?? for all ??? temporarily.
;; 1. Here e:1525 is no use.
; (unify-test '((?? x) 3)
;             '(4 (?? y))
;             '(dict (e:1525 (3) ??) (y ((?? o:1526) 3) ??) (x (4 (?? o:1526)) ??)))

;; test1
;; 0. TODO skipped. See SDF_exercises/chapter_4/4_19_lib_for_4_20_possible_loop.scm
;; 0.a. similar to "SEGMENT SPLITTER loops infinitely, producing new variables."
;; 0.b. anyway /home/czg_arch/SICP_SDF/SDF_exercises/software/sdf/unification/gjs-test.scm
;; also doesn't give one possible SPLITTER result
; ((x-internal:49-right () ???) (x-internal:50-left () ???) (y () ??) (x () ??))
;; based on
; ((x-internal:49-right () ???) (x ((??? x-internal:50-left)) ??) (x ((?? y) (??? x-internal:50-left)) ??))

; (unifier '((?? x))
;           '((?? y) (?? x)))
; (unifier '((?? x) (?? y))
;           '((?? y) (?? x))
;           )
; (unifier '((?? y) (?? x))
;           '((?? x)))

;; all directly (succeed dict fail (cdr var-first1) (cdr var-first2))
(unifier '((?? x) (?? y))
          '((?? x) (?? y))
          )
; ()
;Value: ((?? x) (?? y))

(unifier '((?? x) (?? y))
          '((?? x) (?? x))
          )

;;; SEGMENT SPLITTER tests
;; test1
;; > exponentially explodes on these
;; so slow since each of (?? x) (?? y) (?? x) can match many cases and thses are *multiplied*...
;; $ time scheme < 4_20.scm > 4_20_long_execution.log
;; scheme < 4_20.scm > 4_20_long_execution.log  33.91s user 1.24s system 99% cpu 35.241 total
; (unifier '(a (?? x) (?? y) (?? x) c (?? u))
;           '((? m) b b (?? z) b b (?? w) b b (?? z) c (?? v))
;           )
;;;; info
;;; first
; ((v () ??) (u (b b (?? w) b b c c) ??)
;            (z (c) ??)
;            (y (b b) ??)
;            (x () ??)
;            (m a ?))
;; with u
; (u (b b (?? w) b b c c) ??)

;; then
; (u (b b (?? w) b b c c (??? v-internal:56-left)) ??)
;; then
; (u (b b (?? w) b b c c (?? v)) ??)

;;; second
; (z (c (??? u-internal:58-left)) ??)
;; 

;;;; debug
;; still the problem in SDF_exercises/chapter_4/4_19_lib_for_4_20_possible_loop.scm although it may not cause the infinite loop.
; (dict                                                                                                                                                                                        
;  (v-internal:5183-right (c (?? u)) ???)
;  (v ((??? v-internal:5184-left) c (?? u)) ??)
;  (x-internal:5179-right (c (??? v-internal:5184-left)) ???)
;  (x ((??? x-internal:5180-left) c (??? v-internal:5184-left)) ??)
;  (z-internal:4933-right ((?? y) (??? x-internal:5180-left)) ???)
;  (z ((??? z-internal:4934-left) (?? y) (??? x-internal:5180-left)) ??)
;  (x
;   (b b  
;      (??? z-internal:4934-left)
;      (?? y)
;      (??? x-internal:5180-left)
;      b     
;      b     
;      (?? w)
;      b     
;      b     
;      (??? z-internal:4934-left))
;   ??)
;  (m a ?))

;; expects
; (dict (v ((?? y) b b (?? z) b b (?? w) b b (?? z) c c (?? u))
;                       ??)
;                    (x (b b (?? z) b b (?? w) b b (?? z) c) ??)
;                    (m a ?))

;; test2
(unifier '(((?? x) 4) ((?? x)) (3 4))
         '(((?? y) (?? w)) (3) ((?? y))))
;; TODO I don't what o:1522 etc are in gjs test
(unifier '(((?? y) (?? w)) (3) ((?? y)))
         '(((?? x) 4) ((?? x)) (3 4)))

;; test3 not included before by `grep "((?? x))" -r ./**/*.scm | grep "(?? y)"`
(unifier '((?? x))
          '((?? y) 3))

;; test4 more general result than gjs-test
(unifier '((?? x) (?? y))
          '((?? z) (?? w)))
; ("use preferred-dict" (dict (y-internal:61-right ((?? w)) ???) (y ((??? y-internal:62-left) (?? w)) ??) (z ((?? x) (??? y-internal:62-left)) ??)))
