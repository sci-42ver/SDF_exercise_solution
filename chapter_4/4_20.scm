(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'unification)

(load "4_19_lib_for_4_20.scm")

;; 0. As SDF_exercises/chapter_4/4_19.scm shows, we should get all dicts and then select that most general.
;; 0.a. from main-test.
; (trace unify:collector-wrapper-with-substitution-unique-pairs)
; (trace substitution-instance?)
;; I temporarily use old-unify here to avoid unnecessary messy debug messages.
(define old-unify unify)
(define (substitution-instance? substitution1 substitution2)
  (and (general>=? substitution1 substitution2)
       (old-unify substitution1 substitution2)
       )
  )

(define (unify pattern1 pattern2)
  (let* ((all-pairs 
          (unify:collector-wrapper-with-substitution-unique-pairs
              pattern1 pattern2
                          (match:new-dict)
                          (lambda (dict)
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
        preferred-dict))
    )
  )
(unify:internal '((?? x) 3) '(4 (?? y))
                (match:new-dict)
                (lambda (dict)
                  (pp (match:bindings dict))
                  #f))
(unifier '((?? x) 3) '(4 (?? y)))
