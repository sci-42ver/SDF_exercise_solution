;; IGNORE: I won't write a lot of routine init and setting of generic procedures for simplicity.
;; Anyway the basic ideas are same.
;; Due to we need to use general car, I will just redefine car to avoid renaming everywhere...

; (define (vec-null? vec)
;   (n:= 0 (vector-length vec)))
; (define (vec-not-null? vec)
;   (not (vec-null? vec)))
; (define vec-car vector-first)

;;; just modification of SDF_exercises/chapter_4/4_8_generic_procs.scm
; (define (sequence? data)
;   (cond 
;     ((or (list? data) (vector? data)) #t)
;     ((or (symbol? data) (number? data)) #f)
;     (else (error (list "wrong sequence data types" data)))
;     ))

;; So routine...
;; Here I won't consider segment which just follows the same basic ideas...

;; added
(define old-list? list?)
(define list?
  (simple-generic-procedure 'list? 1
                            (lambda (data)
                              (error (list "wrong arg" data))
                              )
                            ))
(define-generic-procedure-handler list?
                                  (match-args old-list?)
                                  old-list?)
(define-generic-procedure-handler list?
                                  (match-args vector?)
                                  vector?)

;; seq except for list
(define (not-arbitrary-seq? seq)
  (not (vector? seq)))

; (define old-cdr cdr)
; (define cdr
;   (simple-generic-procedure 'cdr 1
;     (lambda (data)
;       (error (list "wrong arg" data))
;       )
;     ))
; (define-generic-procedure-handler cdr
;   (match-args not-arbitrary-seq?)
;   old-cdr)
; (define-generic-procedure-handler cdr
;   (match-args vector?)
;   (lambda (vec) (vector-tail vec 1)))

;; due to cons is needed by simple-generic-procedure addition.
;; We can't redefine cons.
; (define old-cons cons)
; (define cons
;   (simple-generic-procedure 'cons 2
;     (lambda (addend data)
;       (error (list "wrong arg" addend data))
;       )
;     ))
; (define-generic-procedure-handler cons
;   (match-args any-object? (lambda (x) (not (vector? x))))
;   old-cons)

; ;; demo
; ; ((lambda (addend vec) 
; ;     (let* ((vec-len (vector-length vec))
; ;            (target-len (+ 1 vec-len))
; ;            (copy (vector-grow vec target-len))
; ;            (target (make-initialized-vector target-len (lambda (x) addend))))
; ;       (subvector-move-left! copy 0 vec-len target 1)
; ;       target)
; ;     ) 1 (vector 2 3))
; ; (subvector-move-left! (vector 2 3) 0 2 target 1)
; (define-generic-procedure-handler cons
;   (match-args any-object? vector?)
;   (lambda (addend vec) 
;     (let* ((vec-len (vector-length vec))
;            (target-len (+ 1 vec-len))
;            (copy (vector-grow vec target-len))
;            (target (make-initialized-vector target-len (lambda (x) addend))))
;       (subvector-move-left! copy 0 vec-len target 1)
;       target)
;     ))
(define (general-cons addend data)
  (define (vec-cons addend vec)
    (let* ((vec-len (vector-length vec))
           (target-len (+ 1 vec-len))
           (copy (vector-grow vec target-len))
           (target (make-initialized-vector target-len (lambda (x) addend))))
      (subvector-move-left! copy 0 vec-len target 1)
      target)
    )
  (cond 
    ((vector? data) (vec-cons addend data))
    (else (cons addend data))))

;; trivial settings for generic
(define old-null? null?)
(define null?
  (simple-generic-procedure 'null? 1
                            (lambda (data)
                              (error (list "wrong arg" data))
                              )
                            ))
(define-generic-procedure-handler null?
                                  (match-args not-arbitrary-seq?)
                                  old-null?)
(define-generic-procedure-handler null?
                                  (match-args vector?)
                                  (lambda (data) (= 0 (vector-length data))))

;; Not use this since this will make match:named-var? wrong.
; (define old-pair? pair?)
; (define pair?
;   (simple-generic-procedure 'pair? 1
;     (lambda (data)
;       (error (list "wrong arg" data))
;       )
;     ))
; (define-generic-procedure-handler pair?
;   (match-args not-arbitrary-seq?)
;   old-pair?)
; (define-generic-procedure-handler pair?
;   (match-args vector?)
;   (lambda (data) (< 0 (vector-length data))))

;; This will cause infinite recursion since generic-procedure uses old-car but we redefines that to generic.
;; So it will keep searching how to car.
;; Similar for cdr.
; (define old-car car)
; (define car
;   (simple-generic-procedure 'car 1
;     (lambda (data)
;       (error (list "wrong arg" data))
;       )
;     ))
; (define-generic-procedure-handler car
;   (match-args not-arbitrary-seq?)
;   old-car)
; (define-generic-procedure-handler car
;   (match-args vector?)
;   vector-first)

(define (general-car seq)
  (cond 
    ((old-list? seq) (car seq))
    ((vector? seq) (vector-first seq))
    (else (error (list (string-append "wrong arg" " " "for car") seq)))
    ))

(define (general-cdr seq)
  (cond 
    ((old-list? seq) (cdr seq))
    ((vector? seq) (vector-tail seq 1))
    (else (error (list (string-append "wrong arg" " " "for cdr") seq)))
    ))

(define (general-pair? seq)
  (cond 
    ((old-list? seq) (pair? seq))
    ((vector? seq) (< 0 (vector-length seq)))
    (else (error (list (string-append "wrong arg" " " "for pair?") seq)))
    ))
