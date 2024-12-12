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

;;; added
;; seq except for list
(define (not-arbitrary-seq? seq)
  (not (vector? seq)))

(define old-list? list?)
(define list?
  (simple-generic-procedure 'list? 1
    (lambda (data)
      (error (list "wrong arg for list?" data))
      )
    ))
(define-generic-procedure-handler list?
  (match-args not-arbitrary-seq?)
  old-list?)
(define-generic-procedure-handler list?
  (match-args vector?)
  vector?)

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
      (error (list "wrong arg for null?" data))
      )
    ))
(define-generic-procedure-handler null?
  (match-args not-arbitrary-seq?)
  old-null?)
(define-generic-procedure-handler null?
  (match-args vector?)
  (lambda (data) (= 0 (vector-length data))))

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

(define old-append-map append-map)
; https://stackoverflow.com/a/18625764/21294350
(define (vector-append vect-of-list)
  (list->vector (apply append (vector->list vect-of-list))))
(define (append-map proc seq)
  (cond 
    ((old-list? seq) (old-append-map proc seq))
    ;; Here just use list as the transformation medium.
    ;; Here (vector-map proc seq) is (vector (list ...) (list ...) ...).
    ((vector? seq) (vector-append (vector-map proc seq)))
    (else (error (list (string-append "wrong arg" " " "for append-map") seq)))
    ))
