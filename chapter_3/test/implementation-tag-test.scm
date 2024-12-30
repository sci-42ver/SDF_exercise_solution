(cd "~/SICP_SDF/SDF_exercises/chapter_3")
(load "../software/sdf/manager/load.scm")
(manage 'new 'user-defined-types)

(implementation-type-name #f)
;; prints simple-tag for make-simple-predicate -> make-simple-tag with name 'boolean.
;; So tag-name -> tag-shared-name is 'boolean
(implementation-tag #f)
(hash-object #t)
(implementation-tag #t)
; #[<simple-tag> boolean]

; (define tmp-set-predicate-metadata!)
; (let ((association (make-metadata-association)))
;   (set! tmp-set-predicate-metadata! (association 'put!)))
; (tmp-set-predicate-metadata! integer? 'integer)

;; https://lists.nongnu.org/archive/html/mit-scheme-devel/2019-03/msg00000.html
; (define-print-method integer? #f)
(define-print-method integer?
  (standard-print-method
        (lambda (obj)
          (string-append "integer-" (number->string obj)))
      (lambda (obj) (list 'suffix)))
  )
;This dispatcher requires registered predicates: (#[compound-procedure predicate])
; 1

(trace get-tag-shared)
;; IGNORE: will 
; (%make-simple-tag 'ignore)
