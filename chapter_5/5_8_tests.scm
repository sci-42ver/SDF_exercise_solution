;; Not put inside 5_8_book_ver.scm etc (see the last comment of SDF_exercises/software/sdf/manager/software-manager.scm new-environment).
(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "../software/sdf/manager/load.scm")
(manage 'new 'non-strict-arguments)

;;; 5_8_book_ver.scm
;; not work if having (init) there since when this is finished, (init) is also implicitly finished unexpectedly...
; (load "5_8_book_ver.scm")

; (define use_5_8_book_ver #t)
;; Won't work
; (define (test filename pred)
;   ;; not use (and pred (init) rest...) which will run rest... inside and-env instead of that inside (init).
;   (if pred
;     (begin
;       (init)
;       (load-library filename)
;       (write-line "run")
;       (ref-stream (solve (lambda (x) x) 1 0.001) 1000))
;     ))
; (test "5_8_book_ver.scm" use_5_8_book_ver)
;; see
; (read)
; 'test
; ;Value: (quote test)
; ((lambda () 'test))
; ;Value: test
; ;; This is just like running (read) and *then* 'test which won't be fed into the input port.
; ((lambda () (read) 'test))

;; Similarly not work
; (if use_5_8_book_ver
;   (begin
;     (init)
;     (load-library "5_8_book_ver.scm")
;     (ref-stream (solve (lambda (x) x) 1 0.001) 1000))
;   )

(init)
;; > We get an error—ugh!
(load-library "5_8_book_ver.scm")
;; fail as SICP says in section 3.5.4
; (ref-stream (solve (lambda (x) x) 1 0.001) 1000)
;Unbound variable: dy

;; > What has to be changed to make this work as expected?
(load-library "5_8.scm")
; (ref-stream (solve (lambda (x) x) 1 0.001) 1000)
; 2.716923932235896

(load-library "5_8_code_base_ref.scm")
(ref-stream (solve (lambda (x) x) 1 0.001) 1000)
; 2.716923932235896

