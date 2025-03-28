#lang racket
; no use to change the relative dir for require https://stackoverflow.com/q/79535844/21294350
(current-load-relative-directory)
(current-load-relative-directory (expand-user-path "~/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/racket_lib"))
(current-load-relative-directory)

(current-directory)
(current-directory
  (expand-user-path "~/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/racket_lib")
  )
(current-directory (expand-user-path "~/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/5_7_regexp_lib_simplified_based_on_effbot.rkt"))
(current-directory)

;; https://stackoverflow.com/q/79535844/21294350
; (parameterize 
;   ([current-directory 
;     (expand-user-path "~/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/racket_lib")])
;   (require "tests_lib.rkt")
;   )
;; require: not at module level or top level

;; https://stackoverflow.com/a/6380648/21294350
;; implied in `racket -h`.
(define dir-str "~/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/racket_lib/")
;; no use
; (require (file (string-append dir-str "tests_lib.rkt")))
;; file: ill-formed module path

; (define (require-file dir file)
;   ; (let ((abs-file (string-append dir file)))
;   ;   (require (file abs-file)))
;   ;; not at module level or top level
;   ; (define abs-file (string-append dir file))
;   ; (require (file abs-file))
;   ;; still "not at module level or top level"
;   )
; (require-file dir-str "tests_lib.rkt")

(require (file "~/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/racket_lib/tests_lib.rkt"))

;; As the 1st QA says, this will always use this source's parent dir.
(require "../racket_lib/tests_lib.rkt")
