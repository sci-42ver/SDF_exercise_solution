(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "../software/sdf/manager/load.scm")
(manage 'new 'generic-interpreter)

(display (g:read))

;; 0. from SICP
;; modified to have the same behavior as the above
(define (prompt-for-input string)
  ;; TODO I don't know whether the current "output port" is "able to tell whether or not they are at the beginning of a line of output"
  ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Output-Procedures.html#index-fresh_002dline
  ;; Anyway this always does as newline.
  (fresh-line) ; will do the same thing as (fresh-line).
  (newline) 
  ;; seemingly flush automatically.
  (display string)
  ; (newline)
  )
(define (loop)
  (prompt-for-input "eval> ")
  (display (read))
  )
(loop)

;; EOF will make newline at least for MIT/GNU Scheme.
;; see newline doc.
(begin (write '+) (display '+))
(begin (write-line '+) (display '+))
