;; By searching "take-thing" with "*.rkt,*.scm" in VSCode, only 6.945_assignment_solution has sample implementation.
(cd "~/SICP_SDF/SDF_exercises/chapter_3")
(load "../software/sdf/manager/load.scm")
(manage 'new 'user-defined-types)

(define (what-to-do person)
  (go 'east)
  (take-thing 'sicp)
  (go 'skew)
  (drop-thing 'sicp))
(define (pred)
  (equal? 'bldg-26 (get-name (get-location my-avatar))))
(load "adventure-lib.scm")
(restart-game-until-pred pred 'anonymous what-to-do)