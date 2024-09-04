(load "../common-lib/utils.scm")
(define (restart-game-until-pred pred my-name what-to-do)
  (start-adventure my-name)
  
  (do ()
    ;; see `(look-around avatar)` -> `(display-message message port)` where `(get-name item)` is to get the string name
    ;; and `(create-mit)`.
    ((pred)
      (what-to-do my-avatar)
      )
    (displayln "start one new adventure")
    (start-adventure my-name)))