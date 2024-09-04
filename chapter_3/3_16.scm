(cd "~/SICP_SDF/SDF_exercises/chapter_3")
(load "../software/sdf/manager/load.scm")
(manage 'new 'user-defined-types)

(load "../common-lib/utils.scm")
(start-adventure 'anonymous)

(do ()
  ;; see `(look-around avatar)` -> `(display-message message port)` where `(get-name item)` is to get the string name
  ;; and `(create-mit)`.
  ((equal? 'bldg-26 (get-name (get-location my-avatar)))
    (go 'east)
    (take-thing 'sicp)
    (go 'skew)
    (drop-thing 'sicp)
    )
  (displayln "start one new adventure")
  (start-adventure 'anonymous))