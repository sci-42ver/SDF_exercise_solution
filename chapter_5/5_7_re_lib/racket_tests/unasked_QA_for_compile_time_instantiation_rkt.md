<!-- Similar to module_visit.md -->

This is one follow-up question of [this QA answer][1].

For `file.rkt` based on the above answer and [the doc][2]:
```racket
(module compile-time-number racket/base
  (require (for-syntax racket/base))
  (current-directory "/tmp")
  (begin-for-syntax                          
    (printf "dir = ~a~n" (current-directory)))
  (begin-for-syntax
    (printf "picked ~a\n" (random)))
  (printf "running\n"))
```

We have the following behavior:
```bash
/home/user/ $ racket compile_time_instantiation.rkt
dir = /home/hervey_arch/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/racket_tests/
picked 0.29720130744806306
running
/home/user/ $ racket
Welcome to Racket v8.16 [cs].
> (dynamic-require "compile_time_instantiation.rkt" #f)
dir = /home/hervey_arch/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/racket_tests/
picked 0.5217496884343995
running
; now in /tmp
/home/user/ $ racket
Welcome to Racket v8.16 [cs].
> (require "compile_time_instantiation.rkt")
dir = /home/hervey_arch/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/racket_tests/
picked 0.4458015397486092
running
; now in /tmp
dir = /tmp/
picked 0.09498184401454023
```

Then can we think that `racket file.rkt` is same as `(dynamic-require "file.rkt")` since the former will "*Evaluate*/load expressions/files in order, until first error" as `racket -h` says and [doc2][3] says that the latter will
> Meanwhile, dynamic-require only *instantiates* a module; it does not visit the module.



  [1]: https://stackoverflow.com/a/79543395/21294350
  [2]: https://docs.racket-lang.org/guide/macro-module.html#(part._.Visiting_.Modules)
  [3]: https://docs.racket-lang.org/reference/module.html