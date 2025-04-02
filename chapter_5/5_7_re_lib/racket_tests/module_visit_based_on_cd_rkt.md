This is one follow-up question of this QA answer https://stackoverflow.com/a/79543395/21294350.

For `file.rkt`:
```racket
(module name racket
  (current-directory "/tmp")
  (begin-for-syntax
    (printf "dir = ~a~n" (current-directory)))
  (displayln "test")
  (printf "phase level 0 dir = ~a~n" (current-directory))
  )
```

We have the following behavior:
```bash
/home/user/ $ racket
> (module name racket
    (current-directory "/tmp")
    (begin-for-syntax                          
      (printf "dir = ~a~n" (current-directory)))
    (displayln "test")
    (printf "phase level 0 dir = ~a~n" (current-directory))
    )
dir = /home/user/
/home/user/ $ racket cd.rkt
dir = /home/user/
test
phase level 0 dir = /tmp/
/home/user/ $ racket -i -e '(dynamic-require "cd.rkt" #f)'
Welcome to Racket v8.16 [cs].
dir = /home/user/
test
phase level 0 dir = /tmp/
> (current-directory)
#<path:/tmp/>
/home/user/ $ racket
Welcome to Racket v8.16 [cs].
> (require "cd.rkt")
dir = /home/user/
test
phase level 0 dir = /tmp/
; now in /tmp
dir = /tmp/
/home/user/ $ racket
Welcome to Racket v8.16 [cs].
> (dynamic-require "cd.rkt" #f)
dir = /home/user/
test
phase level 0 dir = /tmp/
; now in /tmp
```

I can understand the 1st $-part as [the doc][1] says:
> The evaluation of a module form does not evaluate the expressions in the body of the module (except sometimes for redeclarations; see Module Redeclarations). Evaluation *merely declares* a module

> A module body is *executed only* when the module is explicitly instantiated via require or dynamic-require.

and [doc2][2] says:
> However, declaring a module does *expand and compile* the module.

> When a module is *visited*, the *compile-time expressions (such as macro definition)* in its body are evaluated.

> As a module is *expanded*, it is *visited*.

> The compile-time expressions of a module that are evaluated by visiting include both the right-hand sides of define-syntax forms and the body of begin-for-syntax forms.

In a nutshell, declare -> expand -> visit -> compile-time expression evaluation.

The 2rd $-part is due to `racket -h`:
> Evaluate/load expressions/files in order, until first error

and doc2
> Instantiating the module *evaluates* only the *run-time expressions*, which prints “running” but not a new random number

and https://docs.racket-lang.org/guide/stx-phases.html (also see https://docs.racket-lang.org/guide/phases.html)
> Thus, the run-time definitions in "utils.rkt" can be used to implement swap, as long as they are explicitly *shifted into compile time* by (require (for-syntax ....)).

The 3rd $-part is actually same as the 5th. See doc1
> A module body is executed only when the module is *explicitly instantiated via require or dynamic-require*.

See doc2
> When a module is instantiated, the run-time expressions in its body are evaluated.

The 4th $-part compared with the 3rd and the 5th is due to `require` will do one more "visit". From doc2
> Meanwhile, dynamic-require only instantiates a module; it does not visit the module.

~~TODO~~ "; now in /tmp" is probably done by "read-eval-print loop" as `racket -h` shows. Anyway this is just one comment.

  [1]: https://docs.racket-lang.org/reference/module.html
  [2]: https://docs.racket-lang.org/guide/macro-module.html#(part._compile-time-instantiation)