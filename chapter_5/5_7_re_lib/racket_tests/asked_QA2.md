<!-- CHECKED see "Just read [the doc]" -->

What `racket file.rkt` actually does to load that file?

---

I wanted to know about this due to the ambiguity got from [QA1 I asked before][1]. That implies `racket file.rkt` is a bit different from `(require "file.rkt")`.

Use the same example as that:
```racket
(module compile-time-number racket/base
  (require (for-syntax racket/base))
  (begin-for-syntax
    (printf "picked ~a\n" (random)))
  (printf "running\n"))
```

with behaviours:
```bash
$ racket -i -e '(dynamic-require "compile_time_instantiation.rkt" #f)'
Welcome to Racket v8.16 [cs].
picked 0.015421409441077423
running
$ racket -i -e '(require "compile_time_instantiation.rkt")'
Welcome to Racket v8.16 [cs].
picked 0.14007121863188537
running
picked 0.7200261945290138
$ racket compile_time_instantiation.rkt
picked 0.9883107842338839
running
```

`racket -h` says:
> *Evaluate*/load expressions/files in order, until first error

[doc1][2] says:
> Meanwhile, dynamic-require only *instantiates* a module; it *does not visit the module*. That simplification is why some of the preceding examples use dynamic-require *instead of require*.

> When a module is instantiated, the *run-time expressions* in its body are *evaluated*.

---

`dynamic-require` meaning

I tried searching for ["Dynamically instantiate module in racket" said in doc2][3] but not gets much about what "Dynamically instantiate" means. Then I checked [QA2 answer][4] and it implies `dynamic-require` is just ["Dynamically loading"][5]. That is also implied by [QA3][6]
> If I want to optionally require a module at *runtime*, I can use dynamic-require

Normally we don't have *direct* access to executable in Racket interpreter, so we can't directly check whether the above "Dynamically loading" definition is fine for `dynamic-require`. I assume that is right.

Anyway this is not enough to differentiate between `dynamic-require` and `require` due to *visit* differences and the following in [doc3][7] at least.
>  A require form in a expression context or internal-definition context is a syntax error.

---

The above seems to imply that both `racket file.rkt` and `dynamic-require` only evaluate the run-time expressions which is different from `require`, so that two are similar. Could you give one brief introduction about what `racket file.rkt` actually does to load `file.rkt`?


  [1]: https://stackoverflow.com/q/79549370/21294350
  [2]: https://docs.racket-lang.org/guide/macro-module.html#(part._.Visiting_.Modules)
  [3]: https://docs.racket-lang.org/reference/Module_Names_and_Loading.html#%28def._%28%28quote._~23~25kernel%29._dynamic-require%29%29
  [4]: https://stackoverflow.com/a/64447199/21294350
  [5]: https://stackoverflow.com/q/23114414/21294350
  [6]: https://stackoverflow.com/q/42632854/21294350
  [7]: https://docs.racket-lang.org/reference/require.html

---

Just read [the doc](https://docs.racket-lang.org/guide/racket.html#(part._start-module-mode))
> *requires* the "hello.rkt" module and then exits. Any argument after the file name, flag or otherwise, is preserved as a command-line argument for *use by the required module*
>
> If command-line flags are used, then the -u or --require-script flag can be used to explicitly *require* a file as a module. The -t or --require flag is similar, except that additional command-line flags are processed by racket, *instead of preserved for the required module*.

And then `racket -h`:
>   -u <file>, --require-script <file>
>     Same as -t <file> -N <file> --
>
>   -t <file>, --require <file>
>     Like -e '(require (file "<file>"))' [*]
>
>   -N <file>, --name <file>
>     Sets `(find-system-path 'run-file)` to <file>

So it just does `-e '(require (file "<file>"))'` but without `-i`. Then [this](https://stackoverflow.com/a/79549588/21294350) also answers this question.
```bash
$ racket -e '(require (file "compile_time_instantiation.rkt"))' -N "compile_time_instantiation.rkt"
picked 0.8672117526596517
running
```
