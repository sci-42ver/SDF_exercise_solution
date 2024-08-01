This is licenced same as [SDF book](https://mitpress.mit.edu/9780262045490/software-design-for-flexibility/) using [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International Public License](https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode.txt) since most of my codes are based on the course code base and the book. 

I don't know whether it is appropriate. If it is inappropriate and someone knows detailedly about the difference among licences, please tell me the appropriate choice.
# Notice
- Sometimes, I only give one sample test since I didn't intend to learn how to write the safe tests.
- I use naming with `_` instead of `-` since words constructed with the former can be selected in VSCode with the double clicks. 
- I won't dig into checking whether it is  appropriate to use `eq` or `equal` etc. for each case.
- solution comment also see codes.
- Here I use the absolute path like `(load "~/SICP_SDF/SDF_exercises/software/sdf/abstracting-a-domain/game-interpreter.scm")` to make we can call `scheme < foo.scm` in any dir.
  You can use `sed` etc. to change this.
# SDF_exercise_solution
exercise solution for Software Design for Flexibility (SDF)

## other sdf solutions (by "Software Design for Flexibility exercise solution github")
"Software Design for Flexibility exercise solution" and "Software Design for Flexibility exercise solution gitlab" (almost nothing related) or "... github" both doesn't have more related links with exercises.
- partial
  [This](https://github.com/nbardiuk/software-design-for-flexibility) is one repo with solutions to some exercises although too *less*.
  [This](https://github.com/mbillingr/sdf) has partial solutions by [this search](https://github.com/search?q=repo%3Ambillingr%2Fsdf%20parallel-combine&type=code) although without appropriate tests.
  [This](https://github.com/chebert/software-design-for-flexibility) ~~I *can't find the relation* with the exercises~~ gives partial solution in *Racket* based on chapter. (This seems to be the most detailed one)
    ~~ https://github.com/chebert/software-design-for-flexibility/blob/5eacde59855888180482031b9ee27c7c7b6cf4d1/sdf-function-combinators.rkt#L110-L111 implies it *doesn't solve the exercise* at all.~~
    - ~~Since it uses something like `arity-at-least?`, it will take unnecessary time to check Racket doc. So I won't read this implementation except when I am really stuck at something.~~
  - [~~this~~](https://github.com/jeffhhk/SoftwareDesignForFlexibility) has *no* solutions as README says.
- See [this with annotations](https://docs.google.com/document/d/1oyk2EHiTfSe1t0Wbr-HJtfVVvLvulonckpOWaD5KD3E/edit) from [this](https://docs.google.com/document/d/1daYgzQX6Wuxy-iEA9jqpu8b50a_Zbd0FxvKswxAV478/edit)
  [summary](https://docs.google.com/document/d/1vs9ep6A9VLvtn6QLMalYgxQSMXvfPdZA0vdVT6POoJs/edit#)
  - The above seems to have no solutions for Assignment by searching "Assignment".
  - I [can't join its slack](https://compositional-cb63110.slack.com/) room and this course has finished
### different languages
- ~~[this having ~~the most~~ partial solutions although with full chapter dir's ...](https://github.com/compclub/exercises/blob/main/chapter-2-dsl/rmoehn/README.md) uses Clojure~~
    https://github.com/compclub/exercises/blob/main/chapter-2-dsl/rmoehn/README.md
    > I'm not completing most of the exercises, since I assume the initial thinking about the solution is the most valuable part for me.
    Weird man. So
    > so I might end up *fooling myself*.
- [~~this~~](https://github.com/compclub/projects) uses *Rust*...
- ~~https://github.com/bishwa-poudel/Software-Design-and-Architecture-Specialization-Coursera~~ weirdly use Java.
# exercise solutions by 3 references (i.e. nbardiuk etc.) and 6.945_assignment_solution checked up to now
- 2.1~2.11
# nbardiuk solution comment
By https://github.com/search?q=repo%3Anbardiuk%2Fsoftware-design-for-flexibility%20exercise&type=code it probably only has 3 exercise solutions.
## 2.1
- > ; (assert (= 2 (get-arity h))) this is too naive - does not allow to use 'list
  Not. Since if this holds, then `(assert (= 1 f-arity))` also doesn't work.
## 2.2
- `(guarantee-procedure-of-arity h (make-procedure-arity 2) 'h)` implies `h` can have more than 2 arguments.
## 2.3
- totally independent from 2.1~2 without any assertion.
# mbillingr solution comment
## 2.1
- `call-with-values`
  > the continuation expects to receive the same number of *values* that procedure accepts as *arguments*.
  > Thunk must return multiple values using the values procedure. Then procedure is called with the multiple values as its arguments. The result yielded by procedure is returned as the result of call-with-values
  This implies `compose`.
  > Thunk is invoked with a continuation that expects to receive multiple values;
  i.e. what it does will "receive multiple values" as `(apply g args)` implies. See https://www.gnu.org/software/guile/manual/html_node/Continuations.html
- TODO
  ['"multiple-value" continuation'](https://www.reddit.com/r/scheme/comments/zdp1b5/srfi_244_multiplevalue_definitions/)
## 2.2
The following is more general than mine since it allow the case where f has variable argument number and g has the fixed argument number.
```scheme
    (assert (or (fixed-arity? arity-f)
                (fixed-arity? arity-g)))
```
## 2.3 (See codes)
## lack
2.4
# chebert solution comment
It seems to have no test files by searching "r:seq" with only 1 result file.
## 2.1 (See code comments in this repo)
## 2.2
- `combine-arities` is a bit different due to `arity-list`.
## 2.3
- without any assertion same as nbardiuk.
## TODO
- 2.7
  - > (ere expr) or (bre expr)
    > multiple references ...
## lack
# internal func description
- Use `((lambda (x) x) (values 1 2))` to show what `values` returns.
- `reduce` [diff](https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Folding-of-Lists.html#index-reduce) `fold`
  kw: "right identity"
  > Note that ridentity is used *only* in the empty-list case.
  - > you’d like to avoid the extra application incurred when fold applies f to the head of list and the identity value
    i.e.
    > return (fold f (car list) (cdr list)).
    But this just decreases *only one* operation.
## TODO
- > If multiple returns occur from map, the values returned by earlier returns are not mutated.
  https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Mapping-of-Lists.html
  Does it mean https://groups.google.com/g/comp.lang.scheme/c/ruhDvI9utVc/m/jS0DdIh2yKMJ? Then what does "earlier returns" mean?
- > However, in cases where this ambiguity can arise, you should use find-tail instead of find—find-tail has no such ambiguity:
  IMHO both "no such pair" and "doesn’t find such an element" will occur.
- The following are both undefined in saved-total-index and MIT_Scheme_Reference.
  - list-of-unique-symbols?
  - list-of-type?
    - maybe mean a list of data whose type satisfies `pred` by `(list-of-type? object change?)`.
# exercise comments
## chapter 2
- 2.9
  - [POSIX_regex]
    > The string matched by a contained subexpression shall be within the string matched by the containing subexpression. If *the containing subexpression does not match*, or if there is no match for the contained subexpression within the string matched by the containing subexpression, then back-reference expressions corresponding to the contained subexpression shall not match.
    See "the expression "\(a\(b\)*\)*\2" fails to match 'abab'" where b must correspond to "\2", then "aba" can be matched to "the containing subexpression".
    > When a subexpression matches more than one string, a back-reference expression corresponding to the subexpression shall refer to *the last matched string*.
    See "the expression "^\(ab*\)*\1$" matches 'ababbabb', but fails to match 'ababbab'." where the last "abb" is matched to "\1".
- 2.10
  - [factor](https://stackoverflow.com/a/5600318/21294350)

[POSIX_regex]:https://pubs.opengroup.org/onlinepubs/9699919799/nframe.html