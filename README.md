This is licenced same as [SDF book](https://mitpress.mit.edu/9780262045490/software-design-for-flexibility/) using [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International Public License](https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode.txt) since most of my codes are based on the course code base and the book. 

I don't know whether it is appropriate. If it is inappropriate and someone knows detailedly about the difference among licences, please tell me the appropriate choice.
# Skipped exercise
- 2.14
  - I tried to find one possible example to implement in https://martinfowler.com/dsl.html#:~:text=Domain%2Dspecific%20languages%20have%20been,strut's%20configuration%20file....
    but only ["ant"](https://news.ycombinator.com/item?id=21840947) and ["graphviz's dot language"](https://www.graphviz.org/pdf/dotguide.pdf) *seem* to be implement as one *demo*.
    But the former is related with Java and the latter is related with drawing one graph.
  - ["salary calculation"](https://buddypunch.com/blog/how-is-salary-and-wages-calculated/) in [this](https://en.wikipedia.org/wiki/Domain-specific_language#Overview) doesn't have one general model.
  - After all, this exercise is more about how we think about a domain and then program it.
    IMHO checkers and then manual implementation of chess are enough.
    - "domain model" as SDF_notes.md shows is about "abstract types".
- 3.6-c.
- 3.22-b.
# Notice
- Sometimes, I only give one sample test since I didn't intend to learn how to write the safe tests.
- I use naming with `_` instead of `-` since words constructed with the former can be selected in VSCode with the double clicks. 
- solution comment also see codes.
- Here I use the absolute path like `(load "~/SICP_SDF/SDF_exercises/software/sdf/abstracting-a-domain/game-interpreter.scm")` to make we can call `scheme < foo.scm` in any dir.
  You can use `sed` etc. to change this.
- All exercise solutions will be given related tests. But obviously I won't ensure the test must promise the solution is correct since there is no such a test (Correctness should be proved by maths).
## I *won't dig into* 
- checking whether it is appropriate to use `eq?` or `equal?` etc. for each case. (so same for `memv`, etc.)
- tests of sample implementations.
## miscs clipboard
- SDF_exercises TODO
- sci-42ver/SDF_exercise_solution
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
# @exercise solutions by *3 references* (i.e. nbardiuk etc.) and *6.945_assignment_solution* checked up to now (sometimes *code base* has sample implementations)
with check comments in codes.
- 2, 3.1~3.22
# @exercise tests finished
- 2, 3.1~3.22
# @TODO
- I skipped checking the detailed implementation of the following since they are less related with what the book intends to teach
  - `make-predicate-template` (not shown in the SDF book. There is no related funcs even by searching "template")
    So I skipped checking `SDF_exercises/software/sdf/user-defined-types/vector-arith.scm`.
  - `define-generic-procedure-extractor` (not shown in the SDF book. There is no related funcs even by searching "extractor")
## review of "SDF_exercises TODO"
- From cc09d5b919575d7a27d30d94100d2f12dd8248ef up to .
# nbardiuk solution comment
~~By https://github.com/search?q=repo%3Anbardiuk%2Fsoftware-design-for-flexibility%20exercise&type=code it probably only has 3 exercise solutions.~~
It only have solutions up to chapter 2 regular-expressions based on 5 filenames.
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
- 2.12
  - check the rules *intuitively* by manual playing for 2 players https://www.chessmultiplayer.com/
  - > Don't try to implement the castling rule.
    we need to check 3 conditions in wikipedia if to implement it.
    a. "must not have previously moved" ~~can be checked by flag.~~
    b. "There must be no pieces between the king and the rook;" can be checked by inspecting position-info between them
    c. "The king may not currently be ..." i.e. call *check* for all related positions.
    d. "The castling must be kingside or queenside" since they are the only 2 possible cases.
  - All *rules* are based on https://en.wikipedia.org/wiki/Rules_of_chess#Touch-move_rule
    - Here I *won't check* 
      - "Competitive rules of play", i.e. FIDE rules.
      - "Touch-move rule" and "Resigning" since these depends on artifical interposition.
    - *IGNORE* 
      - (see the following) ~~We need to check~~
        > It is illegal to make a move that places or leaves one's king in check.
        - so also *Checkmate*
      - (see "castling".) ~~3. Check similar to `require-jumps`: If failure, then Checkmate~~
        - So we also won't check "stalemate", then combined with "Dead position" we *won't check "draw"*.
      - due to the very hardness of implementation 
        ~~Dead position~~ skipped due to the possible huge combination number (`man_1_options * man_2_options * man_1_options_currect ...` infinitely recursion) and we must check it after each iteration.
        > by *any sequence* of legal moves.
        even if we don't consider the *complexity overhead*, the *correct* algorithm is not easy https://chess.stackexchange.com/a/22764.
        - https://chess.stackexchange.com/a/22557 doesn't say any valuable but just show possibility ...
          - TODO I can't see such a comments for "in the comments user17439".
    - **what to do beyond "Basic moves"**
      1. Promotion is trivial by checking the type and position (similar to `should-be-crowned?`)
    - En passant: check adjacent piece "on the same rank" ~~whether with flag "advances two squares on its initial move"~~ (only on the move *immediately* following the pawn's advance) and type "pawn".
      - I won't check it since pawn can either advance one square twice or advance "two squares" once. "flag" is associated with `pmove` which *won't be remembered* for later usage.
        IMHO the best solution is to add flag with piece but the interface for `piece` will be all changed.
        - Here whether we can remember history steps is also needed in "castling" to test
          > The king and rook involved in castling must *not have previously moved*
  - notice
    after checking https://en.wikipedia.org/wiki/Rules_of_chess#Basic_moves preface
    > The king can be put in check but *cannot be captured* (see below).
  - chebert
    - check castling
      - by *adding types*. (also used for other *special* moves)
## chapter 3
- See 3_3.scm "(only nbardiuk repo is not included in this repo)"
  but it only have chapter 2 implementation.
- mbillingr ~~only has automatic-differentiation.~~ has nothing.
- by `grep "generic" -r .` chebert has nothing for chapter 3 after 3.2 (included).
  Then by searching keywords like `vec-before-func` in exercise 3.1~3.3, it also have no implementations for these.
- 6.945_assignment_solution 
  - ps03 has 3.2, 3.5, 3.6, 3.7, 4.0 although they are not same as 2024 version.
  - ps04 only has one similar implementation of Exercise 3.16.
- 2
  - [vector vs list](https://stackoverflow.com/a/27589146/21294350)
- 11
  - [[111] reference](https://engineering.purdue.edu/~qobi/papers/ifl2005.pdf)
    - ~~TODO why `(D (λy . x + y) 1)` doesn't calculate derivative and gets 2.~~

[POSIX_regex]:https://pubs.opengroup.org/onlinepubs/9699919799/nframe.html
[software-manager-doc]:https://groups.csail.mit.edu/mac/users/gjs/6.945/sdf/manager/software-manager.pdf