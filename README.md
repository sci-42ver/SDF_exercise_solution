https://stackoverflow.com/questions/79522809/how-to-avoid-evaluating-one-statement-excluded-by-predicate-during-the-condition?noredirect=1#comment140254692_79522809
@WillNess Thanks for your detailed description. Maybe it is better to say why I encountered with this problem. My homework needs me to write one simple parser for one infix expression. I used Pratt parsing as wikipedia hints. In scheme we can't have `'(... , ...)` to parse something like Python infix lambda `lambda arg1, arg2: body`. So I use interned symbol `(intern ",")` and store that inside one variable like `COMMA`. Then I use `if` to dispatch based on whether that symbol is special to use interned symbol *value* or simple to use the original representation. Continued...

I don't use `COMMA` to allow some expression like `lambda COMMA COMMA arg2: body` to be be also parsed. More specifically, it will be tokenized to `(lambda COMMA COMMA arg2 : tokenized-body)`.

# skipped exercise
## You can do if you are interested without extra background knowledge assumption
### needs big changes to the overall program structure
- 4.12 See 0.b.
  > 0.b.0.a. Again, wait implies we need to change the unify internal structure...
  - Also see 4.18 TODO which also relates with delay/wait.
### open-ended
- 2.14
  - > This is not easy.
  - I tried to find one possible example to implement in https://martinfowler.com/dsl.html#:~:text=Domain%2Dspecific%20languages%20have%20been,strut's%20configuration%20file....
    but only ["ant"](https://news.ycombinator.com/item?id=21840947) and ["graphviz's dot language"](https://www.graphviz.org/pdf/dotguide.pdf) *seem* to be implement as one *demo*.
    But the former is related with Java and the latter is related with drawing one graph.
  - ["salary calculation"](https://buddypunch.com/blog/how-is-salary-and-wages-calculated/) in [this](https://en.wikipedia.org/wiki/Domain-specific_language#Overview) doesn't have one general model.
  - After all, this exercise is more about how we think about a domain and then program it.
    IMHO checkers and then manual implementation of chess are enough.
    - "domain model" as SDF_notes.md shows is about "abstract types".
## needs knowledge *beyond* what SICP and SDF teach
- 3.22-b
  > on different terminals.
  beyond programming basic strategies.
## related with *complexity analysis* (TODO after CRLS. Others: search CRLS here.)
- 3.6-c
## ~~unrelated with programming strategies since my goal is "A strong understanding of programming" said in mit_6-046j_2005~~
## maybe need some inspiration
- 5.4
## obscure exercise description...
- 5.6
# partially done exercises
- 4.13
  - > `match:compile-pattern` used in `maybe-substitute`
- 4.17
  - not consider lazy evaluation ~~and `set-car!` etc~~.
    - But this is fine at least for the book demo since *it also doesn't consider* that.
    - "term project" may mean the above and other `set!` variants like `set-car!` etc.
  - See TODO in codes
- 4.19
  - a.
    > look for a functional solution—but don't try too hard!
## related with compiler
- better to do with some basic compiler backgrounds which includes type-inference
  - 4.14
    > make it *as general as you can*
    - I even didn't know what to do for that "general case", more specifically I didn't know what problems will have for "procedures passed as arguments and returned as values" when we assume all procedures with generic arguments ~~(so "free variables" are )~~.
    - related
      - 4.16
- 5.7 (skip most of special features in Python)
  See "0. skip "/" in parameter_list ..." in `SDF_exercises/chapter_5/5_7_naive_algorithm_for_operator_precedence_parser/5_7_precedence_lib.scm`.
  - Here the exercise intends to implement transformation but not implement many new Scheme procedures for new Scheme features.
    so 0., 0.a., 0.b., 0.c., 0.d. are all skipped where 0.b. is supported in Racket.
  - better tokenizer in SDF_exercises/chapter_5/5_7_re_lib/5_7_regexp_lib_simplified_based_on_effbot_based_on_irregex.scm or other TODOs in `grep 'TODO' ~/SICP_SDF/SDF_exercises/chapter_5/5_7*/**/*.scm -r`.
## related with complexity analysis: better done after CRLS
- 4.18
## Only give basic implementation ideas
Anyway this is enough just like pseudocode is enough for algorithm in CRLS.
- 4.25
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
  - SDF_exercises TODO when happens
  - (codes not included by the book)
  - IGNORE:
- sci-42ver/SDF_exercise_solution
- "Won't dig into" in SDF_notes.md
### temporary
- pratt_new_compatible_with_MIT_GNU_Scheme.scm
- effbot
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
# exercise solutions by *3 references* (i.e. nbardiuk etc.) and *6.945_assignment_solution* checked up to now (sometimes *code base* has sample implementations)
Just see "@% reference implementation checked".
- with check comments in codes.
- For chapter 4 later, search in this repo by `chebert*/**/*.rkt,6.945*/**/*.scm,sdf_mbillingr*/**/*.scm,sdf/**/*.scm` ~~and code base by ignoring these~~.
  Ignore `sdf-function-combinators.rkt,ps0[0-4]/,sdf/automatic-differentiation/*.scm,sdf/combining-arithmetics/*.scm` which are all codes for former chapters and `sdf/manager/*.scm` which is about software manager.
  - ~~Here `6.945*/**/*.scm` can be `6.945*/**/*.scm`~~
    We can also ignore later chapters
    `sdf/better-layering/*.scm`
    - So `sdf-function-combinators.rkt,ps0[0-4]/,sdf/automatic-differentiation/*.scm,sdf/combining-arithmetics/*.scm,sdf/manager/*.scm,sdf/better-layering/*.scm`
      - we can add some
        `sdf-function-combinators.rkt,ps0[0-4]/,sdf/automatic-differentiation/*.scm,sdf/combining-arithmetics/*.scm,sdf/manager/*.scm,sdf/better-layering/*.scm,sdf/layers/*.scm,sdf/pattern-matching-on-graphs/*.scm,sdf/propagation`
  - nbardiuk can be skipped.
- 2, 3, 4 (see "chapter 4" section where all exercises with reference search methods have been checked)
# @%exercise tests finished (obviously not considering skipped exercises)
- 2, 3, 
- 4 (4.12, 25 skipped)
  - 4.1~7, 4.8 (see SDF_exercises/chapter_4/4_8_based_on_transformation.scm), 4.9~4.11, 4.13~17, 4.19~24.
  - so all that can be done by myself have been done.
- 5 Use `grep -w -E "(test|tests|demonstrate)" -r .`
  all exercises done up to 5.6.
## no need for tests
### due to about complexity analysis
- 4.18
# TODO
- I skipped checking the detailed implementation of the following since they are less related with what the book intends to teach
  - `make-predicate-template` (*not shown in the SDF book*. There is no related funcs even by searching "template")
    So I *skipped* checking `SDF_exercises/software/sdf/user-defined-types/vector-arith.scm` (*user-defined-types/vector-arith*).
  - `define-generic-procedure-extractor` (*not shown in the SDF book*. There is no related funcs even by searching "extractor")
## @%%review of "SDF_exercises TODO" (this should be checked for each sub-dir recursively in /software/sdf/ **immediately** after finishing one sub-section)
`~/SICP_SDF/SDF_exercises$ grep -v "IGNORE\|(cph)\|SKIPPED\|(codes not included by the book)" -r ./**/*.scm | grep "SDF_exercises TODO" --color=always` (notice here the piped results have esc color codes, so "SDF_exercises TODO when happens" won't work).
- From cc09d5b919575d7a27d30d94100d2f12dd8248ef up to .
## @%review of mere TODO
`grep TODO --exclude="6.945_assignment_solution/ps[0-9]*/code/*.scm" -r ./**/*.scm | grep -v SDF_exercises | grep -v "IGNORE\|(cph)\|SKIPPED"`
up to 30b5ddceb074ac5fe8928cde0764eb91980c93d6.
1. I didn't check for md/rkt but just for my written codes (mostly are with scm suffix).
2. Here **IGNORE** means that *has been finished*, **SKIPPED** means it hasn't been finished but skipped *forever* for some reason (e.g. it is not related with what this book intends to teach like `weak-memq`) and **WAITED** means it will be done in the future.
3. See [this](https://stackoverflow.com/a/221929/21294350) for how glob is used.
4. I skipped SICP related paths using [this](https://unix.stackexchange.com/a/493909/568529)
  `find . \( -type d \( -path ./CS_61A_lab -o -path ./exercise_codes -o -path ./lecs \) -prune \) -o -type f -exec 'grep TODO --exclude="6.945_assignment_solution/ps[0-9]*/code/*.scm" | grep -v SDF_exercises | grep -v "IGNORE\|(cph)\|SKIPPED"' {} +`
  - ~~Use [this](https://stackoverflow.com/a/60425338/21294350) to allow pipe~~
    `find . \( -type d \( -path ./CS_61A_lab -o -path ./exercise_codes -o -path ./lecs \) -prune \) -o -type f 2>&1 | grep TODO --exclude="6.945_assignment_solution/ps[0-9]*/code/*.scm" | grep -v SDF_exercises | grep -v "IGNORE\|(cph)\|SKIPPED"`
  - Use this to allow pipe
    `find . \( -type d \( -path ./CS_61A_lab -o -path ./exercise_codes -o -path ./lecs \) -prune \) -o -type f -printf '"%p" ' | grep TODO --exclude="6.945_assignment_solution/ps[0-9]*/code/*.scm" | grep -v SDF_exercises | grep -v "IGNORE\|(cph)\|SKIPPED"`
    - *better* to [allow filenames with spaces](https://unix.stackexchange.com/a/658969/568529)
      `find . \( -type d \( -path ./.git -o -path ./CS_61A_lab -o -path ./exercise_codes -o -path ./lecs \) -prune \) -o -type f -print0 | xargs -0 grep TODO --exclude={"6.945_assignment_solution/ps[0-9]*/code/*.scm",\*.{md,rkt,sample}} --color=always | grep -v "SDF_exercises TODO" | grep -v "IGNORE\|(cph)\|SKIPPED"` (notice not to use the mere `SDF_exercises` since for grep'ing many files file path is also included in the output)
      - I also excludes `./.git` and `\*.{md,rkt,sample}`.
      - https://unix.stackexchange.com/questions/493723/grep-exclude-dir-dont-work/493909?noredirect=1#comment1512591_493909
        `find . \( -type d \( -path ./.git -o -path ./CS_61A_lab -o -path ./exercise_codes -o -path ./lecs \) -prune \) -o -type f -exec sh -c 'grep TODO --exclude={"6.945_assignment_solution/ps[0-9]*/code/*.scm",\*.{md,rkt,sample}} -v "\(SDF_exercises TODO\)\|IGNORE\|(cph)\|SKIPPED" "$@" --color=always' sh {} +` (This won't work. Here I want to keep both color and pipe. But that may be in conflict). Use `find . \( -type d \( -path ./.git -o -path ./CS_61A_lab -o -path ./exercise_codes -o -path ./lecs \) -prune \) -o -type f -exec sh -c 'grep -v "\(SDF_exercises TODO\)\|IGNORE\|(cph)\|SKIPPED" --exclude={"6.945_assignment_solution/ps[0-9]*/code/*.scm",\*.{md,rkt,sample}} "$@" | grep TODO --color=always' sh {} +` (not use `find . \( -type d \( -path ./.git -o -path ./CS_61A_lab -o -path ./exercise_codes -o -path ./lecs \) -prune \) -o -type f -exec sh -c 'grep TODO --exclude={"6.945_assignment_solution/ps[0-9]*/code/*.scm",\*.{md,rkt,sample}} "$@" | grep -v "\(SDF_exercises TODO\)\|IGNORE\|(cph)\|SKIPPED" --color=always' sh {} +`)
        - [see](https://stackoverflow.com/a/66061678/21294350) (notice `||` is used there for shor circuit so that "no issue" will return true without checking the latter and "issue" will check the latter. Using && will reject "no issue") and [this for `!`](https://superuser.com/a/397325/1658455) (`{}` [won't work for `-name`](https://unix.stackexchange.com/a/351167/568529) for name)
          Here I skipped all sub-files in paths `SDF_exercises/chapter_*` since they are in homework which TODO won't be probably helped by reading the book further.
          Here `*` can match dir prefix like `./fubar3` in `man`.
          `find . \( -type d \( -path ./SDF_exercises/chapter_\* -o -path \*/.git -o -path ./CS_61A_lab -o -path ./exercise_codes -o -path ./lecs \) -prune \) -o \( -type f \( ! -path "./6.945_assignment_solution/ps[0-9]*/code/*.scm" -a ! -name "*.md" -a ! -name "*.rkt" -a ! -name "*.sample" \) \) -exec awk '/TODO/ && !/SDF_exercises TODO/ && !/IGNORE/ && !/IGNORE/ && !/\(cph\)/ && !/SKIPPED/ {match($0,/TODO/); printf "\033[1;31m" FILENAME "\033[0m: " substr($0,1,RSTART-1) "\033[1;31m" substr($0,RSTART,RLENGTH) "\033[0m: " substr($0,RSTART+RLENGTH) "\n"}' {} +`
          - we need [explicit print](https://unix.stackexchange.com/q/658029/568529)
          - [`match`](https://unix.stackexchange.com/a/193254/568529)
          ```bash
          $ find . \
            \( -type d \( -path ./SDF_exercises/chapter_\* \
              -o -path \*/.git \
              -o -path ./CS_61A_lab -o -path ./exercise_codes -o -path ./lecs \) -prune \) -o \
            \( -type f \( ! -path "./6.945_assignment_solution/ps[0-9]*/code/*.scm" \
              -a ! -name "*.md" -a ! -name "*.rkt" -a ! -name "*.sample" \) \) \
            -exec awk '/TODO/ && !/SDF_exercises TODO/ && !/IGNORE/ && !/\(cph\)/ && !/SKIPPED/ && !/code_base TODO/ {match($0,/TODO/); printf "\033[1;31m" FILENAME "\033[0m: " substr($0,1,RSTART-1) "\033[1;31m" substr($0,RSTART,RLENGTH) "\033[0m: " substr($0,RSTART+RLENGTH) "\n"}' {} +
          ```
          - most of codes in the code base uses "TODO:" instead of only "TODO".
          - **Use [this](https://unix.stackexchange.com/a/788823/568529) after many improvements** (use `gsub` as Stéphane Chazelas shows)
            I skipped SDF exercise codes, SICP codes, *.rkt, 6.945_assignment_solution code base codes (by `-prune`),
            and skipped unrelated suffix files like `*.md`.
            Here "code_base TODO"/"cph" means TODO originally in the code base.
            ```bash
            [~/SICP_SDF]$ find . \
            \( -type d \( -path ./SDF_exercises/chapter_\* \
              -o -path \*/.git \
              -o -path ./CS_61A_lab -o -path ./exercise_codes -o -path ./lecs \) -prune \) -o \
            \( -type f \( ! -path "./6.945_assignment_solution/ps[0-9]*/code/*.scm" \
              -a ! -name "*.md" -a ! -name "*.rkt" -a ! -name "*.sample" \) \) \
            -exec awk '!/SDF_exercises TODO/ && !/IGNORE/ && !/\(cph\)/ && !/SKIPPED/ && !/code_base TODO/ && gsub(/TODO/,"\033[1;31m" "&" "\033[0m") {printf "\033[1;31m" FILENAME "\033[0m: " $0 "\n"}' {} +
            ```
            - [perl](https://unix.stackexchange.com/a/788821/568529)
              - [`-p`](https://learnbyexample.github.io/learn_perl_oneliners/one-liner-introduction.html#substitution)
              - test:
                `echo "TODO foo\nTODO TODO" | perl -pe 's/TODO(?! foo)/\e[31;1m$&\e[m/g'`
                `echo "TODO foo\nTODO TODO" | perl -pe 's/TODO/\e[31;1m$&\e[m/g unless /TODO foo/'`
              - > To discard all the lines that contain foo bar and all that don't contain foo and highlight foo in the remaining ones:
                compared with
                > To discard the lines that contain foo bar and highlight occurrences of foo in remaining lines, you could do:
                - `awk` automatically print, so same.
                - TODO IMHO `-e '^'` is redundant.
              - https://unix.stackexchange.com/questions/788816/how-to-highlight-the-matched-regex-pattern-got-by-many-regex-exps-disjoined-with/788821?noredirect=1#comment1512739_788821
                - `printf '%s\n' foo 'foo bar' 'foo foo bar' 'bar foo' bar | perl -ne 'print if ! /foo bar/ && s/foo/\e[1;31m$&\e[m/g'`
                  or `printf '%s\n' foo 'foo bar' 'foo foo bar' 'bar foo' bar | xargs -I{} echo {}`
                - ~~Here `?!` may consume some string~~
                - `printf '%s\n' foo 'foo bar' 'foo foo bar' 'bar foo' bar | grep -v 'foo bar' | grep --color -e '^' -e foo`
                  Here `-e '^'` ~~may not~~ won't be colored since it matches with the starting *empty string*. It just keeps something like `bar`.
                - `printf '%s\n' foo 'foo bar' 'foo foo bar' 'bar foo' bar | grep -P --color 'foo(?! bar)' -`
                  won't remove "foo foo bar" since it has some *sub-string* matched although also with some *sub-string* un-matched.
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
  - Emm... I have forgotten what I meant when I reviewed this while reading chapter4...
## lack
# MIT/GNU Scheme internal func description
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
## chapter 4
1: search `algebra-2` (no codes with explanation)
2: simplification
3: `sort`
4: `algebra-3`
5: `print-all-matches` (just codes in `SICP_SDF/SDF_exercises/software/sdf/design-of-the-matcher/text-examples.scm` without explanation)
6: ?:choice
- 7,9: ?:pletrec
  - See in SDF_notes.md
    > The above is different from SICP 4.79...
    Here binding is just var->value where value *can't be also var*. So we can just use the normal env mechanism.
  - Here the former 3 are related with `define-syntax`. The last is said in the above quote context.
    ```bash
    # https://unix.stackexchange.com/a/721889/568529
    $ grep "env" -r ~/SICP_SDF/SDF_exercises/software/sdf/(term-rewriting|unification) --files-with-matches | grep -oP "(?<=$HOME/).*"
    SICP_SDF/SDF_exercises/software/sdf/term-rewriting/rule-implementation.scm
    SICP_SDF/SDF_exercises/software/sdf/term-rewriting/test-rule-implementation.scm
    SICP_SDF/SDF_exercises/software/sdf/term-rewriting/rule-utils.scm
    SICP_SDF/SDF_exercises/software/sdf/unification/type-resolver.scm
    ```
8: match:vector (assuming following the naming convention)
10: `match-args vector?` (no use: SICP_SDF/SDF_exercises/software/sdf/combining-arithmetics/vector-arith.scm and SICP_SDF/SDF_exercises/software/sdf/common/pretty-printer.scm)
- 11,21: `string?` (all sdf/common/ files are no use.)
  `grep "string" chebert*/**/*.rkt 6.945*/**/*.scm sdf_mbillingr*/**/*.scm SDF_exercises/software/sdf/**/*.scm | awk -F ':' '{print $1}' | sort -u | xargs -I%% sh -c 'if [[ OUTPUT=$(grep "??" %% --color) ]];then echo "in file:%%" "$OUTPUT";fi' sh` is *wrong* since `OUTPUT=...` returns 0 if success instead of `$OUTPUT`.
  - `grep "string" chebert*/**/*.rkt 6.945*/**/*.scm sdf_mbillingr*/**/*.scm SDF_exercises/software/sdf/**/*.scm | awk -F ':' '{print $1}' | sort -u | xargs -I%% sh -c 'OUTPUT=$(grep "??" %% --color);if [[ 0 -eq $? ]];then echo "in file:%%" "$OUTPUT";fi' sh` is right to use `$?` to check.
    - Actually that is unnecessary. See [reference](https://unix.stackexchange.com/a/373334/568529)
      So use `grep "string" chebert*/**/*.rkt 6.945*/**/*.scm sdf_mbillingr*/**/*.scm SDF_exercises/software/sdf/**/*.scm | awk -F ':' '{print $1}' | sort -u | xargs -I%% sh -c 'if OUTPUT=$(grep "??" %% --color=always);then echo "in file:%%" "$OUTPUT";fi' sh`
      - In results, only one having relation with "unification".
    - or with one procedure
      ```bash
      FILES="chebert*/*.rkt 6.945*/**/*.scm sdf_mbillingr*/**/*.scm SDF_exercises/software/sdf/**/*.scm"
          ## find can't recognize ** for empty string which is different from VSCode.
          # for j in $FILES ; do           # glob, matches in current dir!
          #   printf "%s:\n%s\n\n" $j $(find . -path $j) ; done
          ## ls can't parse str...
          ## and it also returns one string instead of list
          # for j in $(ls $FILES) ; do           # glob, matches in current dir!
          #   printf "%s\n" $j ; done
          ## 0. https://stackoverflow.com/a/918931/21294350
          # Here I use zsh https://stackoverflow.com/a/36476068/21294350 see `man zshbuiltins`
          # use declare to check the result https://stackoverflow.com/a/10527046/21294350
          ## 1. IGNORE: Here find can't use glob at all
          # IFS=' ' read -rA ADDR <<< "$FILES"
          # for FILE_STR in ${ADDR[@]}; do           # glob, matches in current dir!
          #   printf "FILE_STR: %s\n" $FILE_STR
          #   for FILE in $(find . -path "$FILE_STR"); do
          #     printf "%s\n" $FILE ; done
          #   ; done
          ## https://unix.stackexchange.com/a/34012/568529
      FILES=(chebert*/*.rkt 6.945*/**/*.scm sdf_mbillingr*/**/*.scm SDF_exercises/software/sdf/**/*.scm)
          ## grep can't accept many files at once. See https://unix.stackexchange.com/q/788987/568529
          # grep "string" $FILES | awk -F ':' '{print $1}' | sort -u | xargs -I%% sh -c 'if OUTPUT=$(grep "??" %% --color=always);then echo "in file:%%" "$OUTPUT";fi' sh
      grep_seq () {
        # https://stackoverflow.com/a/42319729/21294350
        # https://unix.stackexchange.com/q/788987/568529
        # SUBFILES=$(echo $FILES | tr ' ' '\n')
        ## man zshexpn
        ## > "${array[@]}" or "${(@)array}" for arrays
        SUBFILES=("$FILES[@]")
        # SUBFILES=("${FILES}") # this will become one str
        ## explicitly split
        # SUBFILES=("${(s: :)FILES}")
        # SUBFILES=("${=FILES}")

        # https://stackoverflow.com/a/22432604/21294350
        # we can also avoid using i https://unix.stackexchange.com/a/278503/568529
        for ((idx=1;idx<=$#;idx++)); do
          # https://stackoverflow.com/a/10750038/21294350 https://stackoverflow.com/a/8515492/21294350
          # for zsh https://unix.stackexchange.com/a/119442/568529
          pat="${(P)idx}"
          ## https://unix.stackexchange.com/a/742010/568529 and google AI says $# may be not one number
          ## So ($#-1) may be weird
          # echo "pat:" $pat "param_num" $# "param_num-1:" $(( ($#-1) )) $(( 1==($#-1) ))
          declare -p pat
          declare -p SUBFILES

          # if (( ($idx+1)==$# ));then
          if (( $idx==$# ));then
            grep $pat $SUBFILES --color=always;
          else
            # consider possible space in pathname
            local TMP="$(grep $pat $SUBFILES | awk -F ':' '{print $1}' | sort -u)"
            declare -p TMP
            SUBFILES=("${(ps:\n:)TMP}");
          fi
        ;done
      }
      grep_seq "string" "??" "string"
      ```
      - https://unix.stackexchange.com/a/350012/568529
        Here we can either use nameref for reference.
        - `declare -a arr0=("'1 2 3'" "'4 5 6'")` -> `declare -p arr0` with `typeset -a arr0=( \''1 2 3'\' \''4 5 6'\' )`: [`\'` meaning](https://askubuntu.com/a/605440) and see `info bash` ANSI-C Quoting.
          - similarly `'"$2"'=()` is to end quote first and get `$2` expanded.
        - `man zshparam`
          > To reference the value of a parameter, write ‘$name' or ‘${name}'
          so $array[@] or ${array[@]} are both ok for zsh.
          - see `man zshexpn`
            > If name is an array parameter...
            so $array is also fine if not with qoute.
            > No field splitting is done on the result unless the SH_WORD_SPLIT option is set
            so "$array" won't work.
      - better to [quote variable expansion](https://stackoverflow.com/a/10067297/21294350)
      - https://unix.stackexchange.com/a/788995/568529
        - `man ZSHPARAM`
          > In scalar assignment, value is expanded as a single string, in which the elements of arrays are joined together
        - > the assignment creating new, contiguous ones starting from 0
          this is not that case for `zsh`
          - For bash, see `info bash`
            > the index of the element assigned is the last index assigned to by the statement plus one.  Indexing starts at zero.
        - export attribute
          See https://unix.stackexchange.com/a/522379/568529 and https://stackoverflow.com/q/53364895/21294350
        - [@A](https://stackoverflow.com/q/65233512/21294350)
        - [`declare --`](https://unix.stackexchange.com/questions/510220/what-is-declare-in-bash#comment1513138_510342) in `declare -p d_repr`.
        - `[*]` diff from `[@]` (`man zshparam`)
          > ‘"$foo[*]"' eval‐uates to ‘"$foo[1] $foo[2] ..."', whereas ‘"$foo[@]"' evaluates to ‘"$foo[1]" "$foo[2]" ...'
          Due to join, `b="${a[*]}"` and `b="${a[@]}"` are same.
  - notice [what is considered as false](https://stackoverflow.com/a/2933877/21294350) in shell (also see man "a status of  0  (true)  if  successful...").
  - what `...=$(...)` [returns](https://unix.stackexchange.com/a/270831/568529)
    - Here no command name like $0, i.e. /bin/zsh etc.
    - so `...=$(...2)` returns same as `...2`.
  - TODO I don't know why [group command can't be directly used for `xargs`](https://stackoverflow.com/a/6958957/21294350)
- 12: `do-substitute`
13: Emm... following the similar naming as `match:compile-pattern`, we search `unify:compile-pattern`. But this naming convention is not ensured to be used by others.
14: `infer-program-types` then `procedure` (only 1 original implementation file and one test file in code base)
15: `parametric-type` using the book naming convention (then search for `map` there)
16: `(define-parametric-type-operator 'type:union)`
17: `set!-expr?`/`set-expr?` similar to `if-expr?`
18: practical
19: `substitution-instance?`
20: `((?? x) 3)` (with tests in `SDF_exercises/software/sdf/unification/gjs-test.scm` but not reference implementation)
21: see the above
22: `tree->lazy-graph` based on the code base naming conventions.
23: `all-rook-moves`
24: `simple-move` (SICP_SDF/SDF_exercises/software/sdf/abstracting-a-domain can be ignored)
25: `gmatch`
### comments (see the above)
- Here better to do 4.4.3. exercises after learning compiler...
- [x] 1
  - same as SICP `(married Mickey ?who)`
- [x] 2
  - a. same as 1.
  - ~~b. This seems to assume ~~
- [ ] 7,9
  - Here we have no something like `begin` in pattern language, so val is assumed to just len-1.
    And also for let-body.
- [x] 8
  - SDF_exercises/chapter_4/4_8.scm
    is *all* based on generic, but that need to change all the rest matcher API since `data-list` can be vector or "arbitrary sequence".
    I won't spend time to do that *routine* work.
  - SDF_exercises/chapter_4/4_8_based_on_transformation.scm
    is just transforming vector or "arbitrary sequence" to list and *reuse* the original API.
    That is elegant although this transformation may fail for some corner case.
    - I give test for nested seq with vector and list (maybe arbitrary).
      ~~I don't know how to .~~
- [ ] 12
  - ~~Emm... I think no problems exist here at all.~~
- [ ] 13
  - SDF_exercises/chapter_4/4_13.scm is *actually not what the exercise expects* except for `match:compile-pattern` used in `maybe-substitute`.
    Here the generic overhead is avoided trivially but we still checks `car-satisfies` which can't be avoided since the match depends on *types of both sides*. This is why we need "`match:compile-pattern` used in `maybe-substitute`".
    - Maybe it is *worth with this overhead* since
      > to allow easy extension for new language features
    - IMHO the unification underlying idea "*both* sides" implies we should not use a "match procedure" based on "combinators".
    - > This is hard!
      As the above shows, following the code base original ideas can't do that.
      So we may need some *extra* ingenious ideas. I won't do that since this should be taught in CRLS. See "Won't dig into" in SDF_notes.md
- [ ] 14
  - ~~IMHO this is much harder than latter exercises in this section 4.4.3.~~
  - The "specific problem" solution should [just use *generic* type](https://cs3110.github.io/textbook/chapters/interp/inference.html#finishing-type-inference) (i.e. `type-variable` IMHO).
    > the principal type of the identity function fun x -> x would be 'a -> 'a ... the most “lenient”
  - My original thoughts may be not what the author wants to achieve in this exercise.
    I tried to derive procedure argument by the body which can be done in the original codes.
    Then this implies we should not use other types of arg.
    So we should check `procedure-type-domains` when unify.
    - But notice if we allow "*generic* type" for input args, then obviously we can't detect errors for input arg...
      This seems one contradiction...
    - IMHO this is more appropriate to be done in [Type-checking](https://en.wikipedia.org/wiki/Type_inference#Type-checking_vs._type-inference) since this is one *procedure application error* instead of inference error.
      TODO after compiler
  - If just allow all procedure having general argument types, then
    > procedures passed as arguments
    is just allowed forever since with no checking for that.
    - > returned as values? ... there may be free variables in a procedure that are lexically bound
      Here "free variables" only matters for inference when it is used as arg so we look up (see SICP exercise_codes/SICP/book-codes/ch4-mceval.scm), (If just return, then ~~the modification in `SDF_exercises/chapter_4/4_14.scm` doesn't~~ the original one just works. See `test3`) so they are ignored similarly.
      Anyway, these "free variables" defined in procedure can be found with the correct source due to `env` mechanism.
- [ ] 15
  - ~~TODO union for `(list 1 #f)` etc.~~
- [ ] 16
  - a bit hard related with 4.14.
    Also I simplify this problems based on some assumption (see codes "assume" contexts).
- [ ] 17
  - ~~TODO see md~~
    - [non-expansive expression](https://homepages.inf.ed.ac.uk/stg/NOTES/node90.html)
### other possible type-inference extension
- [subtype etc](https://nus-cs2030s.github.io/2425-s1/27-inference.html#argument-typing)
#### TODO why I wrote this subsection?
### TODO
- ~~7,9 (Unification match may use env to implement)~~
  7 will be done implicitly when 9 is done.
  9 is ~~similar to SICP 4.79 which needs implement rule based on *unification* with env~~.
- ~~11 to be done with 21.~~
- ~~See above TODO.~~
- ~~23 TODOs after all related graph implementation understanding.~~
  - ~~24 similarly needs some graph implementations, predicate implementations more specifically.~~
    But this predicate should be done in `match`... So this maybe have been implemented in SDF_exercises/software/sdf/pattern-matching-on-graphs/graph-match.scm.
  1. ~~SDF_exercises/chapter_4/4_23_graph_match_lib/check_lib.scm checked_positions update~~
  2. (I forgot what I meant by this line. After all, 4.23 pred implementation with tests has been finished.) SDF_exercises/chapter_4/4_23_graph_match_lib/castling_lib.scm after check_lib.scm
  3. SDF_exercises/chapter_4/4_23_graph_match_lib/simple_move_mod.scm combining all other `simple-move` implementations.
## @%chapter 5
### @%keywords to search for reference implementation
notice only doc'ed after *having checked* the reference implementation
- 5.1 `(+ (f 3) (* 4 5))`
- 5.2 `procedure-parameters`
- 3 `(lambda (procedure operands calling-environment)`
- 4 "fun "
- 5 `(map (lambda (x) (* x x)) '(1 2 3))`
- 6 mdl ?
### comments
- 5
  > However, primitives that take procedures as arguments, such as map or filter, will not accept nonprimitive procedures (i.e., those created by this interpreter from lambda expressions).
  see SCIP Exercise 4.14.
  IMHO just similar, due to the "created" "nonprimitive procedures" are by `make-compound-procedure` unrecognized by the "Scheme primitive".
### @%TODO
## reference implementation checked
chapter 2~3 seems to not use the regex shown in "@exercise solutions..." (I forgot). 
- For chapter 4, 4.1~25 have been checked with 
  include: `chebert*/**/*.rkt,6.945*/*/*.scm,sdf_mbillingr*/**/*.scm,sdf/**/*.scm` (`6.945*/*/*.scm` is to skip all lib files in something like `6.945_assignment_solution/ps06/code/*.scm`)
  - exclude 
    - due to inside former chapters: `sdf-function-combinators.rkt,ps0[0-4]/,sdf/automatic-differentiation/*.scm`
    - due to being not related with what this book intends to teach: `sdf/manager/*.scm`
    - due to inside *later* chapters: `sdf/better-layering/*.scm,sdf/layers/*.scm,sdf/propagation,sdf/pattern-matching-on-graphs/*.scm`
    - later, exclude remove `sdf/pattern-matching-on-graphs/*.scm`
    - For section 5.1, we add `sdf/compiling-to-execution-procedures,sdf/exploratory-behavior,sdf/interpreter-with-continuations,sdf/non-strict-arguments`
      - So temporarily use `sdf-function-combinators.rkt,ps0[0-4]/,sdf/automatic-differentiation/*.scm,sdf/manager/*.scm,sdf/better-layering/*.scm,sdf/layers/*.scm,sdf/propagation,sdf/compiling-to-execution-procedures,sdf/exploratory-behavior,sdf/interpreter-with-continuations,sdf/non-strict-arguments`
  - implicitly exclude
    `6.945_assignment_solution/ps*,SICP/,lecs,chapter_*`
## related helper files describing useful things
- `sc-macro-transformer`
  - capture-syntactic-environment.scm
# TODO after CRLS
## related with data structure
- See priority queue in 4.25

[POSIX_regex]:https://pubs.opengroup.org/onlinepubs/9699919799/nframe.html
[software-manager-doc]:https://groups.csail.mit.edu/mac/users/gjs/6.945/sdf/manager/software-manager.pdf