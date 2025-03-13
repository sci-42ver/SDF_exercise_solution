Re to 2: what is "the original $x"? With "dynamic scoping only ever applies to package vars", I can understand why `$::x` for "localisation" has "1 .. 3". But `foo` is still complex, which may need another one new question post... For "while for lexicals, foo() is bound to the 1st lexical at compile time", do you mean we first try compile time search, if failure (i.e. the above code snippet 2 and 3) we try runtime search which all  refer to the package variable for code snippet 2 and 3?

Re to 3: do you mean our uses dynamic scope while my uses lexical scope, so the former can be searched by `foo` "symbol table lookup" while the latter can't?

Re to 2: Thanks. For "I just meant the lexical $x whose scope foo was in.", later I found the doc https://perldoc.perl.org/perlsub#Private-Variables-via-my() with one similar example saying "presumably file scope." For "it binds to the glob", what do you mean glob here? In bash, it can be something with "*", how can that exist in the variable value? Continued...

For "but that only changes which instance of a particular lexical in the source is bound.", what do you mean by "instance"? For example, `my $x = 10;sub bumpx { print $x++ };$x = 20;bumpx();` gives 20 while `my $x = 10;sub bumpx { print $x++ };my $x = 20;bumpx();` gives 10. The latter `bumpx` can't access the newly created variable (IMHO that means instance).

Re to the new 1st and 3rd comment: "I guess a better statement would be perl doesn't do dynamic scoping at all": But https://perldoc.perl.org/perlsub#Temporary-Values-via-local() shows "This is known as dynamic scoping." for `local`. "it all happens in the scope of the localization, not in any called code elsewhere.": The former doc link also says "local is mostly used when the current value of a variable must be *visible to called subroutines*." which is just what dynamic scoping means. Continued...

Could you say more about "perl's local fakes it" (better with one example showing its difference from dynamic scoping)?

:57947660 So it means we have "multiple instances" (variables) all pointing to the *changing* `$_` where `{ my $x = $_; \$x }` corresponds to the above `sub ...`.

:57947674 So "multiple instances" just means the same `my$x` etc called in *different calls*. So it depends on runtime. But "a particular lexical in the source" is same since the program is *unchanged*. Is my rephrasing based on understanding right?

:57947587 "localisation as creating a new lexical variable ... dynamic scoping only ever applies to package vars": So can I understand that localisation (e.g. `for $x (1 .. 3) {...}`) can only applies to package vars?

Continued... But that can't explain for code block2 in question code snippet 2.

:57948060 "every lexical is allocated at compile time, then at scope exit, if there are any references to it remaining, a new one is created and put in place of the old one, and the old one lives a separate life.": my understanding for `$$_ for map { my $x = $_; \$x } 1..3` is here $_ is that lexical which is referred to with `\$x` later by `$$_` after "scope exit". Continued...

But it seems weird that "a new" `$_` is created pointing to 2 for example and then "put in place of the old one" pointing to 1. That seems to be implied by map instead of the reference `\$x`. Could you clarify based on one example shown above by you?

:57948075 Clear explanation based on the reference. Then for code snippet 3, with `our $x = 7;` after `my $x = 7;`, "Use of our creates a lexically scoped alias to the package variable for the scope of the our" implies `for $x (1 .. 3)` will use the *most recent* declaration, i.e. `our` which points to package variable here. Is it that case?
# [QA1](https://stackoverflow.com/q/79492046/21294350)
3 files "my_in_for_non_C.perl  my_in_for.perl  my_vs_local.perl".
- [`::` qualified](https://www.perlmonks.org/?node_id=1143881#:~:text=Replies%20are%20listed%20'Best%20First,'@::foo'?&text=Hello%20jmeek%2C%20and%20welcome%20to%20the%20Monastery!&text=In%20all%20cases%2C%20x%20is,preventing%20errors%20from%20use%20strict.)&text=Re%5E2:%20What%20does%20','@::foo'?) seems to like Cpp namespace.
  - Also see https://stackoverflow.com/a/20992896/21294350
- [`SV`](https://perldoc.perl.org/perlguts)
- [`\`](https://stackoverflow.com/a/4173751/21294350)
## skipped
- the [historic behavior](https://stackoverflow.com/a/61026549/21294350) of `my $_`

# maybe ~~TODO~~ perl scoping is so complicated, e.g. [this](https://stackoverflow.com/q/79495318/21294350).
## miscs
- https://chat.stackoverflow.com/transcript/message/57947660#57947660
  [`$$`](https://stackoverflow.com/a/28936629/21294350)
## explanation for scoping
- Here all variables `$x` after `for` points to the outside variable due to localisation and that `my` creates one *new* variable.
  So we only need to care about 2 `print "global $::x\n";`s and `foo();`s.
