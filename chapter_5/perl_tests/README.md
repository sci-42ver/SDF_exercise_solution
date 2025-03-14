:57950425 OK. It is my misunderstanding that the global variable `$_` is one lexical and can have multiple instances... So for the same example, here `\$x` is passed outside after that lexical variable has finished its scope and should be destroyed by something like garbage collector. But we *still* needs that variable. So "a new one is created and put in place of the old one" and the reference *seems to* work as before but actually points to one different object. Continued...

"the old one lives a separate life" means lexical scope, i.e. it can't be accessed outside scope. Is my current rephrasing fine?

:57948075 "Yes, the for $x should use the lexical our $x": So can we tweak the perlsyn reference to "If the variable was previously declared with *our most recently*, it *still* uses the *global* variable"?

We also need to change group as https://askubuntu.com/a/34075/2168145 shows.
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
