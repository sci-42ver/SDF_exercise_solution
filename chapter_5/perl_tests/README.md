:57954361 "I found your current rephrasing confusing": Maybe using codes can convey what I meant better. What I meant is `say $$_ for map { my $x = $_; \$x } 1..3;` has almost the same effects as `my $x = 1; $x_ref1 = \$x; my $x = 2; $x_ref2 = \$x; my $x = 3; $x_ref3 = \$x; map {say $$_ } ($x_ref1, $x_ref2, $x_ref3);`. Here "put in place of the old one" is implicitly shown by = here although being one unidirectional assignment to $x_ref1 instead of $x. Continued...

`use feature qw/say/;{my $x = 1; our $x_ref1 = \$x;} {my $x = 2; our $x_ref2 = \$x;} {my $x = 3; our $x_ref3 = \$x;} map {say $$_ } ($x_ref1, $x_ref2, $x_ref3, $x);` is finer. Then "at scope exit, if there are any references to it remaining, a new one is created and put in place of the old one" is implicitly shown by `our $x_ref1 = \$x;` etc although being one unidirectional assignment to $x_ref1 instead of $x. Continued...

"the old one lives a separate life." is implied by that inner `my $x ...` won't have influences for the outer `map {say $$_ } (... $x)`. This is ensured by lexical scoping. Is my current rephrasing fine?
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
