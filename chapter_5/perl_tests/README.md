Re to 2: what is "the original $x"? With "dynamic scoping only ever applies to package vars", I can understand why `$::x` for "localisation" has "1 .. 3". But `foo` is still complex, which may need another one new question post... For "while for lexicals, foo() is bound to the 1st lexical at compile time", do you mean we first try compile time search, if failure (i.e. the above code snippet 2 and 3) we try runtime search which all  refer to the package variable for code snippet 2 and 3?

Re to 3: do you mean our uses dynamic scope while my uses lexical scope, so the former can be searched by `foo` "symbol table lookup" while the latter can't?
# [QA1](https://stackoverflow.com/q/79492046/21294350)
3 files "my_in_for_non_C.perl  my_in_for.perl  my_vs_local.perl".
- [`::` qualified](https://www.perlmonks.org/?node_id=1143881#:~:text=Replies%20are%20listed%20'Best%20First,'@::foo'?&text=Hello%20jmeek%2C%20and%20welcome%20to%20the%20Monastery!&text=In%20all%20cases%2C%20x%20is,preventing%20errors%20from%20use%20strict.)&text=Re%5E2:%20What%20does%20','@::foo'?) seems to like Cpp namespace.
  - Also see https://stackoverflow.com/a/20992896/21294350
- [`SV`](https://perldoc.perl.org/perlguts)
- [`\`](https://stackoverflow.com/a/4173751/21294350)
## skipped
- the [historic behavior](https://stackoverflow.com/a/61026549/21294350) of `my $_`

# maybe TODO perl scoping is so complicated, e.g. [this](https://stackoverflow.com/q/79495318/21294350).
