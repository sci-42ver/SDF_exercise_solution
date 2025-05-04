Here I use camel case naming same as oilshell Python class implementation to show their correspondence. Similarly I use this convention for all files here with consistency.

Python naming convention [see](https://stackoverflow.com/a/42127721/21294350). Oilshell has some exceptions:
- > Function names should be lowercase, with words separated by underscores as necessary to improve readability.
  > Variable names follow the same convention as function names.
  It uses `LeftComma` etc.

# TODO
- ~~"TODO tests" etc.~~
  - see ModularPratt.scm and test/parse_tests.scm for corrections.
# Notice
- Scheme recognizes `'N` and `'n` as the same thing, so better to use names without the conflict if all transformed into lowercase.
