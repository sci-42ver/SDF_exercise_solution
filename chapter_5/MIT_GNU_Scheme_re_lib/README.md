files chibi and srfi are from SRFI https://srfi.schemers.org/srfi-115/srfi-115.html#Implementation,  which works in chibi-scheme by `(import (srfi 115))`. But MIT/GNU Scheme doesn't support import implied by not supporting `(import (scheme base))` shown in R7RS.

But chibi-scheme's APIs differ a lot from MIT/GNU Scheme, so that it can't directly use the code base...

---

[irregex-0.9.11](https://synthcode.com/scheme/irregex/#h4_(irregex-search%3Cirx%3E%3Cstr%3E[%3Cstart%3E%3Cend%3E])) works.

# miscs
- [DFA for look-around](https://stackoverflow.com/a/2999058/21294350)
# TODO
- > Thompson-style non-backtracking NFA
  why Thompson-style NFA doesn't need "backtrack"
  - maybe related
    - https://stackoverflow.com/a/11255218/21294350
    - https://stackoverflow.com/questions/8132412/which-regular-expression-requires-backtracking#comment9978957_8132412 https://web.archive.org/web/20080801000000*/http://oreilly.com/catalog/regex/chapter/ch04.html
    - https://stackoverflow.com/a/49517297/21294350
