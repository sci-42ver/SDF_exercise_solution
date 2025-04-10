files chibi and srfi are from SRFI https://srfi.schemers.org/srfi-115/srfi-115.html#Implementation,  which works in chibi-scheme by `(import (srfi 115))`. But MIT/GNU Scheme doesn't support import implied by not supporting `(import (scheme base))` shown in R7RS.

But chibi-scheme's APIs differ a lot from MIT/GNU Scheme, so that it can't directly use the code base...

---

[irregex-0.9.11](https://synthcode.com/scheme/irregex/#h4_(irregex-search%3Cirx%3E%3Cstr%3E[%3Cstart%3E%3Cend%3E])) works.

# miscs
- [DFA for look-around](https://stackoverflow.com/a/2999058/21294350)
- > Thompson-style non-backtracking NFA
  why Thompson-style NFA doesn't need "backtrack"
  - maybe related
    - https://stackoverflow.com/a/11255218/21294350
    - https://stackoverflow.com/questions/8132412/which-regular-expression-requires-backtracking#comment9978957_8132412 https://web.archive.org/web/20080801000000*/http://oreilly.com/catalog/regex/chapter/ch04.html
    - https://stackoverflow.com/a/49517297/21294350
  - Said in https://swtch.com/~rsc/regexp/regexp1.html (also see https://cs.stackexchange.com/q/171696/161388)
    - > In the worst case, the NFA might be in *every state* at *each step*, but this results in at worst a *constant* amount of work independent of the length of the string, so arbitrarily large input strings can be processed in linear time.
      So step is at most $n$, and state is at most $k$ which is constant as the number of all possible characters.
    - > This is a dramatic improvement over the *exponential* time required by the backtracking approach. The efficiency comes from tracking the set of reachable states but not which paths were used to reach them. In an NFA with n nodes, there can only be n reachable states at any step, but there might be *$2^n$* paths through the NFA.
      TODO IMHO n [nodes](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton) implies *at most* $n-1$ reachable states.
      > (The run time is superlinear, because we are not keeping the regular expression constant as the input grows. For a regular expression of length m run on text of length n, the Thompson NFA requires O(mn) time.)
# TODO
- IrRegular meaning in IrRegular Expressions.
