In newer python, we need to use `\\d` instead of `\d` https://stackoverflow.com/a/52335971/21294350.

"then you have to renumber your captures": What do you mean by that? With `?:` removed, `m.group(1)` and `m.group(0)` still work as https://docs.python.org/3/library/re.html#re.Match.group implies since the outer parenthesis is not influenced. IMHO here `?:` is just to avoid referring to unrelated things like `m.group(2)` etc as doc https://docs.python.org/3/library/re.html#regular-expression-syntax says "the substring matched by the group cannot be retrieved after performing a match".

@AshishYadav: The 2nd link shows one example for LL. If you want one for LR, https://stackoverflow.com/a/6824545/21294350 will be helpful which also shows for LL. In a  nutshell, IMHO LL uses leftmost derivation so it can just uses one production rule directly which allows top down. While for LR using rightmost derivation, we must keep going forward until reaching "the right end of the syntax pattern that will *combine* them", so bottom-up. This is related with tree traversal order https://blog.reverberate.org/2013/07/ll-and-lr-parsing-demystified.html due to that they are related with AST.
# [wikipedia](https://en.wikipedia.org/wiki/Operator-precedence_parser#)
## TODO
- > an operator-precedence parser is a bottom-up parser that interprets an operator-precedence grammar.
  bottom-up due to recursive descent
## Notes
- > Edsger Dijkstra's shunting yard algorithm is commonly used to implement operator-precedence parsers.
  [see](https://en.wikipedia.org/wiki/Shunting_yard_algorithm#Detailed_examples)
  Here precedence, "Discard matching parenthesis" and "^ is evaluated right-to-left" implies their relation
  - matching parenthesis and "right-to-left" [see](https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing)
  - [difference](https://news.ycombinator.com/item?id=19196335) which can be also got by `stack,queue` used in shunting yard algorithm.
  - > Repeated until "(" found
    because we meet ")". So even if it may be invalid like (1-5+), we just pop.
- > optimized for evaluation such as Reverse Polish notation
  ~~[see](https://www.reddit.com/r/rust/comments/1e55erx/comment/ldld27g/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button)~~
  [see](https://algo.monster/liteproblems/150) which can be got from shunting yard algorithm.
- > An operator-precedence parser is a simple shift-reduce parser
  shift is just like `peek` while reduce is like `rhs := parse_expression_1 (rhs, precedence of op + (1 if lookahead precedence is greater, else 0))` which will combine the right operands as more as possible. [see](https://en.wikipedia.org/wiki/Shift-reduce_parser)
  > A Shift step advances in the input stream by one symbol.
  > A Reduce step ... joining them together as one tree with a new root symbol.
## [precedence climbing vs Pratt parsing](https://www.engr.mun.ca/~theo/Misc/pratt_parsing.htm)
- > To accommodate operators like these and additional prefix operators in a *modular* way, we can use Pratt parsing.
  it just changes `LeD` based on [code](https://tdop.github.io/) instead of writing a sequence of conditional statements (see "The new E is").
### [precedence climbing](https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#making-a-parser)
- > We can transform G to an equivalent non-left-recursive grammar G1 as follows:
  because no `P-->P ...`
- `nextPrec`
  > a value for r that prevents the loop from consuming operators whose precedence is too *low*.
  IMHO here should be high.
  - See
    > In the case of postfix operators, the use of r and nextPrec ensures that, for example, a!^b is an error
    here `prec(^)` is higher than `nextPrec(!)`, so it stops.
- 
## skipped
- > tree rewrite rules
  since wikipedia doesn't say that detailedly.
- > Full parenthesization
  see
  > A limitation to this strategy is that unary operators must all have higher precedence than infix operators.
  > which is probably not what is intended.
# [Bottom-up versus top-down](https://en.wikipedia.org/wiki/Bottom-up_parsing)
- The former is [Post-order](https://en.wikipedia.org/wiki/Tree_traversal#Pre-order,_NLR), while the latter is Pre-order as [this](https://blog.reverberate.org/2013/07/ll-and-lr-parsing-demystified.html) and [LL as top down and LR as bottom-up](https://stackoverflow.com/q/2392431/21294350) (i.e. Pre-order vs Post-order) says.

I encountered with this problem while reading about [Precedence climbing][1] and [Pratt parsing][2]. Their relation is shown in [this wikipedia reference link][3].
> To accommodate operators like these and additional prefix operators in a *modular* way, we can use Pratt parsing.
>
> We'll rewrite the E procedure to use commands. The old E is
> ```
> E(p) is  
>     precondition 0 ≤ p
>     var t := P
>     var r := +inf
>     loop
>         const l := next
>         exit unless p≤prec(next)≤r
>         consume
>         if isBinary(l) then 
>             const y := E( rightPrec(l) )
>             t := mknode(binary(l), t, y)
>         else t := mknode(postfix(l), t) 
>         r := nextPrec(l) 
>     return t
> ```
> The new E is
> ```
> E(p) is  
>     precondition 0 ≤ p
>     var t := P
>     var r := +inf
>     loop
>         const l := next
>         const c := leftComm[l]
>         exit unless p ≤ c.LBP ≤ r
>         consume
>         t := c.LeD( t, l )
>         r := c.NBP
>     return t
> ```


  [1]: https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method
  [2]: https://tdop.github.io/
  [3]: https://www.engr.mun.ca/~theo/Misc/pratt_parsing.htm