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
#### ensure understanding this algorithm
- https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
  see SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/python_demo/precedence_climbing.py
## skipped
- > tree rewrite rules
  since wikipedia doesn't say that detailedly.
- > Full parenthesization
  see
  > A limitation to this strategy is that unary operators must all have higher precedence than infix operators.
  > which is probably not what is intended.
## also see
### https://www.oilshell.org/blog/2016/11/01.html
- > In precedence climbing, right associativity is implemented by making the recursive call with next_min_prec = prec + 1.
  it is next_min_prec = prec.
  > This is the same thing expressed in two slightly different ways.
  just relatively less with former next value based on `prec`/`lbp`.
- The 3rd difference is same as that said in https://www.engr.mun.ca/~theo/Misc/pratt_parsing.htm.
- > Pratt's paper doesn't seem to mention the right associativity trick
  see 
#### skipped
- > These same differences in presentation are in Pratt's and Clarke's original papers
# [Pratt algorithm](https://tdop.github.io/)
## 1
skipped due to introduction of *historic* survey of the problem. (no need if just to understand the algorithm)
## 2.1 Lexical Semantics versus Syntactic Semantics
i.e. [word](https://en.wikipedia.org/wiki/Lexical_semantics) vs [syntax](https://qr.ae/pYEGOt)
- > This suggests that it is more natural to *associate semantics with tokens* than with classes of phrases.
  So **semantic code** instead of using `cond` (or just special `if`) as https://www.engr.mun.ca/~theo/Misc/pratt_parsing.htm shows.
- > There are two advantages of separating semantics from syntax in this way.
  i.e. not using ["phrase-structure rules"](https://en.wikipedia.org/wiki/Phrase_structure_rules) which may be related with [BNF](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form) defining syntax.
  - > So our assignment of semantics to tokens has a much better chance of being *modular* than an assignment to rules.
  - > the language designer is free to develop the syntax of his language without concern for how it will affect the semantics; instead, the semantics will affect decisions about the syntax. ... Thus syntax is the servant of semantics ... the substance of the message is conveyed with the semantics
    just `if then else` can be same as `if condition {} {}`.
    The latter is due to
    > seem appropriate to tailor the syntax to fit the semantics.
    - "servant" means unidirectional influence relation where "semantics" is more significant.
  - > syntax macro
    in Scheme it just rewrites one *token sequence* which can be thought as one big token.
### TODO
- "purely syntactic" tokens
## 2.2 Conventions for Linearizing Trees
cares about **AEB** in string which is got by Linearizing Trees (search for "issue").
> While these remarks are not essential to the basic approach, they do provide a sense in which operator precedence is *more than just an ad hoc solution* to the association problem.
### skipped
- > The theorems of this section may be interpreted as theorems about BNF grammars,
### TODO
- > An attractive alternative to precedence functions would be to dispose of the ordering and rely purely on the data types and legal coercions to resolve associations.
  IMHO we always have operator precedence but not the one for data type... So why care about data type ordering here...
## 2.3 Annotation
> Accordingly we require that all arguments be *delimited by at least one token*
> An obvious choice of delimiters is commas. However, this is not as valuable as a syntactic token that *documents the role of the argument* following it.
> Thus, we may *abbreviate* the previous example to for i to n do b
- > partly because complications arise, e.g., if - is to be used as both an infix and a prefix operator
  e.g. `a - b - c` can be `a - b, - c` or `a, -b, - c` etc.
- > We shall call the semantic tokens *associated* with a delimiter its parents.
  see
  > two parents, itself and '→' (where a→b|c is shorthand for if a then b else c)
  which is similar to `prefix  if ...` which manipulates both `then` and `else`.
- conclusion
  > This is one reason for preferring a procedural embedding of semantics; we can write arbitrary code to find all the arguments when the language designer feels the need to complicate things.
  i.e. **semantic code** for OOP as https://www.engr.mun.ca/~theo/Misc/pratt_parsing.htm shows.
## Section 3
- > which will use the value of left as either the translation or value of the left-hand argument
  i.e. either to another language
  > The object is to translate, for example, a+b into (PLUS a b)
  or get the value like the normal program interpreter does.
- > Clearly we want to be able to deal with a mixture of these two types of tokens, together with tokens having both kinds of arguments (infix operators).
  - > It runs the code of the current token, stores the result in a variable called left, advances the input, and repeats the process.
    based on thegreenplace
    just the `while` loop does almost same as the initial block in `expression`.
  - > The variable left may be consulted by the code of the next token, which will use the value of left as either the translation or value of the left-hand argument,
    i.e. `t.led(left)`
  - > In this case the code of a prefix operator can get its argument by calling the code of the following token.
    i.e. `left * expression(20)`
  - > This process will continue recursively until a token is encountered (e.g., a variable or a constant) that does not require an argument.
    i.e. `rbp < token.lbp` where the next operator `token` can't grab the former argument.
  - > The code of this token returns the appropriate translation and then so does the code of each of the other tokens, in the reverse of the order in which they were called.
    i.e. `return left` and then implied by the stack caused by `left * expression(20)`.
  - "a mixture of these two types of tokens"
    IMHO token can be
    1. number-type etc argument "It runs the code of the current token, stores the result in a variable called left"
    2. maybe ~~infix/~~postfix based on infix context "The variable left may be consulted by the code of the next token,"
    3. prefix "In this case the code of a prefix operator can get its argument by calling the code of the following token."
    4. non-postfix *comparably* "a token is encountered (e.g., a variable or a constant) that does not require an argument."
    5. compound token composed by primitive ones constructed by call stack "The code of this token returns the appropriate translation and then so does the code of each of the other tokens"
    So it may mean postfix and prefix when not considering relation (so not point 4, 5) and primitive non-operator (not point 1).
  - Here "both kinds of arguments" means "left-hand argument" and "right-hand argument".
### TODO
### Significant diagrams
- See the last diagram "The machine becomes..."
  - q0->q1 (i.e. in eli.thegreenplace.net):
    ```python
    t = token
    token = next()
    left = t.nud()
    ```
  - q1->q1, i.e. while loop but with one small difference for `run`.
- `if 3*a + b!↑-3 = 0 then print a + (b-1) else rewind`
  - > The tokens encountered during one incarnation of the parser are enclosed in a dotted circle
### Properties
- i.
  - > it begins with the input positioned just to the right of that token
    at least for `led`, `right = expression(10)` => `token = next()`
  - > starting either at t if t is a nud, or if t is a led then at the beginning of the expression of which left was the interpretation when the code of t started.
    either `left = t.nud()` or `left = t.led(left)`
  - > off the right end of the expression whose tree it is the root
    see
    > if 3*a + b!↑-3 = 0 then print a + (b-1) else rewind ...
- ii. implied by the return *if `while` condition fails*
- iii. 
  > immediately *follows an expression* whose interpretation the parser *has assigned to left*.
  i.e. `t.led(left)` which uses the *immediate* former `left`.
- iv. implied by `while` condition
- v. implied by `return left` and `t.led(left)`
- vi. implied by `literal_token(object)`.
- > Properties (ii), (iv) and (v) together completely account for the two possible fates of the contents of left. 
  where the former 2 constitutes the condition which functions as the base for (v).
- > property (v) guards against losing an expression in left by calling a nud which does not know the expression is there.
  based on (iii) `left` is the value got by `nud` call. And "given to the following led via left." "guards against losing".
- > Property (vi) says that binding powers are only relevant when an argument is involved.
  trivially because that "binding power" is to decide where "an argument" is bound.
  - > The lbp (left binding power) is *a property of the current token* in the input stream, and in general will change each time state q1 is entered. The left binding power is *the only property of the token not in its semantic code*.
    Not for eli.thegreenplace.net implementation
    Here the author may want to say that `lbp` can be located in one table for each token, so *unnecessary* to be in "its semantic code".
#### Definition
- IMHO here "there exists a token t" doesn't mean the same `t` in Property i which means same as thegreenplace codes. More specifically, it is the token past `t` due to `token = next()`.
### TODO
- > Because of the possibility of there being several recursive calls of the parser running simultaneously, a stack of return addresses and right binding powers must be used.
  multi-threaded?
## 4. Examples
The 1st paragraph is same as thegreenplace codes although with small differences like that the latter doesn't using `lbp(token)`.
### One Example is a bit complex (skipped)
> The reader interested in how this approach to theorem provers works is *on his own*
### One Example **closely** related with ex 5.7
> For the next example we describe a translator from the language used in the above to trees whose format is that of the internal representation of LISP s-expressions
- `nilfix  right          ["PARSE", bp] $`
  nud(a)←'b' => nud(right)←'["PARSE", bp]'
- `prefix  +           20 right $`
  nud(a)←'c' => nud(+)←'right' => nud(+)←'["PARSE", 20]' =(implicit)> 'parse(20)'
  - `infix   +           20 is "PLUS" $`
    `prefix  is          1  ["LIST", right, 'left', ["PARSE", bp]] $`
    led(+)←'is "PLUS" => led(+)←["LIST", "PLUS", 'left', ["PARSE", bp]] => '["PLUS", left, parse(20)]'
    lbp(+)←20
- `infix   getlist     25 is "GETLIST" $`
  will call `(a getlist b)` by '["GETLIST", left, parse(25)]' got similar to +.
- `prefix  if          2  ["COND", [right, check "then"; right]] @ (if token = "else" then (advance; [[right]])) $`
  - So for `if 3*a + b!↑-3 = 0 then print a + (b-1) else rewind`
    `3*a + b!↑-3 = 0` will be in the 1st `right`.
    `left = t.nud()` => 3
    `left = t.led(left)` => `left=3*a`
    Then inside that while loop for `left = t.nud()`, we meet `+`
    Similarly, we will meet = after consuming *one rhs object* of `+`.
    So `3,*,+,=` are in "one incarnation of the parser".
  - Similarly `a, 0` as the atom are in "one incarnation of the parser" separately.
    Similar for `-` (`3` is not related with it because it is rhs which may be called like `left * expression(20)`)
  - `b, !` are in "one incarnation of the parser" due to as lhs for `^`
    So they are in "one incarnation of the parser".
  - In a nutshell, `lhs, op` will be in "one incarnation of the parser" while `rhs` is in one separate "incarnation of the parser". Delim like "(" and ")" are in one separate "incarnation of the parser" each.
- `prefix  '           0  ["QUOTE", right & check "'"] $`
  seems to allow 'foo' etc ~~which is different from quote in Scheme~~ and then outputs (quote foo) in Lisp.
#### terms
- PROG2: http://boole.stanford.edu/pub/lingol.html
- `infixr` just tries to grab as more as possible for right.
- `prefix` will grab argument, so plus `bp←b` based on `nilfix`
- `infix` has left, so uses `led` instead of `nud`.
- Due to `["PARSE", bp]`, here `bp` is just `rbp` in `parse(rbp)`.
#### miscs
- > the power to improve the descriptive capacity of the metalanguage we get from bootstrapping.
  - IGNORE "bootstrapping" means "The object is to translate ...", i.e. getting
    > the internal representation of LISP s-expressions, an ideal intermediate language for most compilers
    which then may be able to [compile language L back](https://en.wikipedia.org/wiki/Bootstrapping_(compilers)).
  - Based on
    > It is worth noting how effectively we made use of the bootstrapping capability in defining is, which saved a considerable amount of typing.
    here ["bootstrapping"](https://en.wikipedia.org/wiki/Bootstrapping) means 
    > a self-starting process that is supposed to continue or grow *without external input*
    IMHO a bit like state-machine which is also implied by the former diagrams.
    - "saved a considerable amount of typing" due to no duplicate "["LIST", ..., 'left', ["PARSE", bp]]".
- > The is facility is more declarative than imperative in flavor, even though it is a program.
  ~~"declarative" is due to it outputs one data structure "LIST ...".~~
  See https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
  - > This is already an improvement over a recursive descent parser because our grammar is now more declarative instead of being spread out over a few imperative functions, and we can see the actual grammar *all in one place*.
    Because of OOP, all programs are on token instead of [long procedures](https://en.wikipedia.org/wiki/Recursive_descent_parser) where `block` etc are like what is done in Precedence climbing.
### miscs
- > We present a subset of the definitions of tokens of the language L; all of them are defined in L, although in practice one would begin with a host language H (say the target language, here LISP) and write as many definitions in H as are sufficient to define the reast in L.
  Here we should group as "(...); (..., ...)". see [1](https://www.grammarly.com/blog/punctuation-capitalization/semicolon/#3) and https://www.grammarly.com/blog/punctuation-capitalization/comma-splice/.
  By translate.google, "reast" should be "rest".
### TODO
- > A useful one would be the ability to describe the argument structure of operators using regular expressions.
# implementation for Pratt algorithm
- [Statically typed](https://www.oilshell.org/blog/2016/11/05.html) Pratt parsers
  i.e. type is *fixed* and can be [got when compile](https://stackoverflow.com/a/34789786/21294350).
  - see http://lambda-the-ultimate.org/node/3682
    which at least doesn't say Pratt is bad.
    - Scheme https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/scheme/code/parsing/pratt/pratt.scm
    - https://langdev.stackexchange.com/a/3275 recommends Pratt in Conclusion.
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
# licence choice for scheme_demo
- I followed https://choosealicense.com/ and https://opensource.guide/legal/#which-open-source-license-is-appropriate-for-my-project
  I want to keep open-source, so not those like MIT. ~~But I want license not too restrictive like "choose an identical or compatible license"~~ I choose "“strong” copyleft license" since “weak” copyleft license will
  > The newly added files may be released under a different license or *kept proprietary (closed-source)*.
  So I choose AGPLv3 which seems [the most restrictive](https://choosealicense.com/licenses/).

  [1]: https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method
  [2]: https://tdop.github.io/
  [3]: https://www.engr.mun.ca/~theo/Misc/pratt_parsing.htm