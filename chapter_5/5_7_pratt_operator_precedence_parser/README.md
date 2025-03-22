What Quinten says is also said in video 30:26 and 50:25. Here security may be due to avoiding modifying one global variable which may influence *anywhere* in the program. This is said detailedly in https://security.stackexchange.com/questions/216421/global-variables-and-information-security#comment439249_216421.

You seems to have one typo that `newObj` doesn't have `text` property.

https://stackoverflow.com/a/10657843/21294350: To be more specific based on https://cs.stackexchange.com/a/93148/161388, here one lookahead token is needed for S, at least k+1 lookahead tokens are needed for A, and we don't have one *simpler* LL(k) *grammar* (better with one strict proof as p10 "Suppose now that we have an LL(k-1) grammar equivalent to G, ... which is in contradiction with (4)" in https://sci-hub.ru/10.1007/BF01946814) as ladypary shows for that one, so we have "LL(k+1) language that is not LL(k)".

To be more specific about what Maximilian says, The book (also for the 2nd version) says "Fig. 8.11. An LL(k+1) *grammar* that is not LL(k)" for the above example. "An LL(k+1) grammar that is not LL(k)" is easily got due to we need at least k+1 "tokens of lookahead" https://en.wikipedia.org/wiki/LL_parser to decide whether S ends with a or b. Notice one formal language can have *possibly more than one* formal grammar (Its definition doesn't exclude that possibility https://en.wikipedia.org/wiki/Formal_language). Continued...

So the language corresponding to the above grammar can be also defined based on grammar S->a^kB, B->a|b which is one LL(k) grammar due to either a^k or a|b (i.e. at most k lookahead tokens) is checked (you can check https://cs.stackexchange.com/a/93148/161388 for how LL(1) is derived here).
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
  IMHO it is not that case. see `infixr` in paper.
- > Richards[1] gives a procedure for parsing expressions which uses recursion rather than an explicit stack despite being derived using the operator precedence technique.
  I won't check detailedly for [that reference book](https://archive.org/details/richards1979bcpl)...
  Anyway "explicit stack" probably means [shunting yard algorithm](https://en.wikipedia.org/wiki/Shunting_yard_algorithm). Probably "recursion rather than an explicit stack" just means https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method.
  - > requires the construction of the operator precedence matrix. We show how the procedure can be derived by grammar- and program-transformation from the recursive descent method, using only numerical precedences
    ["operator precedence matrix"](https://www.chegg.com/homework-help/questions-and-answers/calculate-precedence-function-following-operator-precedence-matrix-using-b-matrix-techniqu-q111462667) implies [a bit like](https://stackoverflow.com/a/63677576/21294350) partial order
- [Floyd's paper](https://dl.acm.org/doi/pdf/10.1145/321172.321179) is hard to search. I won't dig into that. Anyway it also uses *matrix* to represent precedence.
  - > a number should be associated with *each argument position* by means of precedence functions over tokens
    i.e. `f, g` in p9 where $*\gtrdot +$ just means the former's bp is larger.
    Then $\gtrdot$ implies the rule application order (see p4).
    - Here `f, g` construction steps may a bit complex based on "*precedence matrix*" (see p3) which is based on [*production* rules](https://en.wikipedia.org/wiki/Production_(computer_science)#Grammar_generation) (see p9. I won't dig into why it is  defined as that is.).
      Then we get $\lessdot, \gtrdot, \doteq$ relation.
      >  one of T1 < T~, T1 == T2, and T1 ::> T2 occurs, according as f( T1) < , T1) = g( T2), or f( Tr) > g( T2), respectively
    - Based on Pratt's paper, here `f, g` is jusr `rbp, lbp`.
      That can be checked by `f, g` definition.
      > X < Y then $f(X)<g(Y)$, if $X\doteq Y$ then f(X) = , and if X :: Y then  Y).
      - $X\lessdot Y$ just means $XU\Rightarrow XKY$ where K is bound to Y, so $rbp(X)<lbp(Y)$.
        Similar for others.
    - Also see https://stackoverflow.com/a/63677576/21294350
      - > ⋖, indicating that the input symbol should be shifted.
        > ⋗, indicating that the stack symbol should be reduced.
        Take $+\lessdot *, *\lessdot +$ as one example, here for 1+2*3, we should shift * like [step 10](https://en.wikipedia.org/wiki/Shift-reduce_parser#Parser_actions).
        But for 1*2+3, we should reduce the symbol `1*2` which is similar to [step 6](https://en.wikipedia.org/wiki/LR_parser#LR_parse_steps_for_example_A*2_+_1).
        - [advantages over LL parser](https://en.wikipedia.org/wiki/Top-down_parsing#Programming_language_application) (see for [L(k), k>=2](https://stackoverflow.com/questions/10634197/ll2-language-that-is-not-ll1#comment140246851_72787522))
          > For an *ambiguous* language to be parsed by an LL parser, the parser must lookahead more than 1 symbol, e.g. LL(3).
          Here reduce [only needs *one* symbol (TODO proof)](https://en.wikipedia.org/wiki/LR_parser#Theory) (also [see](https://stackoverflow.com/a/41752951/21294350)). This also implies Pratt's power which is also one LR.
          > So an LR(1) parsing method was, in theory, powerful enough to handle any reasonable language.
        - LR relation with rightmost
          see https://stackoverflow.com/questions/2392431/difference-between-top-down-and-bottom-up-parsing-techniques/2392440#comment140209425_2392440 and https://en.wikipedia.org/wiki/Context-free_grammar#Derivations_and_syntax_trees where the SO link in the former is one *reversed* derivation for LR.
        - Also see sly doc there
          > ( is always shifted onto the stack, meaning that its right precedence is higher than the left precedence of any operator.
          notice here "right precedence" means
          > the "left precedence" and the "right precedence", one used for the operator on the left and the other for the operator on the right.
          so here it just means precedence of `(` is higher than all the rest on the stack. So it "is always shifted".
  - Relation with Pratt paper
    In a nutshell, "precedence function" => direct assigning unchanged the binding power to each operator token.
    - > announce the outcome in advance for each pair A and B, basing the choices on some reasonable heuristics. ... The outcome was stored in a table.
      trivially this is implicit with the derivation based on bp. "reasonable heuristics" means based on *grammar*.
      - In paper, "table" means "precedence matrix", that for f and g, or that for LTCD and RTCD. All of these are based on *grammar*.
      - Due to derivation based on grammar, the calculation is much more than the mere  assignment of bp.
    - > One objection to this approach is that there seems to be little guarantee that one will always be able to find a set of numbers consistent with one's needs. 
      Floyd just constructs that based on grammar. So no need to "find".
      > Another obeection is that the programmer has to learn as many numbers as there are argument positions, which for a respectable language may be the order of a hundred.
      Again, no need to "learn" if using Floyd's.
      - > We present an approach to language design which simultaneously solves both these problems, without unduly restricting normal usage, yet allows us to retain the numeric approach to operator precedence.
        i.e. *not using Floyd's totally* (so a bit "restricting normal usage")
        > Nevertheless we would be interested to see *a less restrictive* set of conventions that offer a degree of *modularity* comparable with the above while retaining the use of precedence functions. The approach of *recomputing the precedence functions for every operator after one change to the grammar* is not modular, and does not allow flexible access to individual items in a library of semantic tokens.
        - Here "flexible access to individual items" is allowed here by semantic code.
    - It actually still uses operator precedence but not
      > The idea is to assign data types to classes and then to totally order the classes.
      - due to (2 Theorem proofs are skipped)
        > Unfortunately, nonsense testing requires looking up the types rA and aB and *verifying the existence of a coercion from rA to aB*.
        > The non-semantically motivated conventions about and, or, + and ↑ may be implemented by *further subdividing* the appropriate classes (here the Booleans and Algebraics) into pseudo-classes
        - > An attractive alternative to precedence functions would be to dispose of the ordering and rely purely on the data types and legal coercions to resolve associations. Cases which did *not have a unique answer* would be referred back to the programmer, which would be acceptable in an on-line environment, but *undesirable in batch mode*. 
          
          > Our concern about efficiency for interpreters could be dealt with by having the outcome of each association problem *marked at its occurrence, to speed things up on subsequent encounters*.
          IMHO i.e. like memorizer.
          > *Pending such developments*, operator precedence seems to offer the best overall compromise in terms of modularity, ease of use and memorizing, and efficiency.
  - > Pratt is assigning a total order to tokens with the binding power
    at least feasible compared with precedence matrix in Floyd's paper if all operator tokens have rbp & lbp.
- > Fortress language avoided a total order on operator precedence for usability reasons
  see https://labs.oracle.com/pls/apex/f?p=LABS:0:102530341090474:APPLICATION_PROCESS=GETDOC_INLINE:::DOC_ID:952 (just the slides in https://www.youtube.com/watch?v=EZD3Scuv02g. Actually based on "Fortress" context in Transcript and searching for "prece", this is the only thing about precedence said in that lecture.)
  > We use only *the most obvious and familiar* rules of precedence.
  >> This reduces opportunities for for making silly mistake
  https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=4e836364e239fc71ace7c63e01c6cbf89866cea6
  > operator precedence in Fortress is *not always transitive*
  - Anyway Python and C are both allow the total order by listing all based on "Operator precedence" probably implied by the above lecture 32:05
    > all these other language is that then implies that because their precedence is transitive ...
#### Background
Says why the author wants to use Pratt Parsing (because he wants to implement the more general "shell arithmetic")
- > but the core algorithm is the same:
  see
  - "*precedence*, or binding power" which also implies point 2 "precedence less than or equal to"
  - "tokens can be used in the null and/or left position" i.e. the *common* behavior for token.
#### Further Thoughts: Generation vs. Recognition
- https://bford.info/pub/lang/peg.pdf
  - IMHO here Recognition is ~~just shift-reduce... See 3.3 Interpretation of a Grammar where "parsing expression" always *forwards* without using grammar~~
    See "The classic example language $a^nb^nc^n$ is not context-free" where `&(A !b)` ensures $a^nb^n$. Then we consumes $a^n$ and then B ensures $b^nc^n$ and then $!.$ ensures ending.
    Here the only backtracking is done when &/! fails.
    - > The ?, *, and + operators behave as in common regular expression syntax, except that they are “greedy” rather than nondeterministic.
      Here it says about ? since *, + are already greedy.
  - > recognition-based vs. generative or grammar-based methods for describing languages
    see paper
    > Most language syntax theory and practice is based on generative systems, such as regular expressions and context-free grammars, in which a language is defined formally by a set of rules applied recursively to *generate* strings of the language. A recognition-based system, in contrast, defines a language in terms of rules or predicates that *decide whether* or not a given string is in the language.
    where decision is done step by step with shift-reduce.
    - > The TDOP algorithm is clearly in the recognition-based category
      IMHO no programming language will use that "generative or grammar-based methods" since grammar is more complexer than [the trivial LR which can automatically generate one parser](https://tratt.net/laurie/blog/2023/why_we_need_to_know_lr_and_recursive_descent_parsing_techniques.html) etc...
      > I've researched how "real" languages implement their parsers, and I have to agree with Pratt. Languages are often described by grammars, but in general we use hand-written parsers *rather than parsers derived automatically from grammars*.
#### skipped
- > These same differences in presentation are in Pratt's and Clarke's original papers
# [Pratt tutorial review](https://www.oilshell.org/blog/2016/11/02.html)
## [Douglas Crockford](https://crockford.com/javascript/tdop/tdop.html)
- a
  - > JavaScript and Lua both use prototypal inheritance, and I still don't see any advantage of this style over classes.
    Anyway *they are same* in JS as [Inheritance_and_the_prototype_chain] says.
  - > *Object.create()*, which I think has already fallen out of favor.
    said in QA1.
- b (see `original_symbol`)
  - > To me, tokens are data that are operated on with functions (or methods on a separate class.)
    > I've never seen a tutorial on recursive descent where tokens need nontrivial methods.
    same as paper `led(token)` etc.
- c
  - > The reuse of token objects as AST node objects. This results in a lot of mutation in the code
    e.g. it may restrict `arity` by `token.arity = "literal";` etc.
    > Its arity may be changed later to "binary", "unary", or "statement"
    - You can try `{let a;a=1;}` [here](https://crockford.com/javascript/tdop/index.html) for "reuse of token objects as AST node objects" meaning where `"="` has only one arity at all. So no need for the later change at all.
  - [pure function](https://en.wikipedia.org/wiki/Pure_function)
- d
  - `assignment("=");` => `infixr` => `let s = symbol(id, bp);` => `Object.create(original_symbol)`.
    Then `s.led = led || function (left) { ... }`
    - `this` [see](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/this)
      > it is used in object methods, where this refers to the object that the method is attached to,
      - > At the top level of a script, this refers to globalThis whether in strict mode or not.
        is better than https://stackoverflow.com/a/1981556/21294350
      - So here `this` refers to `s` which is one token `token = Object.create(o);` based on symbol.
    - Here it may modify global `symbol_table` by `symbol_table[id] = s;`.
      > Otherwise, you have to trace through multiple function calls to see what variable is being changed.
### [class-free](https://depth-first.com/articles/2019/03/04/class-free-object-oriented-programming/)
- Also see [QA1](https://stackoverflow.com/a/27595904/21294350)
  - Why new etc are dropped.
    - `new` is dropped as https://stackoverflow.com/a/6613332/21294350 says for simplicity.
    - `this` is avoided due to it may refer to [global variable which is bad](https://security.stackexchange.com/questions/216421/global-variables-and-information-security#comment439249_216421)? TODO
    - `null` is dropped to avoid *more than one bottom values*.
    - falisness ([falsy](https://developer.mozilla.org/en-US/docs/Glossary/Falsy)) is to avoid using many interchangable values as false ~~to be different from C~~, maybe for  understandability?
  - Here `{member} = spec` is [Destructuring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring) like `let {member1, ...rest2}={member1: 1, member2: 2}; console.log(rest2);` => `Object { member2: 2 }`.
  - [class free definition](https://rosettacode.org/wiki/Classless-objects)
    > In class-based languages, a new instance is constructed through a *class's constructor* function ... The resulting instance will *inherit all the methods and properties* that were defined in the class
    > With classless, objects can be made *directly and created by other objects*.
    just as here `spec` obj creates one new "frozen object".
    - compared with [Prototypal inheritance](https://javascript.info/prototype-inheritance) (also see [Inheritance_and_the_prototype_chain][Inheritance_and_the_prototype_chain] which also says about "Dynamic objects with prototypal inheritance". Both uses object to implement)
      So ~~object~~ class free doesn't imply impossibility of Prototypal inheritance. Anyway both are implemented by *object*. But the former has no inheritance.
      - Notice Prototypal inheritance is implicitly used in many places https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Inheritance_and_the_prototype_chain#different_ways_of_creating_and_mutating_prototype_chains.
      - Here "Prototypal inheritance" just means "class-based".
      - no [memory conservation](https://stackoverflow.com/a/56589789/21294350) like `rabbit.__proto__ = animal;` ([`Object.assign` doesn't have that conservation](https://stackoverflow.com/a/36956330/21294350) while [`Object.create`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/create#proto) has due to *prototype*).
        That is due to we destructure and construct one *new* object which can be [checked by `===` in JS](https://stackoverflow.com/questions/21457644/how-to-know-that-two-javascript-variable-point-to-the-same-memory-address#comment32380463_21457644).
      - own or inherited
        As the former says, here we *doesn't inherit* but just "destructure" the input `spec`.
      - retroactive heredity
        Again we have *no "inheritance"* at all (because only use that `spec` as one constructor *instead of reference*).
      - performance inhibiting
        Because no *inheritance chain* which may need time-consuming path search.
  - So
    > Objects are class-free. We can add a new member to any object by ordinary assignment. An object can inherit members from another object.
    just means we don't need to [modify class definition or add one "intermediary class"](https://stackoverflow.com/a/22856260/21294350)
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

See the above "Floyd's paper"
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
  - > This is already an improvement over a recursive descent parser because our grammar is now more declarative instead of being spread out over *a few imperative functions*, and we can see the actual grammar *all in one place*.
    Because of OOP, all programs are on token instead of [long procedures](https://en.wikipedia.org/wiki/Recursive_descent_parser) where `block` etc are like what is done in Precedence climbing.
    - same as what [this old wikipedia entry](https://en.wikipedia.org/w/index.php?title=Pratt_parser&oldid=907196572) (referred to in https://www.oilshell.org/blog/2016/11/01.html#python-code) says
      > a Pratt parser is an improved recursive descent parser that associates semantics with tokens *instead of grammar rules*.
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

[Inheritance_and_the_prototype_chain]:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Inheritance_and_the_prototype_chain