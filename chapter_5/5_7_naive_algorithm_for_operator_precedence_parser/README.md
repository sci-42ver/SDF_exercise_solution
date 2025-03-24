I won't use this precedence algorithm because it is so naive.

In SDF_exercises/chapter_5/5_7_naive_algorithm_for_operator_precedence_parser/5_7_precedence_lib.scm, I will iterate through `precedence-list` based on its already implied order (copied from Python). It will first group those elements related with the current *precedence item*. And then do the same thing for the next precedence item.

I originally doubts about whether we should backtrack during one process for one precedence item. For example, `a op1 b op2 c` will fail for op1 at first while going from left to right, but it may succeed after we group `b op2 c`. So we may need to track where we goes back which may cause even more iteration through the input expression...

---

(TODO verify this) Afterwards, I thought my doubt is no need since based on "from left to right" and op1,2 are of the same precedence level, that case won't happen. That means `op1` will always grab 2 objects around after having manipulated all precedence items with higher levels and other same level things on the left. This is just what that table means.

To be more specific, in https://docs.python.org/3/reference/expressions.html#operator-precedence:
1. `(expressions...)` etc can be done by *delimeter* `,` (maybe also `:`) which won't be influenced by other precedence results.
2. `x[index]` etc are similar to point 1.
3. `await x` until `or` are all binary or unary op's. ([`%` for string formatting](https://docs.python.org/3/tutorial/inputoutput.html#old-string-formatting))
4. `if` until `:=` are similar to point 1. (see SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/orig/pratt_new.scm for the simple examples of the former where a. it uses `if cond then res else alt` instead of `res if cond else alt` b. https://peps.python.org/pep-0572/#syntax-and-semantics implies `:=` can be one delimeter)

---

Then I googled "operator precedence algorithm" and found one [bottom-up parser](https://en.wikipedia.org/wiki/Operator-precedence_parser). Then "operator-precedence parser top-down" since I prefers top-down which IMHO is more straightforward (maybe wrong for this opinion) where I found Pratt.

Anyway the above naive algorithm will iterate through the expression *multiple times* which is much more inefficient compared with Pratt.
