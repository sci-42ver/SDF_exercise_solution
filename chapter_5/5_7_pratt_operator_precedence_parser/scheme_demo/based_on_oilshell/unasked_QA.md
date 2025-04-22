How does python manipulate with precedence between comparison and boolean operations? 

---

Recently I want to implement one Python parser for one *infix expression* parser homework. Since this is one daily homework, I just implement a bit of the [Python expression grammar][1] by trying adding at least one op for each level. I have learnt that we should always adhere to the grammar and the related constraints unable to easily described in the grammar instead of adhering to that operator precedence list from my former question https://stackoverflow.com/q/79544489/21294350.

Based on [grammar][2],
> comparison    ::= or_expr (comp_operator or_expr)*

For `1 or 2 > 3` we should interpret it as `(1 or 2) > 3` which returns `False`. But  actually Python 3.13.2 returns 1 for that which implies it executes `1 or `.


  [1]: https://docs.python.org/3/reference/expressions.html#operator-precedence
  [2]: https://docs.python.org/3/reference/expressions.html#comparisons

---

That is `or_expr` instead of `or_test` in `or_test  ::= and_test | or_test "or" and_test`.
