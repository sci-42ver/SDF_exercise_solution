This is from [operator precedence doc][1]:
> (expressions...), [expressions...], {key: value...}, {expressions...} Binding or parenthesized expression, list display, dictionary display, set display

Based on correspondence, `(expressions...)` means "Binding or parenthesized expression". By searching for "binding" in that doc link, I didn't find useful description about how binding is related with parenthesized expression.

"parenthesized expression" seems to mean [parenthesized forms][2]:
> parenth_form ::= "(" [starred_expression] ")"

Compared with the familiar `comp_for`:
> comp_for      ::= ["async"] "for" target_list "in" or_test [comp_iter]

Here `[starred_expression]` is optional. We can have
```python
def f(a, b, c, d): 
    print(a, b, c, d, sep = '&')
f(*[1, 2, 3, 4])
>>> 1&2&3&4
```
But it fails to use `parenth_form`
```python
f((*[1, 2, 3, 4]))
>>> 
```


  [1]: https://docs.python.org/3/reference/expressions.html#operator-precedence
  [2]: https://docs.python.org/3/reference/expressions.html#parenthesized-forms

---

> The first thing the code block will do is bind the formal parameters to the arguments

So the above `(*[1, 2, 3, 4])` is just that `parenth_form`.


