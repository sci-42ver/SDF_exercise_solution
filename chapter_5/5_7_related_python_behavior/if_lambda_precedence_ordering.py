# IGNORE (I forgot why I give one unrelated link) similar to https://stackoverflow.com/a/79497941/21294350
# This is related with if-consequent is or_test which can't be lambda https://docs.python.org/3/reference/expressions.html#if-expr.
print((lambda a: (a, a**2) if 2 > 1 else a**3)(2))

# TODO this docstring convention is not shown in PEP https://peps.python.org/pep-0257/#specification
"""
https://stackoverflow.com/a/79497941/21294350

b-#t a
b-#f d-#t c 
b-#f d-#f e

d-#t b-#f c
d-#t b-#t a
d-#f e

0.a. The following is also shown in Python 3.3 https://pythononlineeditor.com/python-online-editor-33/ although not explicitly shown in doc https://web.archive.org/web/20121101045119/https://docs.python.org/3/reference/expressions.html#summary.
"""
"first" if True else "ignore" if False else "second"
# 'first'