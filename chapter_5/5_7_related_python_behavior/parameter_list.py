## Just one demo to show https://docs.python.org/3/reference/compound_stmts.html#function-definitions parameter_list

# 0. https://thepythoncodingbook.com/2022/11/30/what-are-args-and-kwargs-in-python/
# **kwargs must be the end. So not "**kwargs, *, kw" etc.
# So '"*" [star_parameter] ("," defparameter)* ["," [parameter_star_kwargs]] "*" ("," defparameter)+' doesn't ensure valid syntax... https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-star_parameter
# 1. https://stackoverflow.com/a/36908/21294350 for *, **
# 1.a. https://stackoverflow.com/a/37032111/21294350 for hints of *, **. (TODO: * str hint meaning)
def test(*args : str, a, b : int = 3, **kwargs):
  pass

# test("1", 2, 4, name="2")
# https://stackoverflow.com/a/59630371/21294350
test("1", a=2, b=4, name="2")

# 0. https://stackoverflow.com/a/79104752/21294350
# 1. Here "identifier [":" ["*"] expression]" has implicit restriction for expression if having *.
# 2. Use mypy https://mypy.readthedocs.io/en/stable/getting_started.html https://www.reddit.com/r/learnpython/comments/1espz01/comment/li7jwlm/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button to throw errors.
def test2(*args : *tuple[int, str, int | None], a, b : int = 3, **kwargs):
  for item in args:
    print(item)
test2(1,a=2)

def test3(a,b,):
  print(a,b)
test3(1,2)

def test4(a0, a=1, /, b=3, *args, *, kwd_only):
  print(a,b,kwd_only)
test4(1,b=2,kwd_only=3)
