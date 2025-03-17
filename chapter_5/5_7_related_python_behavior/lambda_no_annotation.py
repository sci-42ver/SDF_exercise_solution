# see https://stackoverflow.com/questions/28429680/lambda-and-multiple-statements-in-python/28429947#comment140227809_28429680 
# (lambda a:int : print(a))(3)
"""
    (lambda a:int : print(a))(3)
                  ^
SyntaxError: invalid syntax
"""

def test(a:int):
  print(a)
test(3)
