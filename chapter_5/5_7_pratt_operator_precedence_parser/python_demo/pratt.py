"""
Better to add based on paper:
1. nonud etc (i.e. NullError in oilshell)

add based on https://www.engr.mun.ca/~theo/Misc/pratt_parsing.htm (see "The new E is"):
1. r
2. 
"""
# https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing
import re

token_pat = re.compile("\\s*(?:(\\d+)|(.))")

def tokenize(program):
    for number, operator in token_pat.findall(program):
        if number:
            yield literal_token(number)
        elif operator == "+":
            yield operator_add_token()
        elif operator == "-":
            yield operator_sub_token()
        elif operator == "*":
            yield operator_mul_token()
        elif operator == "/":
            yield operator_div_token()
        elif operator == "^":
            yield operator_pow_token()
        elif operator == '(':
            yield operator_lparen_token()
        elif operator == ')':
            yield operator_rparen_token()
        else:
            raise SyntaxError('unknown operator: %s', operator)
    yield end_token()

# IGNORE See https://realpython.com/python-use-global-variable-in-function/#the-global-keyword for global
# Here we can implicitly access that global var https://docs.python.org/3/faq/programming.html#what-are-the-rules-for-local-and-global-variables-in-python
# > In Python, variables that are only referenced inside a function are implicitly global. If a variable is assigned a value anywhere within the function’s body, it’s assumed to be a local unless explicitly declared as global.
def match(tok=None):
    global token
    if tok and tok != type(token):
        raise SyntaxError('Expected %s' % tok)
    token = next()


def parse(program):
    global token, next
    next = tokenize(program).__next__
    token = next()
    return expression()


def expression(rbp=0):
    global token
    t = token
    token = next()
    left = t.nud()
    while rbp < token.lbp:
        t = token
        token = next()
        left = t.led(left)
    return left

class literal_token(object):
    def __init__(self, value):
        self.value = int(value)
    def nud(self):
        return self.value

class operator_add_token(object):
    lbp = 10
    def nud(self):
        return expression(100)
    def led(self, left):
        right = expression(10)
        return left + right

class operator_sub_token(object):
    lbp = 10
    def nud(self):
        return -expression(100)
    def led(self, left):
        return left - expression(10)

class operator_mul_token(object):
    lbp = 20
    def led(self, left):
        return left * expression(20)

class operator_div_token(object):
    lbp = 20
    def led(self, left):
        return left / expression(20)

class operator_pow_token(object):
    lbp = 30
    def led(self, left):
        return left ** expression(30 - 1)

class operator_lparen_token(object):
    lbp = 0
    def nud(self):
        expr = expression()
        match(operator_rparen_token)
        return expr

class operator_rparen_token(object):
    lbp = 0

class end_token(object):
    lbp = 0

test_str1="3 * (2 + -4) ^ 4"
print(parse(test_str1),eval(test_str1.replace("^","**")))
