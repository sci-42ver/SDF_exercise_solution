class Parser(object):
  def Eat(self, val):
    if val:
      print(val)

Parser().Eat() # error
Parser().Eat(1)
