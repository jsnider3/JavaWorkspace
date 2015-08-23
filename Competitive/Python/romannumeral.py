''' Wraps Roman Numeral class.
    @author: Josh Snider.'''

class RomanNumeral(object):
  ''' Store roman numerals. As per
      https://projecteuler.net/about=roman_numerals'''

  values = [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"),
            (100, "C"), (90, "XC"), (50, "L"), (40, "XL"),
            (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]

  rev_dict = [(v, k) for (k, v) in values]

  def __init__(self, text):
    self.text = text.strip()

  def __int__(self):
    ''' Convert self into
        a normal int. '''
    text = self.text
    num = 0
    while text:
      for v, k in self.rev_dict:
        if text.find(v) == 0:
          text = text[len(v):]
          num += k
          break
    return num

  def __len__(self):
    return len(self.text)

  def __repr__(self):
    return "RomanNumeral({0})".format(self.text)

  def __str__(self):
    return self.text

  @classmethod
  def from_int(cls, num):
    ''' Greedy algorithm both works and is minimal!
       Matroids for the win! '''
    text = ""
    for val, roman_num in cls.values:
      while num >= val:
        text += roman_num
        num -= val
    return RomanNumeral(text)

  def minimize(self):
    ''' Represent self using as few
        letters as possible. '''
    num = int(self)
    return RomanNumeral.from_int(num).text
