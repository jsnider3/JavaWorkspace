'''The pandigitals are n-digit numbers made from the digits [1, n].
  @author: Josh Snider'''
import itertools
import hacklib

class Pandigitals(object):
  ''' Provides utilities for
      working with the
      pandigitals. '''

  def __contains__(self, num):
    ''' Check that an n-digit number contains the digits [1, n]
      once and only once. As a side note, 8 and 9 digit
      pandigitals are never prime. '''
    strn = str(num)
    return all((str(digit) in strn for digit in range(1, len(strn) + 1)))

  def __iter__(self):
    for x in range(1, 10):
      nums = list(range(1, x + 1))
      for pan in itertools.permutations(nums):
        yield hacklib.tuple_to_num(pan)

  def __reversed__(self):
    ''' Unacceptably slow.'''
    count = 987654321
    while count:
      if count in self:
        yield count
      count -= 1

  def get_pandigital_product(self, num):
    ''' See Project Euler 32 '''
    for n in range(1, int(num**.5) + 1):
      m = num // n
      if num == n * m:
        if str(num) + str(n) + str(m) in self:
          return (n, m, num)
    return None

  def is_pandigital_multiple(self, num):
    ''' As defined in Project Euler 38 '''
    if num in self:
      strn = str(num)
      for n in range(1, hacklib.num_digits(num)//2 + 1):
        if self.multiple_helper(strn[n:], int(strn[:n]), 2):
          return True
    return False

  def multiple_helper(self, strn, start, mult):
    ''' Helper for is_pandigital_multiple. '''
    if strn == "":
      return True
    for x in range(1, len(strn) + 1):
      guess = int(strn[:x])
      if guess == start * mult:
        return self.multiple_helper(strn[x:], start, mult + 1)
      elif guess > start * mult:
        return False


