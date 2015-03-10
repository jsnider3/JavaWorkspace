''' Utilities for my Project Euler solutions.'''
import fractions
import functools
import itertools
import math
import numpy
import pdb
import string
from graph import Graph
from primes import Primes

class Abundants(object):
  ''' Generate the abundant numbers
      and check if given numbers are abundant.'''
  _List = []

  def __contains__(self, key):
    if key in self._List:
      return True
    abundant = sum(proper_divisors(key)) > key
    if abundant:
      self._List.append(key)
      self._List.sort()
    return abundant

  def __iter__(self):
    for num in self._List:
      yield num
    count = self._List[-1:]
    while True:
      count += 1
      if count in self:
        yield count

#########################

class Collatz(object):
  ''' Create a tree representation of the collatz sequence.'''
  def __init__(self):
    self._Depths = {1 : 1}

  @staticmethod
  def next(aInt):
    ''' Return what follows aInt in the Collatz sequence. '''
    assert aInt > 0
    if aInt % 2 == 0:
      return aInt / 2
    else:
      return (3 * aInt + 1) / 2

  def depth(self, aInt):
    ''' Return the distance of aInt from the root. '''
    if aInt in self._Depths:
      return self._Depths.get(aInt)
    else:
      self._Depths[aInt] = self.depth(self.next(aInt)) + 1
      return self._Depths[aInt]

  def max_depth(self):
    ''' Return the number farthest from the root. '''
    tMax = 1
    max_num = 1
    for entry in self._Depths.items():
      (tKey, tValue) = entry
      if tValue > tMax:
        tMax = tValue
        max_num = tKey
    return max_num

#########################

class Hexagonals(object):
  ''' Provides iterators and accessors for
      the hexagonal numbers. '''

  def __contains__(self, aInt):
    guess = (math.sqrt(8 * aInt + 1) + 1)/4
    return guess.is_integer()

  def __getitem__(self, n):
    return n * (2 * n - 1)

  def __iter__(self):
    count = 1
    while True:
      yield self[count]
      count += 1

#########################

class Naturals(object):
  ''' Provides iterators for
      the naturals. '''

  def __contains(self, num):
    return num > 0 and int(num) == num    

  def __iter__(self):
    count = 1
    while count:
      yield count
      count += 1

#########################
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
    #TODO This can be done for efficiently with itertools
    for x in range(1, 10):
      nums = list(range(1, x + 1))
      for pan in itertools.permutations(nums):
        yield tuple_to_num(pan)

  def __reversed__(self):
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
      for n in range(1, num_digits(num)//2 + 1):
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

#########################


class Pentagonals(object):
  ''' Provides iterators and accessors for
      the pentagonal numbers. '''

  def __contains__(self, num):
    return math.sqrt(24 * num + 1).is_integer()

  def __getitem__(self, n):
    return n * (3*n - 1)/2

  def __iter__(self):
    count = 1
    while True:
      yield self[count]
      count += 1

#########################

class Roman_Numeral(object):
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

  @classmethod 
  def from_int(cls, num):
    ''' Greedy algorithm both works and is minimal!
       Matroids for the win! '''
    text = ""
    #print(cls.values)
    for val, roman_num in cls.values:
      while num >= val:
        text += roman_num
        num -= val
    return Roman_Numeral(text)

  def minimize(self):
    ''' Represent self using as few
        letters as possible. '''
    num = int(self)
    return Roman_Numeral.from_int(num).text

#########################

class Square_Chain(object):
  ''' Create a tree representation of the sequence in Euler #92.'''
  def __init__(self):
    self._End = {1 : 1, 89 : 89}

  @staticmethod
  def next(num):
    ''' Return what follows num in the sequence. '''
    return digits_exp(num, 2)

  def get_end(self, num):
    ''' Return the final result of num. '''
    if num in self._End:
      return self._End.get(num)
    else:
      self._End[num] = self.get_end(self.next(num))
      return self._End[num]

#########################

class Squares(object):
  ''' Provides iterators and accessors for
      the triangular numbers. '''
  
  def __contains__(self, num):
    ''' Check if num is a square number. '''
    root = int(math.sqrt(num) + .5)
    return num == root * root

  def __getitem__(self, num):
    return int(num ** 2)

  def __iter__(self):
    count = 1
    while True:
      yield count ** 2
      count += 1

#########################

class Triangulars(object):
  ''' Provides iterators and accessors for
      the triangular numbers. '''

  def __contains__(self, num):
    if num < 0:
      return False
    guess = int((num) ** (.5))
    while self[guess] < num:
      guess += 1
    return self[guess] == num

  def __getitem__(self, aInt):
    return int(.5 * aInt * (aInt + 1))

  def __iter__(self):
    count = 0
    total = 0
    while True:
      total += count
      count += 1
      yield total

#########################

def alphabet_score(word):
  ''' Sum the difference between
      each character + 1 and. '''
  word = word.lower()
  total = 0
  for char in word:
    val = ord(char) - ord('a') + 1
    assert val > -1
    total += val
  return total

def ascii_sum(strn):
  ''' Sum the ascii values in a string'''
  return sum([ord(x) for x in strn])

def british_number_string(num):
  ''' Convert a number to a string
      in the british way. '''
  strn = str(num)
  words = []
  tens = {"1" : "ten", "2" : "twenty", "3" : "thirty", "4" : "forty",
          "5" : "fifty", "6" : "sixty", "7" : "seventy", "8" : "eighty",
          "9" : "ninety", "11" : "eleven", "12" : "twelve", "13" : "thirteen",
          "14" : "fourteen", "15" : "fifteen", "16" : "sixteen",
          "17" : "seventeen", "18" : "eighteen", "19" : "nineteen"}
  ones = {"1" : "one", "2" : "two", "3" : "three", "4" : "four",
          "5" : "five", "6" : "six", "7" : "seven", "8" : "eight",
          "9" : "nine"}
  if len(strn) == 4:
    words.append(ones[strn[0]])
    words.append("thousand")
    strn = strn[1:]
  if len(strn) == 3:
    if strn[0] in ones:
      words.append(ones[strn[0]])
      words.append("hundred")
    if strn[1:] != "00":
      words.append("and")
    strn = strn[1:]
  if len(strn) == 2:
    if strn in tens:
      words.append(tens[strn])
      strn = strn[2:]
    else:
      if strn[0] in tens:
        words.append(tens[strn[0]])
      strn = strn[1:]
  if len(strn) == 1:
    if strn[0] in ones:
      words.append(ones[strn[0]])
    strn = strn[1:]
  strn = " ".join(words)
  print(strn)
  return strn

def champernowne(digit):
  ''' Get the nth digit of
      champernowne's constant. '''
  assert digit > 0
  count = 1
  strn = ""
  while len(strn) < digit:
    strn += str(count)
    count = count + 1
  return int(strn[digit - 1])

def choose(n, r):
  ''' return n choose r. '''
  if n < r:
    return 0
  else:
    return math.factorial(n)/(math.factorial(r) * math.factorial(n - r))

def coprime(first, second):
  ''' check if first and second are coprimes.'''
  return fractions.gcd(first, second) == 1

def digits_exp(num, pwr):
  ''' wrapper for digits_foo '''
  return digits_foo(num, lambda x: x ** pwr)

def digits_foo(num, foo):
  ''' Call foo on each digit of num
      and return the sum. '''
  strn = str(num)
  total = 0
  for char in strn:
    total += foo(int(char))
  return total

def digits_fac(num):
  ''' wrapper for digits_foo '''
  return digits_foo(num, lambda x: math.factorial(x))

def digits_sum(num):
  ''' wrapper for digits_foo '''
  return digits_exp(num, 1)

def fibonacci(term):
  ''' Return the termth fibonacci number. O(n)'''
  first = 0
  second = 1
  count = 1
  while count < term:
    tNext = first + second
    first = second
    second = tNext
    count = count + 1
  return second

def find_largest_palindrome_product(low, high):
  ''' Find the largest number P, which is a
      palindrome. Given P == A * B and
      low <= A < B <= high. '''
  first = high
  largest = 1
  while first >= low:
    for second in range(first + 1, high + 1):
      prod = first * second
      if is_palindrome(prod) and prod > largest:
        largest = prod
    first -= 1
  return largest

def find_pythag_triplet(total):
  ''' Find the pythagorean triplet
      that sums to total.'''
  for tA in range(1, total):
    for tB in range(tA + 1, total - tA + 1):
      for tC in range(tB + 1, total - tA - tB + 1):
        if tA ** 2 + tB ** 2 == tC ** 2:
          if tA + tB + tC == total:
            return [tA, tB, tC]

def find_right_triangles(perim):
  '''Find all right triangles having given perimeter.'''
  tTriangles = []
  for tA in range(1, perim/3):
    for tB in range(1, perim/2):
      tC = math.sqrt(tA ** 2 + tB ** 2)
      if tC.is_integer() and tA + tB + tC == perim:
        tTriangles.append((tA, tB, tC))
  return tTriangles

def first_incorrect_term(approx, func):
  '''Given an approximation of a function.
     Count how many terms it takes to diverge.'''
  count = 1
  while approx(count) == func(count):
    count += 1
  return approx(count)

def fit_polynomial(aTerms):
  ''' fit a polynomial to terms. '''
  xes = range(1, len(aTerms) + 1)
  return lambda x: int(.5 + numpy.polyval(
                        numpy.polyfit(xes, aTerms, len(aTerms) - 1), x))

def flat_index(aTwoDShape, aIndex):
  index = aIndex
  for row in aTwoDShape:
    if index < len(row):
      return row[index]
    else:
      index -= len(row)

def freq_counts(lst):
  freqs = {}
  for foo in lst:
    if not foo in freqs:
      freqs[foo] = 1
    else:
      freqs[foo] = freqs[foo] + 1
  return freqs

def get_amicable_pair(low):
  ''' If low is the smallest number of an amicable pair
      return the pair, else return None. '''
  high = sum(proper_divisors(low))
  if low < high and sum(proper_divisors(high)) == low:
    return (low, high)
  return None

def goldbach(num):
  ''' Does num have the property 
      described in Euler 46. '''
  primes = Primes()
  assert not num in primes
  assert num % 2
  for square in Squares():
    if num - 2 * square in primes:
      return True
    elif num < 2 * square:
      return False

def group_into_equivalency_classes(aList, equals):
  ''' Use aList and the given comparator to make
      a list of equivalency classes. '''
  classes = []
  for elem in aList:
    found = False
    for tClass in classes:
      if equals(tClass[0], elem):
        found = True
        tClass.append(elem)
        break
    if not found:
      classes.append([elem])
  return classes

def hexadecimal_strings(digits, fixed):
  '''Returns the number of n-digit hexadecimal strings,
     that are required to contain fixed specific characters'''
  #TODO Incorrect
  if digits < fixed:
    return 0
  return (16 ** (digits - fixed)) * math.factorial(fixed)

def is_anagram(first, second):
  ''' return if first and second are anagrams 
      of each other. '''
  first = str(first)
  second = str(second)
  for tChar in first + second:
    if first.count(tChar) != second.count(tChar):
      return False
  return True

def is_anagram_series(base, length):
  ''' Are base, base*2, ..., base*length
      anagrams of each other. '''
  nums = []
  for x in range(1, length + 1):
    nxt = base * x
    if nums and not is_anagram(nxt, nums[-1]):
      return False
    nums.append(base * x)
  return True

def is_arithmetically_increasing(aList):
  ''' Check that each member of aList differs
      from its predecessor by a single constant. '''
  assert len(aList) > 1
  tList = sorted(aList)
  tDiff = tList[1] - tList[0]
  for index in range(0, len(tList) - 1):
    new_diff = tList[index + 1] - tList[index]
    if new_diff != tDiff:
      return False
  return True

def is_palindrome(arg, base=10):
  ''' Check that arg is a palindrome.
      Works for numbers in either base 10 or base 2. '''
  strn = str(arg)
  if base == 2:
    strn = "{0:b}".format(arg)
  return strn == strn[::-1]

def is_power_of(num, base):
  ''' check if num == base ** a
      for some a. ''' 
  count = 0
  while base ** count < num:
    count += 1
  return base ** count == num

def is_rotation(first, second):
  ''' return if first and second are
      rotations of each other. '''
  first = str(first)
  second = str(second)
  if len(first) != len(second):
    return False
  return second in first + first

def largest_grid_product(grid):
  rows = len(grid)
  max_product = -float("inf")
  for tY in range(rows):
    row = grid[tY]
    tCols = len(row)
    for tX in range(tCols):
      if tY < rows - 4:
        prod = product([grid[tY][tX], grid[tY+1][tX],
                        grid[tY+2][tX], grid[tY+3][tX]])
        max_product = max(max_product, prod)
      if tX < tCols - 4:
        prod = product([grid[tY][tX], grid[tY][tX+1],
                        grid[tY][tX+2], grid[tY][tX+3]])
        max_product = max(max_product, prod)
      if tY < rows - 4 and tX < tCols - 4:
        prod = product([grid[tY][tX], grid[tY+1][tX+1],
                        grid[tY+2][tX+2], grid[tY+3][tX+3]])
        max_product = max(max_product, prod)
      if tY < rows - 4 and tX - 4 >= 0:
        prod = product([grid[tY][tX], grid[tY+1][tX-1],
                        grid[tY+2][tX-2], grid[tY+3][tX-3]])
        max_product = max(max_product, prod)
  return max_product

def largest_product_in_series(series, length):
  ''' Find the subsequence of given length with
      the largest product. '''
  assert len(series) >= length
  prod = 1
  for count in range(length):
    prod *= series[count]
  largest = prod
  for count in range(length + 1, len(series) - length):
    prod = 1
    for count in range(count, count + length):
      prod *= series[count]
    if prod > largest:
      largest = prod
  return largest

def longest_arithmetically_increasing_sequence(aList):
  ''' Finding the subsequence of the given list
      which is arithmetically increasing and the longest. '''
  if len(aList) < 2:
    return aList
  max_len = 1
  max_diff = aList[1] - aList[0]
  max_start = aList[0]
  for tLow in range(len(aList)):
    for tHigh in range(tLow + 1, len(aList)):
      tDiff = aList[tHigh] - aList[tLow]
      count = 0
      while aList[tLow] + tDiff * (count + 1) in aList:
        count += 1
      if count > max_len:
        max_len = count
        max_diff = tDiff
        max_start = aList[tLow]
  tSequence = []
  for count in range(0, max_len + 1):
    tSequence.append(max_start + max_diff * count)
  return tSequence

def lowest_common_multiple(numbers):
  common_factors = []
  for number in numbers:
    factors = Primes.factors(number)
    for factor in factors:
      while common_factors.count(factor) < factors.count(factor):
        common_factors.append(factor)
  return product(common_factors)

def make_change(coins, total):
  ''' Dynamic programming solution to 
      make change. '''
  num_coins = len(coins)
  solution = numpy.zeros((total + 1, num_coins))
  for index in range(num_coins):
    solution[0][index] = 1
  for loop in range(1, total + 1):
    for index in range(num_coins):
      tX = 0
      if loop - coins[index] >= 0:
        tX = solution[loop - coins[index]][index]
      tY = 0
      if index > 0:
        tY = solution[loop][index - 1]
      solution[loop][index] = tX + tY
  return int(solution[total][num_coins - 1])

def nim_3n(n):
  return not( "11" in bin(n))

def nim_sum(first, second):
  '''Fancy way of saying xor'''
  return first ^ second

def nim_winner(heaps):
  '''xord == 0 means current player loses. 
     xord != 0 means current player wins.'''
  xord = functools.reduce(nim_sum, heaps)
  return not xord

def number_spiral_sum(row):
  if row == 0:
    return 1
  else:
    n = 2 * row + 1
    return 4 * (n ** 2) - 12 * (row)

def num_digits(num):
  '''return Number of digits in num'''
  return len(str(num))

def points_on_slope(rise, run):
  points = 1 + fractions.gcd(rise, run)
  return points

def product(aList):
  ''' sum but for multiplication '''
  return functools.reduce(lambda x, y: x * y, aList)

def proper_divisors(num):
  ''' return the proper divisors of num '''
  divisors = []
  for x in range(1, num):
    if num % x == 0:
      divisors.append(x)
  return divisors

def quad_prime(a, b):
  ''' counts how many primes are
      generated by the function
      f(x) = x^2 + ax + b '''
  func = lambda x: x**2 + a * x + b
  count = 0
  while func(count) in Primes():
    count += 1
  return count

def eulerphi(num):
  #TODO Wrong for 12, probably wrong for others.
  #phi = aNum
  #for i in range(2, aNum + 1):
  #  if i in Primes() and aNum % i == 0:
  #    phi *= 1 - 1/i
  '''Factors = prime_facs(aNum)
  tFactors = set(tFactors)
  phi =  int(aNum * product(map(lambda x: 1 - 1 / x, tFactors)))
  return phi'''
  coprimes = 0
  for i in range(1, num + 1):
    if coprime(num, i):
      coprimes += 1
  return coprimes #int(phi)

def resilience(denom):
  ''' As defined by Project Euler 243 '''
  assert denom > 1
  resil = eulerphi(denom) / (denom - 1)
  print(denom, resil)
  return resil

#@functools.lru_cache(maxsize=None)
def rod_cuts(length, cut_size):
  ''' Calculate how many possible 
      ways to tile a 1d surface '''
  cuts = 1
  for _ in range(length - cut_size):
    #TODO This is wrong obviously.
    cuts += rod_cuts(length - cut_size, cut_size)
  return cuts

#@functools.lru_cache(maxsize=None)
def prime_facs(aNum):
  ''' convenience function '''
  return Primes.factors(aNum)

def shared_members(iters):
  '''Given a list of iterators which generate
    monotonically increasing numbers yield their shared
    numbers.'''
  members = []
  for seq in iters:
    members.append(next(seq))
  while True:
    for x in range(len(iters) - 1):
      while members[x] < members[-1]:
        members[x] = next(iters[x])
    for x in range(len(iters) - 1):
      if members[-1] < members[x]:
        members[-1] = next(iters[-1])
    if all(map(lambda x: members[-1] == x, members[:-1])):
      yield members[-1]
      members[-1] = next(iters[-1])

def square_sum(aNum):
  ''' Return the square of the sum of [1, aNum] '''
  total = 0
  for count in range(1, aNum + 1):
    total += count
  return total ** 2

def substring_div43(num):
  ''' As defined by Euler 43. '''
  if num_digits(num) != 10:
    return False
  strn = str(num)
  if (int(strn[1:4]) % 2 == 0 and
     int(strn[2:5]) % 3 == 0 and
     int(strn[3:6]) % 5 == 0 and
     int(strn[4:7]) % 7 == 0 and
     int(strn[5:8]) % 11 == 0 and
     int(strn[6:9]) % 13 == 0 and
     int(strn[7:10]) % 17 == 0):
    return True
  else:
    return False

def sum_squares(num):
  ''' Return the sum of the squares of [1, aNum] '''
  total = 0
  for count in range(1, num + 1):
    total += (count ** 2)
  return total

def triangle_max_path(aTriangle):
  triangle = list(reversed(aTriangle))
  tMax = triangle[0][:]
  for row in range(len(triangle)):
    if row > 0:
      cur_row = triangle[row]
      print(row)
      for col in range(len(cur_row)):
        tMax[col] = cur_row[col] + max(tMax[col], tMax[col + 1])
  triangle.reverse()
  return tMax[0]

def tuple_to_num(tupe):
  ''' Take a tuple of form (1, 2, 3, ... )
      and make it the number 123... '''
  tupe = list(tupe)
  tupe = [str(c) for c in tupe]
  tupe = "".join(tupe)
  return int(tupe)

def xor_file(text, key):
  key_ind = 0
  result = []
  for t in text:
    result.append(t ^ key[key_ind])
    key_ind += 1
    key_ind %= len(key)
  return result

def main():
  ''' main '''
  print("REDACTED")
  count = 9
  '''sums = 0
  for num in range(1, 17):
    print(num, hexadecimal_strings(num, 3))
    sums += hexadecimal_strings(num, 3)
  print(sums)
  print(hex(sums))
  '''

if __name__ == "__main__":
  main()
