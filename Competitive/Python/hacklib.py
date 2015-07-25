'''Solutions for Project Euler, HackerRank, and various
  coding challenges.
  @author: Josh Snider'''
import fractions
import functools
import itertools
import math
import numpy
import pdb
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
    self.depths = {1 : 1}

  @staticmethod
  def next(num):
    ''' Return what follows num in the Collatz sequence. '''
    assert num > 0
    if num % 2 == 0:
      return num / 2
    else:
      return (3 * num + 1) / 2

  def depth(self, num):
    ''' Return the distance of num from the root. '''
    if num in self.depths:
      return self.depths.get(num)
    else:
      self.depths[num] = self.depth(self.next(num)) + 1
      return self.depths[num]

  def max_depth(self):
    ''' Return the number farthest from the root. '''
    tMax = 1
    max_num = 1
    for entry in self.depths.items():
      (tKey, tValue) = entry
      if tValue > tMax:
        tMax = tValue
        max_num = tKey
    return max_num

#########################
class Convergents(object):
  ''' See Euler 65. '''

  def __init__(self, start, denoms):
    self.start = start
    self.denoms = iter(denoms)
    self.stack = []

  def __iter__(self):
    yield fractions.Fraction(numerator=self.start, denominator=1)
    pdb.set_trace()
    while True:
      self.stack.append(next(self.denoms))
      convergent = fractions.Fraction()
      for d in reversed(self.stack):
        convergent = 1 / (d + convergent)
      yield self.start + convergent

  @staticmethod
  def of_e():
    def e_denoms():
      loops = 1
      lst = [1, 2, 1]
      while True:
        for n in lst:
          if n == 1:
            yield 1
          else:
            yield 2 * loops
        loops += 1
    return Convergents(2, iter(e_denoms()))

#########################
class Hexagonals(object):
  ''' Provides iterators and accessors for
      the hexagonal numbers. '''

  def __contains__(self, num):
    guess = (math.sqrt(8 * num + 1) + 1)/4
    return guess.is_integer()

  def __getitem__(self, n):
    return n * (2 * n - 1)

  def __iter__(self):
    count = 1
    while True:
      yield self[count]
      count += 1

#########################
class Matrix(object):
  ''' Represents an MxN matrix. '''

  def __init__(self, arr):
    self.mat = arr
    self.rows = len(arr)
    self.cols = len(arr[0])
    lens = [len(row) for row in arr]
    if min(lens) != max(lens):
      raise ValueError('A matrix must have rows of equal length.')

  def __str__(self):
    return "\n".join(str(row) for row in self.mat)

  def rotate(self):
    ''' Rotate an array by 90 degrees clockwise in place. '''
    if self.rows != self.cols:
      raise ValueError('Can only rotate square matrices.')
    xes = self.cols // 2 + self.cols % 2
    yes = self.rows // 2
    xmax = self.cols - 1
    ymax = self.rows - 1
    for x in range(xes):
      for y in range(yes):
        swap = [self.mat[y][x],
                self.mat[x][xmax - y],
                self.mat[ymax - y][xmax - x],
                self.mat[ymax - x][y]]
        self.mat[y][x] = swap[-1]
        self.mat[x][xmax - y] = swap[0]
        self.mat[ymax - y][xmax - x] = swap[1]
        self.mat[ymax - x][y] = swap[2]

  def zero(self):
    ''' Zero each row and column that contains a zero. '''
    colzeros = [False for _ in range(self.cols)]
    rowzeros = [False for _ in range(self.rows)]
    for x in range(self.cols):
      for y in range(self.rows):
        if self.mat[y][x] == 0:
          colzeros[x] = True
          rowzeros[y] = True
    for x in range(self.cols):
      for y in range(self.rows):
        if colzeros[x] or rowzeros[y]:
          self.mat[y][x] = 0

#########################
class Naturals(object):
  ''' Provides iterators for
      the naturals. '''

  def __contains(self, num):
    ''' The naturals are the positive integers. '''
    return num > 0 and int(num) == num

  def __iter__(self):
    ''' 1, 2, 3... '''
    count = 1
    while count:
      yield count
      count += 1

#########################
class Palindrome(object):
  ''' A palindrome is a string s where s == reversed(s).'''

  def __contains__(self, pal):
    ''' A straightforward definition.'''
    return pal == pal[::-1]

  def __init__(self):
    ''' Nothing to do.'''
    pass

  def find_largest_product(self, low, high):
    ''' Find the largest number P, which is a
        palindrome. Given P == A * B and
        low <= A < B <= high. '''
    first = high
    largest = 1
    while first >= low:
      for second in range(first + 1, high + 1):
        prod = first * second
        if self.has_number(prod) and prod > largest:
          largest = prod
      first -= 1
    return largest

  def has_number(self, arg, base=10):
    ''' Check that arg is a palindrome.
        Works for numbers in either base 10 or base 2. '''
    strn = str(arg)
    if base == 2:
      strn = "{0:b}".format(arg)
    return strn in self

#########################
class Pentagonals(object):
  ''' Provides iterators and accessors for
      the pentagonal numbers. '''

  def __contains__(self, num):
    return math.sqrt(24 * num + 1).is_integer()

  def __getitem__(self, n):
    return int(n * (3*n - 1)/2)

  def __iter__(self):
    count = 1
    while True:
      yield self[count]
      count += 1

  def pair(self, m, n):
    ''' See Euler 44 '''
    return max(m, n) - min(m, n) in self and m + n in self

#########################
class SquareChain(object):
  ''' Create a tree representation of the sequence in Euler #92.'''
  def __init__(self):
    self.end = {1 : 1, 89 : 89}

  @staticmethod
  def next(num):
    ''' Return what follows num in the sequence. '''
    return digits_exp(num, 2)

  def get_end(self, num):
    ''' Return the final result of num. '''
    if num in self.end:
      return self.end.get(num)
    else:
      self.end[num] = self.get_end(self.next(num))
      return self.end[num]

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

  def __getitem__(self, num):
    return int(.5 * num * (num + 1))

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

def balanced_array(arr):
  ''' If there is an int n where sum(arr[:n]) = sum(arr[n+1:]),
      return it. Otherwise return None.'''
  left = [0]
  for num in arr:
    left.append(left[-1] + num)
  left.pop()
  right = [0]
  for num in reversed(arr):
    right.append(right[-1] + num)
  right.pop()
  right = list(reversed(right))
  for num in range(len(arr)):
    if left[num] == right[num]:
      return num
  return None

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

def chars_used(word):
  ''' Get the number of unique characters in a string. '''
  return len(set(word))

def choose(n, r):
  ''' return n choose r. '''
  if n < r:
    return 0
  else:
    return math.factorial(n)/(math.factorial(r) * math.factorial(n - r))

def digits_exp(num, pwr):
  ''' wrapper for digits_foo '''
  return digits_foo(num, lambda x: x ** pwr)

def digits_foo(num, func):
  ''' Call foo on each digit of num
      and return the sum. '''
  return sum([func(int(char)) for char in str(num)])

def digits_fac(num):
  ''' wrapper for digits_foo '''
  return digits_foo(num, math.factorial)

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

def fit_polynomial(terms):
  ''' fit a polynomial to terms. '''
  xes = range(1, len(terms) + 1)
  return lambda x: int(.5 + numpy.polyval(
                        numpy.polyfit(xes, terms, len(terms) - 1), x))

def flat_index(two_d, index):
  for row in two_d:
    if index < len(row):
      return row[index]
    else:
      index -= len(row)

def freq_counts(lst):
  ''' Return a map of a -> count(a) for a in lst. '''
  freqs = {}
  for elem in lst:
    if not elem in freqs:
      freqs[elem] = 0
    freqs[elem] += 1
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

def group_into_equivalency_classes(lst, equals):
  ''' Use lst and the given comparator to make
      a list of equivalency classes. '''
  classes = []
  for elem in lst:
    found = False
    for cls in classes:
      if equals(cls[0], elem):
        found = True
        cls.append(elem)
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
  for char in first + second:
    if first.count(char) != second.count(char):
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

def is_arithmetically_increasing(lst):
  ''' Check that each member of lst differs
      from its predecessor by a single constant. '''
  assert len(lst) > 1
  lst = sorted(lst)
  diff = lst[1] - lst[0]
  for index in range(0, len(lst) - 1):
    new_diff = lst[index + 1] - lst[index]
    if new_diff != diff:
      return False
  return True

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

def kth_element(arr, k):
  ''' Use quicksort to find the kth element of arr.'''
  assert len(arr)
  less = [x for x in arr if x < arr[0]]
  if k < len(less):
    return kth_element(less, k)
  k -= len(less)
  eql = [x for x in arr if x == arr[0]]
  if k < len(eql):
    return eql[k]
  k -= len(eql)
  gre = [x for x in arr if x > arr[0]]
  return kth_element(gre, k)

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
  prod = product(series[0:length])
  largest = prod
  for count in range(length + 1, len(series) - length):
    prod = product(series[count:count + length])
    if prod > largest:
      largest = prod
  return largest

def lonely_member(b):
  ''' In a list b where
      everything occurs twice
      except for one that only
      occurs once, return the
      one that only occurs once.'''
  seen = set([])
  for n in b:
    if n in seen:
      seen.remove(n)
    else:
      seen.add(n)
  return list(seen)[0]

def longest_arithmetically_increasing_sequence(lst):
  ''' Finding the subsequence of the given list
      which is arithmetically increasing and the longest. '''
  if len(lst) < 2:
    return lst
  max_len = 1
  max_diff = lst[1] - lst[0]
  max_start = lst[0]
  for tLow in range(len(lst)):
    for high in range(tLow + 1, len(lst)):
      diff = lst[high] - lst[tLow]
      count = 0
      while lst[tLow] + diff * (count + 1) in lst:
        count += 1
      if count > max_len:
        max_len = count
        max_diff = diff
        max_start = lst[tLow]
  sequence = []
  for count in range(0, max_len + 1):
    sequence.append(max_start + max_diff * count)
  return sequence

def lowest_common_multiple(numbers):
  '''Find the lcm of a list of numbers.'''
  common_factors = []
  for number in set(numbers):
    factors = Primes().factors(number)
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
  return "11" not in bin(n)

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

def partition(arr):
  ''' Given an array [a, b...],
      partition it based on the first element.'''
  assert len(arr)
  return ([x for x in arr if x < arr[0]]
          + [x for x in arr if x == arr[0]]
          + [x for x in arr if x > arr[0]])

def points_on_slope(rise, run):
  '''Count the number of points on a slope that
    have integer x, y coordinates. '''
  points = 1 + fractions.gcd(rise, run)
  return points

def product(ls):
  ''' sum but for multiplication '''
  return functools.reduce(lambda x, y: x * y, ls)

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

def reorder_chars(strn, left, right):
  '''Change strn so that right is never
    followed and touching a left. '''
  ret = ""
  x = 0
  while x < len(strn):
    if strn[x] not in [left, right]:
      ret += strn[x]
    else:
      numleft = 0
      numright = 0
      y = x
      while y < len(strn) and strn[y] in [left, right]:
        if strn[y] == left:
          numleft += 1
        else:
          numright += 1
        y += 1
      ret += left * numleft
      ret += right * numright
      x = y - 1
    x += 1
  return ret

def resilience(denom):
  ''' As defined by Project Euler 243 '''
  assert denom > 1
  resil = totient(denom) / (denom - 1)
  return resil

def resilient_search(thresh):
  ''' Find the lowest d with
       resil(d) < thresh. '''
  primes = Primes()
  piter = iter(primes)
  guess = next(piter)
  while resilience(guess) >= thresh:
    nxt = next(piter)
    if resilience(guess*nxt) < thresh:
      break
    else:
      guess *= nxt
  for n in range(2, max(primes.factors(guess))):
    if resilience(guess * n) < thresh:
      return guess * n

def rod_cuts(length, cut_size):
  ''' Calculate how many possible
      ways to tile a 1d surface '''
  cuts = 1
  for _ in range(length - cut_size):
    #TODO This is wrong obviously.
    cuts += rod_cuts(length - cut_size, cut_size)
  return cuts

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
    if all([members[-1] == x for x in members[:-1]]):
      yield members[-1]
      members[-1] = next(iters[-1])

def square_sum(num):
  ''' Return the square of the sum of [1, num] '''
  total = sum(range(1, num + 1))
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
  ''' Return the sum of the squares of [1, num] '''
  total = 0
  for count in range(1, num + 1):
    total += (count ** 2)
  return total

def totient(num):
  ''' Requires python 3 to be correct. '''
  primes = Primes()
  factors = primes.factors(num)
  factors = set(factors)
  prod = product([(1 - 1/x) for x in factors])
  phi = num * prod
  return int(phi)

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

def unique_str(words):
  ''' Determine if a string is composed entirely
      of unique characters. '''
  return len(words) == chars_used(words)

def xor_file(text, key):
  ''' Go through a list of numbers
      and xor them with numbers in
      the provided key. '''
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

if __name__ == "__main__":
  main()
