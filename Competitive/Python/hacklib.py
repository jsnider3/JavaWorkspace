'''Solutions for Project Euler, HackerRank, and various
  coding challenges.
  @author: Josh Snider'''
import bisect
from decimal import *
import fractions
import functools
import heapq
import itertools
import math
import numpy
import pdb
import strings
from graph import Graph
import primes

class Abundants(object):
  ''' Generate the abundant numbers
      and check if given numbers are abundant.'''

  def __contains__(self, key):
    if key in self.cache:
      return True
    abundant = sum(proper_divisors(key)) > key
    return abundant

  def __init__(self):
    self.cache = [12]

  def __iter__(self):
    count = 11
    while True:
      count += 1
      if count in self:
        yield count
        if count not in self.cache:
          self.cache.append(count)

  def __len__(self):
    raise NotImplementedError("There are infinite abundants.")

#########################
class ArithSequence(object):
  ''' Generalization of the Fibonacci sequence to other
      starting numbers.'''

  def __contains__(self, num):
    '''Check if num is in self, assuming
       the series is monotonically non-decreasing.'''
    assert all(num >= 0 for num in self.series[0:2])
    while self.series[-1] < num:
      self.series.append(self.series[-1] + self.series[-2])
    if self.series[-1] == num:
      return True
    else:
      return binary_search(self.series, num) != None

  def __getitem__(self, ind):
    ''' Walk up to the ind-th number and cache what
        we find.'''
    while len(self.series) <= ind:
      self.series.append(self.series[-1] + self.series[-2])
    return self.series[ind]

  def __init__(self, zeroth, first):
    ''' Start with two numbers.'''
    self.series = [zeroth, first]

  def __iter__(self):
    ''' Count up forever.'''
    count = 0
    while True:
      yield self[count]
      count += 1

  def __len__(self):
    raise NotImplementedError("This is an infinite sequence.")

#########################
class Collatz(object):
  ''' Create a tree representation of the collatz sequence.'''
  def __init__(self):
    self.depths = {1 : 1}

  @classmethod
  def next(cls, num):
    ''' Return what follows num in the Collatz sequence. '''
    assert num > 0
    if num % 2 == 0:
      return num / 2
    else:
      return (3 * num + 1)

  def depth(self, num):
    ''' Return the distance of num from the root. '''
    if num in self.depths:
      return self.depths.get(num)
    else:
      self.depths[num] = self.depth(self.next(num)) + 1
      return self.depths[num]

  def max_depth(self):
    ''' Return the number farthest from the root. '''
    return max(self.depths, key=lambda d: self.depths[d])

#########################
class Convergents(object):
  ''' See Euler 65. '''

  def __init__(self, start, denoms):
    self.start = start
    self.denoms = iter(denoms)
    self.stack = []

  def __iter__(self):
    yield fractions.Fraction(numerator=self.start, denominator=1)
    while True:
      self.stack.append(next(self.denoms))
      convergent = fractions.Fraction()
      for d in reversed(self.stack):
        convergent = 1 / (d + convergent)
      yield self.start + convergent

  @classmethod
  def of_e(cls):
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
    return cls(2, iter(e_denoms()))

#########################
class Decents(object):
  '''As per https://www.hackerrank.com/challenges/sherlock-and-the-beast.'''

  def __init__(self):
    '''Nothing to do here.'''
    pass

  def largest_of_len(self, leng):
    '''Find the largest Decent number given len(str(num)) == leng.'''
    if leng > 0:
      fives = leng - leng % 3
      while fives >= 0:
        threes = leng - fives
        if threes % 5 == 0:
          return int('5' * fives + '3' * threes)
        fives -= 3
    return None

#########################
class DisjointSet(object):
  ''' My implementation of
      https://en.wikipedia.org/wiki/Disjoint-set_data_structure. '''

  def __eq__(self, other):
    return set(self) == set(other)

  def __init__(self, members):
    ''' Make each element its own disjoint set. '''
    self.members = set(members)
    self.parents = {}
    for member in self.members:
      self.parents[member] = None

  def __iter__(self):
    ''' Return the disjoint sets this contains. '''
    groups = {}
    for member in self.members:
      parent = self.find(member)
      if parent not in groups:
        groups[parent] = set([])
      groups[parent].add(member)
    for group in groups:
      yield frozenset(groups[group])

  def __len__(self):
    ''' How many distinct sets do we have? '''
    return len(list(iter(self)))

  def __ne__(self, other):
    return not self.__eq__(other)

  def is_chief(self, x):
    ''' Is x the parent of a set? '''
    return self.parents[x] == None

  def find(self, x):
    ''' Find the chief representative of our set.'''
    if not self.is_chief(x):
      ourparent = self.parents[x]
      temp = self.find(ourparent)
      if temp != ourparent:
        self.parents[x] = temp
      return temp
    else:
      return x

  def union(self, x, y):
    ''' Place x and y in the same set by making one's head
        a child of the other. '''
    xparent = self.find(x)
    yparent = self.find(y)
    if xparent != yparent:
      self.parents[yparent] = xparent

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

  def __len__(self):
    raise NotImplementedError("This is an infinite sequence.")

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

  def __len__(self):
    return self.rows * self.cols

  def __str__(self):
    return "\n".join(str(row) for row in self.mat)

  def column(self, ind):
    ''' Return a column of the matrix as a list.'''
    if 0 > ind or ind >= self.cols:
      raise ValueError('Column index out of range.')
    col = []
    for row in range(self.rows):
      col.append(self.mat[row][ind])
    return col

  def diag_diff(self):
    ''' Return the sum of the top left -> bottom right diagonal
        minus the sum of the top right -> bottom left diagonal.'''
    if self.rows != self.cols:
      raise ValueError('Diag diff is only valid for square matrices.')
    bottomdown = 0
    upright = 0
    for col in range(self.cols):
      upright += self.mat[self.cols - col - 1][col]
      bottomdown += self.mat[col][col]
    return bottomdown - upright

  def find(self, other):
    ''' Try to find another matrix in this one. If found,
        return (rx, cx) the coordinates of the topleft. Otherwise,
        return None. '''
    assert isinstance(other, Matrix)
    if self.rows >= other.rows and self.cols >= other.cols:
      for row in range(0, self.rows - other.rows+1):
        for col in range(0, self.cols - other.cols+1):
          if other.mat[0][0] == self.mat[row][col]:
            if self.find_at(other, row, col):
                return (row, col)

  def find_at(self, other, row, col):
    ''' Try to find another matrix at the given location.'''
    for rshift in range(other.rows):
        for cshift in range(other.cols):
          if (other.mat[rshift][cshift] !=
             self.mat[row + rshift][col + cshift]):
            return False
    return True

  def local_maxes(self):
    ''' Mark all of the local maxima with X's.'''
    for col in range(1, self.cols - 1):
      for row in range(1, self.rows - 1):
        if (self.mat[row][col] > self.mat[row][col - 1] and
           self.mat[row][col] > self.mat[row - 1][col] and
           self.mat[row][col] > self.mat[row][col + 1] and
           self.mat[row][col] > self.mat[row + 1][col]):
          self.mat[row][col] = 'X'

  def is_col_sorted(self):
    ''' Check if each column is in sorted order.'''
    for col in range(self.cols):
      if not is_sorted(self.column(col)):
        return False
    return True

  def is_row_sorted(self):
    ''' Check if each row is in sorted order.'''
    return all(is_sorted(row) for row in self.mat)

  def ring(self, tl, br):
    ''' Get the perimeter of a square with the given
        top-left and bottom-right. '''
    ring = []
    (x1, y1) = tl
    (x2, y2) = br
    (indx, indy) = (x1, y1)
    while indx < x2:
      ring.append(self.mat[indy][indx])
      indx += 1
    while indy < y2:
      ring.append(self.mat[indy][indx])
      indy += 1
    while indx > x1:
      ring.append(self.mat[indy][indx])
      indx -= 1
    while indy > y1:
      ring.append(self.mat[indy][indx])
      indy -= 1
    if not ring:
      ring.append(self.mat[y1][x1])
    return ring

  def ring_set(self, tl, br, source):
    ''' Write to the specified ring. '''
    (x1, y1) = tl
    (x2, y2) = br
    (indx, indy) = (x1, y1)
    sind = 0
    while indx < x2:
      self.mat[indy][indx] = source[sind]
      indx += 1
      sind += 1
    while indy < y2:
      self.mat[indy][indx] = source[sind]
      indy += 1
      sind += 1
    while indx > x1:
      self.mat[indy][indx] = source[sind]
      indx -= 1
      sind += 1
    while indy > y1:
      self.mat[indy][indx] = source[sind]
      indy -= 1
      sind += 1
    if tl == br:
      self.mat[indy][indx] = source[sind]

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

  def slide(self, dist):
    ''' Slide the values in a matrix clockwise in place. '''
    if dist:
      ind = 0
      tl = (ind, ind)
      br = (self.cols - ind - 1, self.rows - ind - 1)
      while tl[0] < br[0] and tl[1] < br[1]:
        ring = self.ring(tl, br)
        shift = dist % len(ring)
        ring = ring[-shift:] + ring[:-shift]
        self.ring_set(tl, br, ring)
        ind += 1
        tl = (ind, ind)
        br = (self.cols - ind - 1, self.rows - ind - 1)

  def sort_by_rows(self):
    ''' Sort each row in the matrix. '''
    for row in self.mat:
      row.sort()

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

  def __contains__(self, num):
    ''' The naturals are the positive integers. '''
    return int(num) == num and num > 0

  def __iter__(self):
    ''' 1, 2, 3... '''
    count = 1
    while count:
      yield count
      count += 1

  def __len__(self):
    raise NotImplementedError("This is an infinite sequence.")

#########################
class OnlineMedian(object):
  ''' Get the median of a list while adding to it. '''

  def __init__(self):
    ''' Make two heaps, for elements less than and greater
        than the median. '''
    self.small = []
    self.large = []

  def add(self, elm):
    ''' Add an element in O(ln(n)) time. '''
    if len(self.small) + len(self.large) == 0:
      self.small.append(-elm)
    elif elm > self.median():
      heapq.heappush(self.large, elm)
    else:
      heapq.heappush(self.small, -elm)
    self.balance()

  def balance(self):
    ''' Make sure our sides are similarly sized.'''
    if len(self.small) > len(self.large) + 1:
      move = heapq.heappop(self.small)
      heapq.heappush(self.large, -move)
    elif len(self.large) > len(self.small) + 1:
      move = heapq.heappop(self.large)
      heapq.heappush(self.small, -move)

  def median(self):
    ''' Get the median. '''
    med = None
    if not len(self.small):
      med = self.large[0]
    elif not len(self.large):
      med = -self.small[0]
    elif len(self.small) > len(self.large):
      med = -self.small[0]
    elif len(self.large) > len(self.small):
      med = self.large[0]
    else:
      med = (self.large[0] - self.small[0]) / 2
    return float(med)

#########################
class Palindrome(object):
  ''' A palindrome is a string s where s == reversed(s).'''

  def __contains__(self, pal):
    ''' A straightforward definition.'''
    return pal == pal[::-1]

  def __init__(self):
    ''' Nothing to do.'''
    pass

  def by_reduction(self, word):
    ''' Return the number of character reductions
        needed to make word a palindrome. As per,
        www.hackerrank.com/challenges/the-love-letter-mystery.'''
    reductions = 0
    if word not in self:
      for ind in range(len(word) // 2):
        reductions += abs(ord(word[ind]) - ord(word[- 1 - ind]))
    return reductions

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

  def from_anagram(self, word):
    ''' Check if there is an anagram of word that is
        a palindrome.'''
    charcounts = freq_counts(word)
    odds = [c for c in charcounts if charcounts[c] % 2]
    return len(odds) <= 1

  def has_number(self, arg, base=10):
    ''' Check that arg is a palindrome.
        Works for numbers in either base 10 or base 2. '''
    strn = str(arg)
    if base == 2:
      strn = "{0:b}".format(arg)
    return strn in self

  def split_index(self, word):
    ''' Given a string find the lowest index that we can split
        it at to make a palindrome. Return None on failure.'''
    lind = 0
    eind = len(word) - 1
    if word in self:
      return None
    while lind <= eind:
      if word[lind] != word[eind]:
        if word[:lind] + word[lind+1:] in self:
          return lind
        elif word[:eind] + word[eind+1:] in self:
          return eind
        else:
          return None
      lind += 1
      eind -= 1
    return None


#########################
class Pentagonals(object):
  ''' Provides iterators and accessors for
      the pentagonal numbers. '''

  def __contains__(self, num):
    test = 24 * num + 1
    return test in Squares() and int(math.sqrt(test)) % 6 == 5

  def __getitem__(self, n):
    assert n > 0
    return (3*(n**2) - n) // 2

  def __iter__(self):
    count = 1
    while True:
      yield self[count]
      count += 1

  def __len__(self):
    raise NotImplementedError("This is an infinite sequence.")

  def index(self, num):
    ''' Given a pentagonal number find its index in the sequence.'''
    assert num in self
    ind = 1
    while self[ind] < num:
      ind *= 2
    return bisect.bisect_left(self, num, ind // 2, ind)

  def min_difference(self):
    ''' Find the minimal difference between two pentagonal
        numbers, whose difference and sum is also pentagonal.'''
    smallp = None
    bigp = None
    diff = None
    ind = 1
    while True:
      bottom = 1
      if diff:
        bottom = self.index(self.round(self[ind] - diff))
      for sm in range(bottom, ind):
        if (self.pair(self[ind], self[sm])
            and (not diff or self[ind] - self[sm] < diff)):
          bigp = self[ind]
          smallp = self[sm]
          assert bigp > smallp
          diff = self[ind] - self[sm]
      ind += 1
      if ind > 10000:
        return diff

  def pair(self, m, n):
    ''' See Euler 44 '''
    return max(m, n) - min(m, n) in self and m + n in self

  def round(self, guess):
    ''' Get the biggest pentagonal number p, where p <= guess.'''
    top = 2
    while self[top] < guess:
      top *= 2
    bottom = top // 2
    return self[bisect.bisect_left(self, guess, bottom, top)]

#########################
class Segment(object):
  ''' A range of numbers from [bottom, top].'''

  def __add__(self, other):
    if type(other) == int:
      return Segment(min(self.bottom, other), max(self.top, other))
    elif type(other) == Segment:
      return Segment(min(self.bottom, other.bottom), max(self.top, other.top))
    else:
      raise ValueError("Can't add segment and " + str(type(other)))

  def __contains__(self, num):
    return type(num) == int and num >= self.bottom and num <= self.top

  def __eq__(self, other):
    return (type(other) == Segment and
            other.bottom == self.bottom and
            other.top == self.top)

  def __gt__(self, num):
    return type(num) == int and num < self.bottom

  def __init__(self, bottom, top):
    self.bottom, self.top = min(bottom, top), max(bottom, top)

  def __len__(self):
    return self.top - self.bottom

  def __lt__(self, num):
    return type(num) == int and num > self.top

  def __repr__(self):
    return str(self)

  def __str__(self):
    return "Segment(" + str(self.bottom) + ", " + str(self.top) + ")"

#########################
class SquareChain(object):
  ''' Create a tree representation of the sequence in Euler #92.'''
  def __init__(self):
    self.end = {1 : 1, 89 : 89}

  @classmethod
  def next(cls, num):
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
      yield self[count]
      count += 1

  def __len__(self):
    raise NotImplementedError("This is an infinite sequence.")

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
    while True:
      yield self[count]
      count += 1

  def __len__(self):
    raise NotImplementedError("This is an infinite sequence.")

#########################

def accumulate(arr, tot):
  ''' Take as many values from arr as possible, given
      that the sum must be less than or equal to tot.'''
  arr = sorted(arr)
  take = []
  count = 0
  for ind in range(len(arr)):
    count += arr[ind]
    if count >= tot:
      take = arr[:ind]
      break
  return take

def all_zeroes(grid, tl, br):
  ''' Determine if the grid is all zeroes in the given area. '''
  [tl_x, tl_y] = tl
  [br_x, br_y] = br
  for x in range(tl_x, br_x):
    for y in range(tl_y, br_y):
      if grid[x][y] != 0:
        return False
  return True

def alphabet_score(word):
  ''' Sum the difference between
      each character + 1 and. '''
  word = word.lower()
  total = ascii_sum(word) - len(word) * (ord('a') - 1)
  return total

def and_product(fst, secnd):
    '''Returns functools.reduce(&&, range(fst, secnd+1)) which
        is the same as their shared binary prefix with 0's on the end.'''
    fst = list(bin(fst))[2:]
    secnd = list(bin(secnd))[2:]
    num = 0
    if len(fst) == len(secnd):
      same = True
      for ind in range(len(fst)):
        num *= 2
        same &= fst[ind] == secnd[ind]
        if same:
          num += int(fst[ind])
    return num

def ascii_sum(strn):
  ''' Sum the ascii values in a string'''
  return sum([ord(x) for x in strn])

def assign_candies(arr):
  ''' Given a list of numbers. Assign a number to each
      such that if a number is bigger than a neighbor
      it has more candies than it. Return the minimal amount
      of candy. From https://www.hackerrank.com/challenges/candies.'''
  candies = 1
  downtrend = 1
  uptrend = 1
  prev = arr[0]
  prevCandies = [1]
  for candy in arr[1:]:
    if candy == prev:
      downtrend = 1
      uptrend = 1
      candies += 1
      prevCandies = [1]
    elif candy < prev:
      prevCandies.append(1)
      candies += 1
      for ind in reversed(range(1, len(prevCandies))):
        if prevCandies[ind] == prevCandies[ind-1]:
          prevCandies[ind-1]+= 1
          candies += 1
      uptrend = 1
    elif candy > prev:
      downtrend = 1
      uptrend += 1
      candies += uptrend
      prevCandies = [uptrend]
    prev = candy
  return candies

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

def balanced_parentheses(line):
  ''' Check that line contains balanced parentheses. '''
  count = 0
  balanced = True
  for elm in line:
    if elm == '{':
      count += 1
    elif elm == '}':
      count -= 1
      if count < 0:
        balanced = False
        break
  return balanced and count == 0

def binary_search(arr, elm):
  ''' Search for elm in a sorted array,
      O(log(n)) time.'''
  if len(arr):
    low = 0
    high = len(arr) - 1
    mid = 0
    while low <= high:
      mid = (high + low) // 2
      if low == high and arr[mid] != elm:
        return None
      elif arr[mid] < elm:
        low = mid + 1
      elif arr[mid] > elm:
        high = mid
      else:
        return mid

def bitstring_fillin(bitstring):
  ''' Given a bitstring where some characters are hidden
      with '?', return every possible string it could be.'''
  if bitstring.count('?') == 0:
    return [bitstring]
  else:
    fillers = '1' * bitstring.count('?')
    locs = [ind for ind in range(len(bitstring)) if bitstring[ind] == '?']
    results = []
    while True:
      arr = list(bitstring)
      for (loc, fill) in zip(locs, fillers):
        arr[loc] = fill
      results.append(''.join(arr))
      if fillers == '0' * bitstring.count('?'):
        return results[::-1]
      else:
        next_fill = bin(int(fillers, 2) - 1)[2:]
        fillers = '0' * (bitstring.count('?') - len(next_fill)) + next_fill

def bitstring_or(fst, secnd):
  ''' Bitwise or for strings of 1 and 0. '''
  assert len(fst) == len(secnd)
  bitchar_or = lambda x, y: "1" if x == "1" or y == "1" else "0"
  return ''.join(list(bitchar_or(x, y) for (x,y) in zip(fst, secnd)))

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

def child_ages(total):
  ''' The name is from a logic puzzle.
      Find possible ages for three children, age <= 18,
      whose product is total.'''
  ages = set([])
  for young in range(1, 19):
    for mid in range(young, 19):
      for old in range(mid, 19):
        if young * mid * old == total:
          ages.add((young, mid, old))
  groups = group_into_equivalency_classes(ages, lambda x, y: sum(x) == sum(y))
  return groups

def choose(n, k):
  """
  A fast way to calculate binomial coefficients by Andrew Dalke.
  """
  if 0 <= k <= n:
    ntok = 1
    ktok = 1
    for t in range(1, min(k, n - k) + 1):
      ntok *= n
      ktok *= t
      n -= 1
    return ntok // ktok
  else:
    return 0

def closest_numbers(arr):
  ''' Return a list of tuples with minimal difference. '''
  arr = sorted(arr)
  diff = abs(arr[1] - arr[0])
  pairs = []
  for ind in range(0, len(arr) - 1):
    pair = (min(arr[ind:ind+2]) ,max(arr[ind:ind+2]))
    if abs(pair[1]-pair[0]) == diff:
      pairs.append(pair)
    elif abs(pair[1]-pair[0]) < diff:
      diff = abs(pair[1]-pair[0])
      pairs = [pair]
  return sorted(pairs)

def closest_string(phrase, letters):
  ''' Return phrase modified so that it only has
      the given number of unique letters. The modified
      version will change the smallest amount of the
      original as possible and be as lexicographically
      small as possible. '''
  assert letters > 0
  srtd = sorted(set(phrase))[::-1]
  srtd.sort(key=phrase.count)
  preserved = srtd[-letters:]
  lowest = preserved[0]
  print(phrase,letters,srtd,preserved,lowest)
  nstr = []
  for char in phrase:
    if char in preserved:
      nstr.append(char)
    else:
      nstr.append(lowest)
  return "".join(nstr)

def common_elements(*arrs):
  ''' Given a list of arrays return their common elements.'''
  sets = [set(arr) for arr in arrs]
  common = {}
  if len(sets):
    common = sets[0]
    for group in sets:
      common = common.intersection(group)
  return common

def convert_time_to_24(time):
  ''' Take time in form HH:MM:SS(AM|PM) and
      convert it to 24 hour time. '''
  mil = time[:-2]
  if time.endswith('AM') and time.startswith('12'):
    mil = '00' + mil[2:]
  elif time.endswith('PM') and time.startswith('12'):
    mil = '12' + mil[2:]
  elif time.endswith('PM'):
    hours = int(time[:2]) + 12
    mil = str(hours) + mil[2:]
  return mil

def cross_product(fst, secnd):
  ''' Cross-product of two 3D vectors.'''
  (fi, fj, fk) = fst
  (si, sj, sk) = secnd
  return (fj * sk - fk * sj, fk * si - fi * sk, fi * sj - fj * si)

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

def eight_queens_is_valid(soltn):
  ''' Is this a valid solution for the eight queens problem?'''
  return (len(soltn) == 8 and
          len(set(soltn)) == 8 and
          all(col >= 0 and col < 8 for col in soltn) and
          no_diag_collisions)

def eight_queens_solutions():
  ''' Generate solutions for the eight queens problems.
      Solution is a list of ints eight long. The queen
      goes in position (index, arr[index]).'''
  nums = list(range(1, x + 1))
  for soltn in itertools.permutations(nums):
    if is_valid_eight_queens(soltn):
      yield soltn

def electronic_numbers(n):
  numbers = {}
  numbers[0] = 6
  numbers[1] = 2
  numbers[2] = 5
  #numbers[3] = 5
  numbers[4] = 4
  #numbers[5] = 5
  numbers[6] = 6
  numbers[7] = 3
  numbers[8] = 7
  numbers[10] = 8
  numbers[22] = 10
  numbers[68] = 13
  #numbers[9] = 6
  # There's no reason to use 3, 5, 9.
  # 6 only beats out 0 when we can't use trailing zeros.
  # 7 loses to 8 when n >= 7.
  res = 0
  while n > 0:
    #assert n > 0
    nxt = None
    if n == 13:
      nxt = 68
    elif n % 2:
      if n >= 7:
        nxt = 8
      elif n >= 5:
        nxt = 2
      elif n >= 3:
        nxt = 7
    elif n == 6 or res > 0:
      nxt = 0
    elif n == 10:
      nxt = 22
    elif n == 8:
      nxt = 10
    elif n >= 14:
      nxt = 8
    elif n >= 6:
      nxt = 6
    elif n >= 4:
      nxt = 4
    elif n == 2:
      nxt = 1
    n -= numbers[nxt]
    res = res * (10 ** len(str(nxt))) + nxt
  if res > 0:
    snum = str(res)
    nonzeroes = list(sorted(c for c in snum if c != '0'))
    snum = [nonzeroes[0]] + [c for c in snum if c == '0'] + nonzeroes[1:]
    res = int(''.join(snum))
  return res

def extract_order(words, graf=None):
  ''' Given a list of things which are sorted,
      return the elements that occur in the sorted way.'''
  if not graf:
    graf = Graph()
  chars = list(functools.reduce(set.union, [set(word) for word in words]))
  inds = {words[ind]:ind for ind in range(len(words))}
  classes = group_into_equivalency_classes(words, lambda x, y: x[0] == y[0])
  for fst in range(len(classes) - 1):
    for secnd in range(fst + 1, len(classes)):
      less = inds[classes[fst][0]] < inds[classes[secnd][0]]
      if less:
        graf.set_edge(classes[fst][0][0], classes[secnd][0][0])
      else:
        graf.set_edge(classes[secnd][0][0], classes[fst][0][0])
  for cls in classes:
    sub_words = [word[1:] for word in cls if len(word) > 1]
    if len(sub_words):
      sub_order = extract_order(sub_words, graf)
  res = ''.join(str(elm) for elm in graf.topo_sort())
  return res

def factorial(num):
  prod = Decimal(1)
  for val in range(2, int(num + 1)):
    prod *= val
  return prod

def fencing(pieces, cutoff):
  ''' Given an array of fence piece lengths, return the number
      of unique ways we can make a rectangular fence whose area
      is greater than or equal to the cutoff. '''
  lengths = {elm:pieces.count(elm) for elm in pieces if pieces.count(elm) > 1}
  lenset = sorted(set(lengths))
  ways = 0
  for lo_ind in range(len(lenset)):
    lo = lenset[lo_ind]
    if lengths[lo] > 3 and lo * lo >= cutoff:
      ways += 1
    start = bisect.bisect_left(lenset, cutoff / lo, lo=lo_ind+1)
    if start < len(lenset):
      for hi_ind in range(start, len(lenset)):
        hi = lenset[hi_ind]
        if lo * hi >= cutoff:
          ways += len(lenset) - hi_ind
          break
  return ways

def fibonacci(term):
  ''' Return the termth fibonacci number. O(n)'''
  Fibs = ArithSequence(0, 1)
  return Fibs[term]

def fillings(heights):
  ''' In a list of ints, how much must be added
      to ensure that no element of an array is a local
      minima. '''
  count = 0
  if len(heights) > 2:
    leftmax = [0]
    for height in heights:
      leftmax.append(max(leftmax[-1], height))
    rightmax = [0]
    for height in reversed(heights):
      rightmax.append(max(rightmax[-1], height))
    rightmax = list(reversed(rightmax))
    for ind in range(len(heights)):
      count += max(min(leftmax[ind], rightmax[ind]) - heights[ind], 0)
  return count

def find_pythag_triplet(total):
  ''' Find the pythagorean triplet
      that sums to total.'''
  for adj in range(1, total):
    for opp in range(adj + 1, total - adj + 1):
      for hyp in range(opp + 1, total - adj - opp + 1):
        if magnitude([adj, opp]) == hyp:
          if adj + opp + hyp == total:
            return (adj, opp, hyp)

def find_right_triangles(perim):
  '''Find all right triangles having given perimeter.'''
  triangles = []
  for opp in range(1, perim // 3):
    for adj in range(1, perim // 2):
      hyp = magnitude([opp, adj])
      if hyp.is_integer() and opp + adj + hyp == perim:
        triangles.append((opp, adj, int(hyp)))
  return triangles

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

def freq_sort(lst):
  ''' Return the elements of a list from most to least common.
      Ties are broken by the natural order of the elements.'''
  elms = set(lst)
  elms = sorted(list(elms))
  return sorted(elms, key= lambda x: lst.count(x), reverse=True)

def get_amicable_pair(num):
  ''' If low is the smallest number of an amicable pair
      return the pair, else return None. '''
  oth = sum(proper_divisors(num))
  if sum(proper_divisors(oth)) == num:
    return (min(oth, num), max(oth, num))
  return None

def goldbach(num):
  ''' Does num have the property
      described in Euler 46. '''
  ps = primes.Primes()
  assert not num in ps
  assert num % 2
  for square in Squares():
    if num - 2 * square in ps:
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

def is_anagram_series(base, length):
  ''' Are base, base*2, ..., base*length
      anagrams of each other. '''
  nums = []
  for x in range(1, length + 1):
    nxt = base * x
    if nums and not strings.is_anagram(nxt, nums[-1]):
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

def is_digit_cancelling_fraction(numer, denom):
  ''' Is numer/denom, like 49/98 in that cancelling
      shared digits among the numerator and denominator
      accidentally correct? '''
  numdigits = set(str(numer))
  dendigits = set(str(denom))
  numlist = list(str(numer))
  denlist = list(str(denom))
  if not numdigits.intersection(dendigits):
    return False
  else:
    numlist = [num for num in numlist if num not in dendigits]
    denlist = [num for num in denlist if num not in numdigits]
    if numlist and denlist:
      rednum = int(''.join(numlist))
      reddenom = int(''.join(denlist))
      return (reddenom != 0 and fractions.Fraction(numer, denom) ==
              fractions.Fraction(rednum, reddenom))
    else:
      return False

def is_trivial_digit_cancelling_fraction(numer, denom):
  return (is_digit_cancelling_fraction(numer, denom) and
          (numer % 10 == 0 or denom % 10 == 0))

def is_permutation(fst, snd):
  for elm in fst:
    if fst.count(elm) != snd.count(elm):
      return False
  return True

def is_power_of(num, base):
  ''' check if num == base ** a
      for some a. '''
  count = 0
  while base ** count < num:
    count += 1
  return base ** count == num

def is_primitive_root(num, base):
  ''' Check if a number if a primitive root of a given base. '''
  return (fractions.gcd(num, base) == 1 and
          multiplicative_order(num) == primes.Primes().totient(base) % base)

def is_relatively_prime(fst, snd):
  return fractions.gcd(fst, snd) == 1

def is_rotation(first, second):
  ''' return if first and second are
      rotations of each other. '''
  first = str(first)
  second = str(second)
  if len(first) != len(second):
    return False
  return second in first + first

def is_sorted(arr):
  ''' Check that arr is in sorted order. '''
  if len(arr):
    elm = arr[0]
    for mem in arr:
      if mem < elm:
        return False
      elm = mem
  return True

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

def knapsack_isopriced(weights, total):
  ''' Given a bunch of weights. Return
      a mix of weights that sum to as high as possible,
      but not greater than the given total. This is
      different from regular knapsack in that the weights
      and values are equal. '''
  assert all(weight > 0 for weight in weights)
  sums = []
  sums.append(0)
  for _ in range(1, total + 1):
    best = 0
    for weight in weights:
      if len(sums) - weight >= 0:
        best = max(best, weight + sums[-weight])
    sums.append(best)
  assert len(sums) == total + 1
  return sums[-1]

def k_split(arr, leng):
  ''' Split arr into subarrays of length leng. '''
  assert len(arr) % leng == 0
  splits = []
  ind = 0
  while ind < len(arr):
      splits.append(arr[ind:ind+leng])
      ind += leng
  return splits

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

def largest_permutation(arr, swaps):
  ''' Make the largest permutation of arr with a given
      number of swaps.'''
  for walk in range(len(arr)):
    if swaps > 0:
      ind = arr[walk:].index(max(arr[walk:])) + walk
      if ind != walk:
        arr[ind], arr[walk] = arr[walk], arr[ind]
        swaps -= 1
  return arr

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

def least_common_multiple(numbers):
  '''Find the lcm of a list of numbers.'''
  common_factors = []
  for number in set(numbers):
    factors = primes.Primes().factors(number)
    for factor in factors:
      while common_factors.count(factor) < factors.count(factor):
        common_factors.append(factor)
  return product(common_factors)

def line_cover(locs, width):
  ''' Given a list of 1D coordinates,
      return a list of points such that each point
      in loc is within width a covering point.'''
  locs.sort()
  coverings = []
  high_cover = min(locs) - 2 * width
  for ind in range(len(locs)):
    if high_cover + width >= locs[ind]:
      pass
    elif high_cover + width < locs[ind]:
      coverings.append(locs[ind])
      high_cover = locs[ind]
    elif ind == len(locs) - 1 or locs[ind + 1] - width > locs[ind]:
      coverings.append(locs[ind])
      high_cover = locs[ind]
  return coverings

def list_cover(loc_lists):
  ''' Find a range [x, y] which covers at least one element
      of each list and where y - x is minimal. This code
      is hideous and needs to be rewritten. '''
  assert type(loc_lists) == list
  assert len(loc_lists)
  assert all(type(ls) == list for ls in loc_lists)
  assert all(len(x) for x in loc_lists)
  min_cover = None
  if len(loc_lists[0]) > 1:
    # For each element of the first list create a range [val, val]
    for val in loc_lists[0]:
      cover = list_cover([[val]] + loc_lists[1:])
      if not min_cover or len(cover) < len(min_cover):
        min_cover = cover
  else:
    # For every other list:
    min_cover = Segment(loc_lists[0][0], loc_lists[0][0])
    for ind in range(1, len(loc_lists)):
      if loc_lists[ind][0] > min_cover:
        loc_lists[ind] = [loc_lists[ind][0]]
        min_cover += loc_lists[ind][0]
      elif loc_lists[ind][-1] < min_cover:
        loc_lists[ind] = [loc_lists[ind][-1]]
        min_cover += loc_lists[ind][-1]
      elif any(num in min_cover for num in loc_lists[ind]):
        for num in loc_lists[ind]:
          if num in min_cover:
            loc_lists[ind] = [num]
            break
      else:
        #   else select the closest one thats too low and the closest one
        #      thats too high and recurse on them.
        for subind in range(len(loc_lists[ind]) - 1):
          if (loc_lists[ind][subind] < min_cover and
              loc_lists[ind][subind + 1] > min_cover):
            lo_cover = list_cover(loc_lists[:ind] + [[loc_lists[ind][subind]]]
                                                  + loc_lists[ind + 1:])
            hi_cover = list_cover(loc_lists[:ind] + [[loc_lists[ind][subind + 1]]]
                                                  + loc_lists[ind + 1:])
            return min(lo_cover, hi_cover)
  return min_cover

def load_time_estimator(sizes, uploadingStart, bandwidth):
  ''' Calculate the finish times for downloads of given
      size and start time which share a fixed bandwidth. '''
  def estimatorTick(done, downloads, amt):
    for download in downloads:
      done[download] += amt
  times = [0] * len(sizes)
  done = [0] * len(sizes)
  downloading = set([])
  starts = sorted(set(uploadingStart))
  for sind in xrange(len(starts)):
    start = starts[sind]
    downloading.update([ind for ind in xrange(len(uploadingStart)) if uploadingStart[ind] == start])
    rate = float(bandwidth) / len(downloading)
    nind = min(downloading, key=lambda ind: sizes[ind] - done[ind])
    finishtime = (sizes[nind] - done[nind]) / rate
    curtime = starts[sind]
    if len(downloading) and sind < len(starts) - 1 and finishtime >= starts[sind+1] - starts[sind]:
      estimatorTick(done, downloading, rate * (starts[sind+1] - starts[sind]))
    while len(downloading) and (sind == len(starts) - 1 or
                                finishtime < starts[sind+1] - starts[sind]):
      estimatorTick(done, downloading, rate * finishtime)
      for ind in list(downloading):
        if sizes[ind] - done[ind] <= 0:
          times[ind] = int(math.ceil(curtime + finishtime))
          downloading.remove(ind)
      curtime += finishtime
      if len(downloading):
        rate = float(bandwidth) /  len(downloading)
        nind = min(downloading, key=lambda ind: sizes[ind] - done[ind])
        finishtime = (sizes[nind] - done[nind]) / rate
  return times

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

def longest_increasing_subsequence(arr):
  ''' Return the longest subsequence that is in
      increasing order. '''
  subseqs = [[arr[0]]]
  maxes = [arr[0]]
  for elm in arr[1:]:
    ind = bisect.bisect_left(maxes, elm)
    if (ind == len(subseqs) or elm < subseqs[ind][-1]):
      subseqs.insert(ind, subseqs[ind-1] + [elm])
      maxes.insert(ind, elm)
  return subseqs[-1]

def magnitude(vec):
  ''' Euclidean norm of a vector. '''
  return math.sqrt(sum(num ** 2 for num in vec))

def make_change(coins, total):
  ''' Dynamic programming solution to
      count the possible ways to make change. '''
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

def manhattan_distance_1D(start, end):
  ''' Used by 2D for each axis. '''
  if math.floor(start) == math.floor(end):
    left = start - math.floor(start) + end - math.floor(end)
    right = math.ceil(start) - start + math.ceil(end) - end
    return min(left, right)
  else:
    return abs(start-end)

def manhattan_distance_2D(start, end):
  [x1, y1] = start
  [x2, y2] = end
  return (manhattan_distance_1D(x1, x2) +
          manhattan_distance_1D(y1, y2))

def max_subarray(arr):
  ''' Find the contiguous subarray in arr with the
      largest sum and return it.'''
  if max(arr) <= 0:
    return [max(arr)]
  max_here = 0
  max_tot = 0
  max_ind = 0
  for ind in range(len(arr)):
    max_here = max(arr[ind], max_here + arr[ind])
    if max_here > max_tot:
      max_tot = max_here
      max_ind = ind
  tot = 0
  for ind in reversed(range(max_ind + 1)):
    tot += arr[ind]
    if tot == max_tot:
      return arr[ind : max_ind + 1]

def max_sum_increasing_subsequence(arr):
  ''' Find the (possibly non-contiguous) subsequence
      in arr which has the highest values. '''
  max_ends = list(arr)
  for end in range(1, len(arr)):
    for cand in range(end):
      if (arr[cand] < arr[end] and
          max_ends[end] < max_ends[cand] + arr[end]):
        max_ends[end] = max_ends[cand] + arr[end]
  return max(max_ends)

def min_rotated_array(arr):
  ''' Find the minimum of a sorted (and rotated) array in
      O(ln(n)) time. '''
  lowest = None
  if len(arr) == 1 or arr[0] < arr[-1]:
    lowest = arr[0]
  else:
    midpoint = len(arr) // 2
    if arr[midpoint] < arr[0]:
      lowest = min_rotated_array(arr[1:midpoint + 1])
    else:
      lowest = min_rotated_array(arr[midpoint:])
  return lowest

def moore_voting(arr):
  ''' If the majority of arr is a single element,
      return it in a list.
      Returns None on failure.
      O(n) time and constant space. '''
  maj = None
  if arr:
    max_elm = arr[0]
    count = 0
    for elm in arr:
      if elm == max_elm:
        count += 1
      else:
        count -= 1
      if count == 0:
        max_elm = elm
        count = 1
    if arr.count(max_elm) > len(arr) / 2:
      maj = [max_elm]
  return maj

def multiplicative_order(num):
  ''' Return the multiplicative order of a number.'''
  pass

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

def nodes_with_branching(height, branches):
  ''' The maximum number of nodes that can be in a tree of
        a given height and uniform branching factor.'''
  count = 1
  for num in range(1, height + 1):
    count += branches ** num
  return count

def no_repeats(lst):
  ''' Take a series and yield a series without two
      following elements being equal.'''
  if len(lst):
    elem = lst[0]
    yield elem
    for nxt in lst[1:]:
      if nxt != elem:
        elem = nxt
        yield elem

def number_spiral_sum(row):
  ''' What is the sum of the four corners on a
      number spiral sum with width 2 * row + 1.'''
  if row == 0:
    return 1
  else:
    n = 2 * row + 1
    return 4 * (n ** 2) - 12 * (row)

def num_digits(num):
  '''return Number of digits in num'''
  return len(str(num))

def pair_diffs(arr, diff):
  ''' Return a list of tuples in arr whose difference is diff,
      given that the nums in arr are distinct.'''
  nums = set(arr)
  pairs = []
  for num in nums:
    if num + diff in nums:
      pairs.append((num, num + diff))
  return pairs

def pair_sums(arr, tot):
  ''' Return a list of tuples of indexes of arr whose
      sum is tot.'''
  inds = {}
  for ind in range(len(arr)):
    if arr[ind] not in inds:
      inds[arr[ind]] = []
    inds[arr[ind]].append(ind)
  pairs = set([])
  for ind in range(len(arr)):
    val = arr[ind]
    diff = tot - val
    if val <= diff and diff in inds:
      for other_ind in inds[diff]:
        if ind < other_ind:
          pairs.add((ind, other_ind))
        elif other_ind < ind:
          pairs.add((other_ind, ind))
  return pairs

def parking_access(carDimensions, parkingLot, luckySpot):
  ''' Given a car's dimensions, a parking lot where
      we can't drive through 1's, and the top left / bottom
      right of a place we want to park, can we get there without
      turning? '''
  tl_x, tl_y = luckySpot[0], luckySpot[1]
  br_x, br_y = luckySpot[2], luckySpot[3]
  # Determine if we're going left/right or up/down
  if carDimensions[1] == br_x - tl_x + 1:
    left = all_zeroes(parkingLot, [tl_x, 0], [br_x+1,br_y+1])
    right = all_zeroes(parkingLot, [tl_x, tl_y],
                      [br_x+1,len(parkingLot[0])])
    return left or right
  else:
    top = all_zeroes(parkingLot, [0, tl_y], [br_x+1,br_y+1])
    bottom = all_zeroes(parkingLot, [tl_x,tl_y],
                       [len(parkingLot),br_y+1])
    return top or bottom

def partition(arr):
  ''' Given an array [a, b...],
      partition it based on the first element.'''
  assert len(arr)
  return ([x for x in arr if x < arr[0]]
          + [x for x in arr if x == arr[0]]
          + [x for x in arr if x > arr[0]])

def permutation_set(arr):
  ''' Returns a generator that returns all
      of the unique permutations of arr in sorted order. '''
  seen = set([])
  def helper(prefix, end):
    if len(end):
      for ind in range(len(end)):
        for perm in helper(prefix + [end[ind]], end[:ind] + end[ind+1:]):
          yield perm
    elif str(prefix) not in seen:
      seen.add(str(prefix))
      yield prefix
  for perm in helper([], list(sorted(arr))):
    yield perm

def points_in_triangle(top, left, right):
  ''' How many points in a triangle with the given
      vertices are completely enclosed and have integer
      coordinates? The solution is Pick's Theorem. '''
  full_area = triangle_area(top, left, right)
  slopes = []
  slopes.append((top[0] - left[0], top[1] - left[1]))
  slopes.append((right[0] - left[0], right[1] - left[1]))
  slopes.append((right[0] - top[0], right[1] - top[1]))
  boundary_points = sum(points_on_slope(*slope) for slope in slopes)
  boundary_points -= 3
  return int(full_area - boundary_points / 2.0 + 1)

def points_on_slope(rise, run):
  '''Count the number of points on a slope that
    have integer x, y coordinates. '''
  points = 1 + fractions.gcd(abs(rise), abs(run))
  return points

def possible_ends(length, fst, secnd):
  ''' Given a series of length n starting at 0
      where each element differs from the previous by
      either fst or secnd, what are the possible endings?'''
  fst, secnd = min(fst, secnd), max(fst, secnd)
  assert length > 0
  ends = [fst * (length - 1)]
  if fst != secnd:
    diff = secnd - fst
    for _ in range(length - 1):
      ends.append(ends[-1] + diff)
  return ends

def product(ls):
  ''' sum but for multiplication '''
  prod = 1
  for num in ls:
    prod *= num
  return prod

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
  ps = primes.Primes()
  count = 0
  while func(count) in ps:
    count += 1
  return count

def reciprocal_length(denom):
  ''' Multiplicative order of 10 (mod n). '''
  order = 1
  if is_relatively_prime(denom, 10):
    while (10 ** order) % denom != 1:
      order += 1
  return order

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
  resil = primes.Primes().totient(denom) / (denom - 1.0)
  return resil

def resilient_search(thresh):
  ''' Find the lowest d with
       resil(d) < thresh. '''
  ps = primes.Primes()
  piter = iter(ps)
  guess = next(piter)
  while resilience(guess) >= thresh:
    nxt = next(piter)
    if resilience(guess * nxt) < thresh:
      break
    else:
      guess *= nxt
  for n in range(2, max(ps.factors(guess))):
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

def stock_maximize(prices):
  ''' Take an array of stock prices and calculate the maximum
      profit that could be made if you're limited to buying one share
      a day and can't short sell.'''
  profit = 0
  if len(prices):
    peak = prices[-1]
    for bid in reversed(prices):
      if bid < peak:
        profit += peak - bid
      elif peak < bid:
        peak = bid
  return profit

def substring_div43(num):
  ''' As defined by Euler 43. '''
  if num_digits(num) != 10:
    return False
  strn = str(num)
  return (int(strn[1:4]) % 2 == 0 and
     int(strn[2:5]) % 3 == 0 and
     int(strn[3:6]) % 5 == 0 and
     int(strn[4:7]) % 7 == 0 and
     int(strn[5:8]) % 11 == 0 and
     int(strn[6:9]) % 13 == 0 and
     int(strn[7:10]) % 17 == 0)

def successor(arr):
  ''' Given an array, find the smallest permutation
      that is bigger than it. None means failure.'''
  if list(sorted(arr)) != arr[-1::-1]:
    #Find the first character that's not in reverse sorted order,
    pivot = None
    for ind in reversed(range(len(arr)-1)):
      if arr[ind] < arr[ind + 1]:
        pivot = ind
        break
    assert(pivot is not None)
    # swap it with the lowest element to its right and then sort
    # everything after where you pivoted.
    swp = [num for num in arr[pivot+1:] if num > arr[pivot]]
    repl = arr[pivot+1:].index(min(swp)) + pivot + 1
    arr[pivot], arr[repl] = arr[repl], arr[pivot]
    arr[pivot + 1 :] = list(sorted(arr[pivot + 1 :]))
    return arr
  else:
    return None

def sum_squares(num):
  ''' Return the sum of the squares of [1, num] '''
  return sum(count**2 for count in range(1, num + 1))

def take(source, num):
  taken = []
  source = iter(source)
  for _ in range(num):
    taken.append(next(source))
  return taken

def tile_four_by_n(width):
  ''' How many ways are there to tile a 4 X N floor
      with 4 X 1 and 1 X 4 tiles? '''
  ways = 1
  for blocks in range(1, width // 4 + 1):
    verts = width - blocks * 4
    spots = blocks + 1
    ways += int(choose(verts + spots - 1, spots - 1))
  return ways

def towers_of_hanoi(arr):
  ''' We have three poles and we want to move a stack of disks
      from one to a third. We have three rules:
      1) We can only move one disk at a time.
      2) We can only move the top one on a stack.
      3) All disks are of different sizes and we can only put one
         on smaller bigger disks.
      To move a tower of size n to another peg
      takes 2^n - 1 moves. '''
  pass

def triangle_area(*verts):
  ''' Area of a 2D triangle.'''
  assert len(verts) == 3
  left, top, right = sorted(verts)
  vec_one = (top[0] - left[0], top[1] - left[1], 0)
  vec_two = (top[0] - right[0], top[1] - right[1], 0)
  return .5 * magnitude(cross_product(vec_one, vec_two))

def triangle_max_path(aTriangle):
  triangle = list(reversed(aTriangle))
  tMax = triangle[0][:]
  for row in range(len(triangle)):
    if row > 0:
      cur_row = triangle[row]
      for col in range(len(cur_row)):
        tMax[col] = cur_row[col] + max(tMax[col], tMax[col + 1])
  triangle.reverse()
  return tMax[0]

def two_power_rank(num):
  ''' Subtract the highest power of two less than num from num.
      Return how many times it takes to reduce num to 1.'''
  assert num > 0
  if num == 1:
    return 0
  count = 1
  while 2 * count < num:
    count *= 2
  return 1 + two_power_rank(num-count)

def tunnel_possibilities(nodes, edges, cache={}):
  ''' How many connected graphs are there with a given
        number of distinct nodes and a given number of edges.'''
  #Maybe this is number of possible edges choose number of actual edges?
  # Then subtract out the non-connected things.
  ways = 1
  max_edges = nodes * (nodes - 1) // 2
  if edges < nodes - 1 or edges > max_edges:
    ways = 0
    print('branch 0', (nodes,edges), ways)
  elif edges == nodes - 1:
    ways = nodes ** (nodes - 2)
    print('branch 1', (nodes,edges), ways)
  elif edges >= choose(nodes - 1, 2) + 1:
    ways = choose(choose(nodes, 2), edges)
    print('branch 2', (nodes,edges), ways)
  elif (nodes, edges) in cache:
    ways = cache[(nodes, edges)]
    print('branch 3', (nodes,edges), ways)
  else:
    possible_graphs = choose(max_edges, edges)
    ways = possible_graphs - unconnected_graphs(nodes, edges)
    cache[(nodes, edges)] = ways
    print('branch 4', (nodes,edges), ways)
  return (ways)

def tuple_to_num(tupe):
  ''' Take a tuple of form (1, 2, 3, ... )
      and make it the number 123... '''
  tupe = list(tupe)
  tupe = [str(c) for c in tupe]
  tupe = "".join(tupe)
  return int(tupe)

def unconnected_graphs(nodes, edges, unc_graph_cache={}):
  ''' How many graphs with a given number of nodes and edges
      are not connected? '''
  unconnected_graphs = 0
  if (nodes, edges) in unc_graph_cache:
    unconnected_graphs = unc_graph_cache[(nodes, edges)]
  else:
    for num in range(nodes - 1):
      accum = 0
      top = (nodes - 1 - num) * (nodes - 2 - num) / 2
      stop = min(edges + 1, top + 2)
      for val in range(stop):
        accum += (choose(top, val) *
                  tunnel_possibilities(num + 1, edges - val))
        unconnected_graphs += choose(nodes - 1, num) * accum
      unc_graph_cache[(nodes, edges)] = unconnected_graphs
  return unconnected_graphs

def uniq(seq):
    ''' Return a sequence with elements that appear
        multiple times only appearing once when they
        first do.'''
    copy = []
    for elm in seq:
        if elm not in copy:
            copy.append(elm)
    return copy

def uniq_in_range(seq, k):
  ''' Return a sequence with elements removed so that
      no duplicates are within k of each other in the
      output. '''
  res = []
  ind = 0
  spots = {}
  for elm in seq:
    if elm not in spots or spots[elm] + k < ind:
      spots[elm] = ind
      res.append(elm)
      ind += 1
  return res

def visual_insert(arr, num):
  ''' Insert num into a sorted list arr
      and print out the array every time
      we copy a value.'''
  ind = len(arr) - 1
  arr.append(arr[ind])
  output = [" ".join(str(n) for n in arr)]
  while(ind != 0 and arr[ind - 1] > num):
    arr[ind] = arr[ind - 1]
    output.append(" ".join(str(n) for n in arr))
    ind -= 1
  arr[ind] = num
  output.append(" ".join(str(n) for n in arr))
  return output

def visual_insertion_sort(nums):
  '''Use insertion sort to sort a list
     of numbers and print out every step.'''
  for ind in range(1, len(nums)):
    for find in range(ind):
      if nums[find] >= nums[ind]:
        move = nums[ind]
        nums[find+1:ind+1] = nums[find:ind]
        nums[find] = move
    print(" ".join(str(n) for n in nums))

def word_location_map(document):
  ''' Take a document of space seperated strings and
      return a map where keys are words in the dictionary
      and values are lists of positions where they occur.'''
  split = document.split()
  loc_map = {}
  for ind in range(len(split)):
    word = split[ind]
    if word not in loc_map:
      loc_map[word] = []
    loc_map[word].append(ind)
  return loc_map

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

def xor_maximum(low, high):
  ''' Find the numbers (a, b) such that
      first <= a <= b <= second and a ^ b is maximal.
      Return the maximal value.'''
  max_num = low ^ high
  for first in range(low, high + 1):
    for second in range(first, high + 1):
      if first ^ second > max_num:
        max_num = first ^ second
  return max_num

def xor_sansa(arr):
  ''' For an array [1, 2, 3, 4, 5],
      return 1 xor 2 xor ... (1 xor 2) xor (2 xor 3) ...
      (1 xor 2 xor 3 xor 4 xor 5).'''
  sansa = 0
  cache =  []
  for _ in range(len(arr)+1):
    cache.append(0)
  count = 0
  for start in range(len(arr)):
    for end in range(start, len(arr)):
      cache[start] = cache[start] ^ arr[end]
      sansa = sansa ^ cache[start]
  return sansa

def zip_array_sum(first, second, tot):
  ''' Check if there's a permutation of first and second
      so that every element of zip (+) first second is
      greater than tot.'''
  return all(elm + oth >= tot for (elm, oth) in
          zip(sorted(first), reversed(sorted(second))))

def main():
  ''' main '''
  print("REDACTED")

if __name__ == "__main__":
  main()
