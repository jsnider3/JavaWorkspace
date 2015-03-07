''' Utilities for my Project Euler solutions.'''
import fractions
import functools
import math
import numpy
#import pdb
from primes import Primes
#import sys

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

class Graph(object):
  ''' Class for graphs, containing vertices linked by
      weighted edges. '''
  def __init__(self):
    self._EdgeMap = {}
    self._Vertices = set([])

  def dijkstra(self, source):
    ''' Perform dijkstra's algorithm 
        starting at source. '''
    tDist = {}
    tPrev = {}
    tUnvisited = []
    #TODO Fixme
    for tKey in self._Vertices:
      tDist[tKey] = float("inf")
      tPrev[tKey] = float("inf")
      tUnvisited.append(tKey)
    tDist[source] = 0
    tDist[-2] = float("inf")
    while tUnvisited:
      tVisit = min(tUnvisited, key=lambda x: tDist[x])
      tUnvisited.remove(tVisit)
      tEdges = self._EdgeMap[tVisit]
      for entry in tEdges.items():
        (tNeighbor, tLeng) = entry
        tWeight = tDist[tVisit] + tLeng
        if tWeight < tDist[tNeighbor]:
          tPrev[tNeighbor] = tVisit
          tDist[tNeighbor] = tWeight
    return (tDist, tPrev)

  def dump_edges(self):
    ''' Print the edges in the graph. '''
    print(self._EdgeMap)

  @staticmethod
  def from_matrix(matrix):
    ''' Make a graph out of matrix. '''
    graph = Graph()
    rows = len(matrix)
    num_vertices = rows * len(matrix[0])
    func = lambda x, y: rows * y + x
    graph = Graph()
    for tX in range(rows):
      for tY in range(rows):
        vertex = func(tX, tY)
        left = func(tX - 1, tY)
        up = func(tX, tY - 1)
        right = func(tX + 1, tY)
        down = func(tX, tY + 1)
        weight = matrix[tY][tX]
        if tX > 0 and left >= 0:
          graph.set_edge(left, vertex, weight)
        if tY > 0 and up >= 0:
          graph.set_edge(up, vertex, weight)
        if tX < rows - 1 and right < num_vertices:
          graph.set_edge(right, vertex, weight)
        if tY < rows - 1 and down < num_vertices:
          graph.set_edge(down, vertex, weight)
    return graph

  @staticmethod
  def from_triangle(aTriangle):
    ''' Make a graph out of aTriangle. '''
    count = 0
    rows = len(aTriangle)
    graph = Graph()
    for row in range(rows):
      tColumns = len(aTriangle[row])
      if row != rows - 1:
        next_row = aTriangle[row + 1]
        for tX in range(tColumns):
          vertex = count + tX
          left_child = vertex + tColumns
          graph.set_edge(vertex, left_child, next_row[tX])
          graph.set_edge(vertex, left_child + 1, next_row[tX + 1])
      else:
        for tX in range(tColumns):
          vertex = count + tX
          graph.set_edge(vertex, -1, 0)
      count += tColumns
    return graph

  def longest_dag_path(self, source, dest):
    ''' Given that self is a dag, find the longest path
        between source and dest. '''
    tDist = {}
    tPrev = {}
    tUnvisited = []
    for tKey in self._Vertices:
      tDist[tKey] = - float("inf")
      #tPrev[tKey] = - float("inf")
      tUnvisited.append(tKey)
    tDist[source] = 0
    while tUnvisited:
      tVisit = max(tUnvisited, key=lambda x: tDist[x])
      tUnvisited.remove(tVisit)
      tEdges = self._EdgeMap[tVisit]
      for entry in tEdges.items():
        (tNeighbor, tLeng) = entry
        weight = -(tDist[tVisit] + tLeng)
        if weight > tDist[tNeighbor]:
          tPrev[tNeighbor] = tVisit
          tDist[tNeighbor] = weight
    tTree = tPrev
    tPath = []
    tPrev = dest
    while tPrev != source:
      tPath.append(tPrev)
      tTemp = tPrev
      tPrev = tTree[tPrev]
    tPath.append(tPrev)
    tPath.reverse()
    return tPath

  def set_edge(self, aX, aY, weight):
    ''' Set the weight of aX -> aY to weight. '''
    self._Vertices.add(aX)
    self._Vertices.add(aY)
    if aX in self._EdgeMap:
      self._EdgeMap[aX][aY] = int(weight)
    else:
      self._EdgeMap[aX] = {aY : int(weight)}
    if not aY in self._EdgeMap:
      self._EdgeMap[aY] = {}

  def shortest_path(self, source, dest):
    ''' Return the nodes along the shortest path from
        source to dest. '''
    (tWeight, tTree) = self.dijkstra(source)
    tPath = []
    tPrev = dest
    tDist = tWeight[tPrev]
    while tPrev != source:
      tPath.append(tPrev)
      tTemp = tPrev
      tPrev = tTree[tPrev]
      tDist = tWeight[tTemp]
    tPath.append(tPrev)
    tPath.reverse()
    return tPath

#########################

class Hexagonals(object):
  ''' Provides iterators and accessors for
      the hexagonal numbers. '''

  def __contains__(self, aInt):
    guess = (math.sqrt(8 * aInt + 1) + 1)/4
    return guess.is_integer()

  def __getitem__(self, n):
    return n * (2*n - 1)

  def __iter__(self):
    count = 1
    while True:
      yield self[count]
      count += 1

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

class Triangulars(object):
  ''' Provides iterators and accessors for
      the triangular numbers. '''

  def __contains__(self, aInt):
    if aInt < 0:
      return False
    guess = int((aInt) ** (.5))
    while self[guess] < aInt:
      guess += 1
    return self[guess] == aInt

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

def find_largest_palindrome_product(aMin, aMax):
  ''' Find the largest number P, which is a
      palindrome. Given P == A * B and
      aMin <= A < B <= aMax. '''
  first = aMax
  largest = 1
  while first >= aMin:
    for second in range(first + 1, aMax + 1):
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

def get_amicable_pair(low):
  ''' If low is the smallest number of an amicable pair
      return the pair, else return None. '''
  high = sum(proper_divisors(low))
  if low < high and sum(proper_divisors(high)) == low:
    return (low, high)
  return None

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

def is_pandigital(num):
  ''' Check that an n-digit number contains the digits [1, n]
      once and only once. As a side note, 8 and 9 digit
      pandigitals are never prime. '''
  strn = str(num)
  return all((str(digit) in strn for digit in range(1, len(strn) + 1)))

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

def is_square(num):
  ''' Check if num is a square number. '''
  root = int(math.sqrt(num) + .5)
  return num == root * root

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

def main():
  ''' main '''
  print("REDACTED")
  '''sums = 0
  for num in range(1, 17):
    print(num, hexadecimal_strings(num, 3))
    sums += hexadecimal_strings(num, 3)
  print(sums)
  print(hex(sums))'''
  

if __name__ == "__main__":
  main()
