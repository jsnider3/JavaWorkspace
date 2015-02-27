import functools
import math
import numpy

class Collatz:
  def __init__(self):
    self._Depths = {1 : 1}
  
  @staticmethod
  def next(aInt):
    assert(aInt > 0)
    if aInt % 2 == 0:
      return aInt / 2
    else:
      return (3 * aInt + 1) / 2

  def depth(self, aInt):
    if aInt in self._Depths:
      return self._Depths.get(aInt)
    else:
      self._Depths[aInt] = self.depth(self.next(aInt)) + 1
      return self._Depths[aInt]

  def max_depth(self):
    tMax = 1
    tMaxNum = 1
    for tEntry in self._Depths.items():
      (tKey, tValue) = tEntry
      if tValue > tMax:
        tMax = tValue
        tMaxNum = tKey
    return tMaxNum

#########################

class Graph:
  def __init__(self):
    self._EdgeMap = {}
    self._Vertices = set([])

  def dijkstra(self, aSource):
    tDist = {}
    tPrev = {}
    tUnvisited = []
    #TODO Fixme
    for tKey in self._Vertices:
      tDist[tKey] = float("inf")
      tPrev[tKey] = float("inf")
      tUnvisited.append(tKey)
    tDist[aSource] = 0
    tDist[-2] = float("inf")
    while tUnvisited:
      tVisit = min(tUnvisited, key = lambda x: tDist[x])
      tUnvisited.remove(tVisit)
      tEdges = self._EdgeMap[tVisit]
      for tEntry in tEdges.items():
        (tNeighbor, tLeng) = tEntry
        tWeight = tDist[tVisit] + tLeng
        if tWeight < tDist[tNeighbor]:
          tPrev[tNeighbor] = tVisit
          tDist[tNeighbor] = tWeight
    return (tDist, tPrev)

  def dump_edges(self):
    print(self._EdgeMap)

  @staticmethod
  def from_matrix(aMatrix):
    tGraph = Graph()
    tRows = len(tMatrix)
    tNumVertices = tRows * len(tMatrix[0])
    tFunc = lambda x, y: tRows * y + x
    tGraph = Graph()
    for tX in range(tRows):
      for tY in range(tRows):
        tVertex = tFunc(tX, tY)
        tLeft = tFunc(tX - 1, tY)
        tUp = tFunc(tX, tY - 1)
        tRight = tFunc(tX + 1, tY)
        tDown = tFunc(tX, tY + 1)
        tWeight = tMatrix[tY][tX]
        if tX > 0 and tLeft >= 0:
          tGraph.set_edge(tLeft, tVertex, tWeight)
        if tY > 0 and tUp >= 0:
          tGraph.set_edge(tUp, tVertex, tWeight)
        if tX < tRows - 1 and tRight < tNumVertices:
          tGraph.set_edge(tRight, tVertex, tWeight)
        if tY < tRows - 1 and tDown < tNumVertices:
          tGraph.set_edge(tDown, tVertex, tWeight)
    return tGraph
  
  @staticmethod
  def from_triangle(aTriangle):
    tCount = 0
    tRows = len(aTriangle)
    tGraph = Graph()
    for tRow in range(tRows):
      tColumns = len(aTriangle[tRow])
      if tRow != tRows - 1:
        tNextRow = aTriangle[tRow + 1]
        for tX in range(tColumns):
          tVertex = tCount + tX
          tLeftChild = tVertex + tColumns
          tGraph.set_edge(tVertex, tLeftChild, tNextRow[tX])
          tGraph.set_edge(tVertex, tLeftChild + 1, tNextRow[tX + 1])
      else:
        for tX in range(tColumns):
          tVertex = tCount + tX
          tGraph.set_edge(tVertex, -1, 0) 
      tCount += tColumns
    return tGraph

  def longest_dag_path(self, aSource, aDest):
    tDist = {}
    tPrev = {}
    tUnvisited = []
    for tKey in self._Vertices:
      tDist[tKey] = - float("inf")
      #tPrev[tKey] = - float("inf")
      tUnvisited.append(tKey)
    tDist[aSource] = 0
    while tUnvisited:
      tVisit = max(tUnvisited, key = lambda x: tDist[x])
      tUnvisited.remove(tVisit)
      tEdges = self._EdgeMap[tVisit]
      for tEntry in tEdges.items():
        (tNeighbor, tLeng) = tEntry
        tWeight = -(tDist[tVisit] + tLeng)
        if tWeight > tDist[tNeighbor]:
          tPrev[tNeighbor] = tVisit
          tDist[tNeighbor] = tWeight
    (tWeight, tTree) = (tDist, tPrev)
    tPath = []
    tPrev = aDest
    tDist = tWeight[tPrev]
    while tPrev != aSource:
      tPath.append(tPrev)
      tTemp = tPrev
      tPrev = tTree[tPrev]
      tDist = tWeight[tTemp]
    tPath.append(tPrev)
    tPath.reverse()
    return tPath

  def set_edge(self, aX, aY, aWeight):
    self._Vertices.add(aX)
    self._Vertices.add(aY)
    if aX in self._EdgeMap:
      self._EdgeMap[aX][aY] = int(aWeight)
    else:
      self._EdgeMap[aX] = {aY : int(aWeight)}
    if not aY in self._EdgeMap:
      self._EdgeMap[aY] = {}

  def shortest_path(self, aSource, aDest):
    (tWeight, tTree) = self.dijkstra(aSource)
    tPath = []
    tPrev = aDest
    tDist = tWeight[tPrev]
    while tPrev != aSource:
      tPath.append(tPrev)
      tTemp = tPrev
      tPrev = tTree[tPrev]
      tDist = tWeight[tTemp]
    tPath.append(tPrev)
    tPath.reverse()
    return tPath

#########################

class Primes(object):

  def __contains__(self, aInt):
    if aInt < 2:
      return False
    if aInt % 2 == 0:
      return False
    for tCount in range(3, int(aInt ** (.5) + 1), 2):
      if aInt % tCount == 0:
        return False
    return True

  def __getitem__(aNth):
    assert(aNth > 0)
    tPrime = 1
    tCount = 2
    tFound = 0
    while tFound < aNth:
      if tCount in self:
        tPrime = tCount
        tFound = tFound + 1
      tCount = tCount + 1
    return tPrime
  
  def __iter__(self):
    yield 2
    tCount = 3
    while True:
      if tCount in self:
        yield tCount
      tCount += 2

  @staticmethod
#  @functools.lru_cache()
  def factors(aInt):
    if aInt == 0 :
      return []
    if aInt == 1 :
      return []
    if aInt % 2 == 0:
      tFactors = Primes.factors(aInt // 2)
      tFactors.append(2)
      return tFactors
    for tCount in range(3, aInt + 1, 2):
      if (aInt % tCount) == 0:
        tFactors = Primes.factors(aInt // tCount)
        tFactors.append(tCount)
        return tFactors
    return [aInt]

  def in_range(aMin, aMax):
    tPrimes = []
    for tCount in range(aMin, aMax):
      if tCount in self:
        tPrimes.append(tCount)
    return tPrimes  

  def less_than(aMax):
    return in_range(2, aMax)
  
  @staticmethod 
  def num_divisors(aNum):
    tPrimeFactors = Primes.factors(aNum)
    tSet = set(tPrimeFactors)
    tFactors = 1
    for tFactor in tSet:
      tFactors *= tPrimeFactors.count(tFactor) + 1
    print(tPrimeFactors)
    print(aNum, tFactors)
    return tFactors

#########################

class Triangulars(object):

  def __contains__(self, aInt):
    if aInt < 0:
      return False
    tGuess = int((aInt) ** (.5))
    while self[tGuess] < aInt:
      tGuess += 1
    return self[tGuess] == aInt

  def __getitem__(self, aInt):
    return int(.5 * aInt * (aInt + 1))

  def __iter__(self):
    tCount = 0
    tSum = 0
    while True:
      tSum += tCount
      tCount += 1
      yield tSum

#########################

def alphabet_score(aWord):
  aWord = aWord.lower()
  tSum = 0
  for tChar in aWord:
    tCharVal = ord(tChar) - ord('a') + 1
    assert(tCharVal > -1)
    tSum = tSum + tCharVal
  return tSum

def british_number_string(aNum):
  tNum = int(aNum)
  tStr = str(tNum)
  tWords = []
  tTens = {"1" : "ten", "2" : "twenty", "3" : "thirty", "4" : "forty",
           "5" : "fifty", "6" : "sixty", "7" : "seventy", "8" : "eighty",
           "9" : "ninety", "11" : "eleven", "12" : "twelve", "13" : "thirteen",
           "14" : "fourteen", "15" : "fifteen", "16" : "sixteen", 
           "17" : "seventeen", "18" : "eighteen", "19" : "ninteen"}
  tOnes = {"1" : "one", "2" : "two", "3" : "three", "4" : "four",
           "5" : "five", "6" : "six", "7" : "seven", "8" : "eight",
           "9" : "nine"}
  if len(tStr) == 4:
    tWords.append(tOnes[tStr[0]])
    tWords.append("thousand")
    tStr = tStr[1:]
  if len(tStr) == 3:
    if tStr[0] in tOnes:
      tWords.append(tOnes[tStr[0]])
      tWords.append("hundred")
    if tStr[1:] != "00":
      tWords.append("and")
    tStr = tStr[1:]
  if len(tStr) == 2:
    if tStr in tTens:
      tWords.append(tTens[tStr])
      tStr = tStr[2:]
    else:
      if tStr[0] in tTens:
        tWords.append(tTens[tStr[0]])
      tStr = tStr[1:]
  if len(tStr) == 1:
    if tStr[0] in tOnes:
      tWords.append(tOnes[tStr[0]])
    tStr = tStr[1:]
  tStr = "".join(tWords)
  print(tStr)
  return tStr

def champernowne(aDigit):
  assert(aDigit > 0)
  tCount = 1
  tStr = ""
  while len(tStr) < aDigit:
    tStr = tStr + str(tCount)
    tCount = tCount + 1
  return int(tStr[aDigit - 1])

def coprime(aFirst, aSecond):
  return greatest_common_divisor(aFirst, aSecond) == 1 

def fibonacci(aTerm):
  tFirst = 0
  tSecond = 1
  tCount = 1
  while tCount < aTerm:
    tNext = tFirst + tSecond
    tFirst = tSecond
    tSecond = tNext
    tCount = tCount + 1
  return tSecond

def find_largest_palindrome_product(aMin, aMax):
	tFirst = aMax
	tLargest = 1
	while tFirst >= aMin:
		for tSecond in range(tFirst + 1, aMax + 1):
			tProduct = tFirst * tSecond
			if is_palindrome(tProduct) and tProduct > tLargest:				
				tLargest = tProduct
		tFirst = tFirst - 1
	return tLargest

def find_pythag_triplet(aSum):
  tMax = int(aSum / 3) + 1
  for tA in range(1, aSum):
    for tB in range(tA + 1, aSum - tA + 1):
      for tC in range(tB + 1, aSum - tA - tB + 1):
        if tA ** 2 + tB ** 2 == tC ** 2:
          if tA + tB + tC == 1000:
            return [tA, tB, tC]

def find_right_triangles(aPerim):
  tTriangles = []
  for tA in range(1, aPerim/3):
    for tB in range(1, aPerim/2):
      tC = math.sqrt(tA ** 2 + tB ** 2)
      if tC.is_integer() and tA + tB + tC == aPerim:
        tTriangles.append((tA, tB, tC))
  return tTriangles

def first_incorrect_term(aApprox, aFunc):
  tCount = 1
  while aApprox(tCount) == aFunc(tCount):
    tCount = tCount + 1
  return aApprox(tCount)

def fit_polynomial(aTerms):
  tXes = range(1, len(aTerms) + 1)
  return lambda x: int(.5 + numpy.polyval(
                        numpy.polyfit(tXes, aTerms, len(aTerms) - 1), x))

def flat_index(aTwoDShape, aIndex):
  tIndex = aIndex
  for tRow in aTwoDShape: 
    if tIndex < len(tRow):
      return tRow[tIndex]
    else:
      tIndex -= len(tRow)

def get_amicable_pair(low):
  high = sum(proper_divisors(low))
  if low < high and sum(proper_divisors(high)) == low:
    return (low, high)
  return None

def greatest_common_divisor(aFirst, aSecond):
  tMax = max(aFirst, aSecond)
  tMin = min(aFirst, aSecond)
  while tMin != 0:
    assert(tMin > 0)
    tTemp = tMin
    tMin = tMax - tMin
    tMax = tTemp    
    if tMin > tMax:
      tMax, tMin = tMin, tMax
  return tMax

def group_into_equivalency_classes(aList, aEquals):
  tClasses = []
  for tElem in aList:
    tFound = False
    for tClass in tClasses:
      if aEquals(tClass[0], tElem):
        tFound = True
        tClass.append(tElem)
        break
    if not tFound:
      tClasses.append([tElem])
  return tClasses 

def is_anagram(aFirst, aSecond):
  tFirst = str(aFirst)
  tSecond = str(aSecond)
  for tChar in tFirst + tSecond:
    if tFirst.count(tChar) != tSecond.count(tChar):
      return False
  return True

def is_arithmetically_increasing(aList):
  assert(len(aList) > 1)
  tList = sorted(aList)
  tDiff = tList[1] - tList[0]
  for tIndex in range(0, len(tList) - 1):
    tNewDiff = tList[tIndex + 1] - tList[tIndex]
    if tNewDiff != tDiff:
      return False
  return True

def is_pandigital(aNum):
  #As a side note, 8 and 9 digit pandigitals are never prime.
  tStr = str(aNum)
  return all((str(tDigit) in tStr for tDigit in range(1, len(tStr) + 1)))

def is_palindrome(aArg, base=10):
  tStr = str(aArg)
  if base == 2:
    tStr = "{0:b}".format(aArg)
  return tStr == tStr[::-1]

def is_power_of(aNum, aBase):
  tCount = 0
  while aBase ** tCount < aNum:
    tCount = tCount + 1
  return aBase ** tCount == aNum

def is_rotation(aFirst, aSecond):
  tFirst = str(aFirst)
  tSecond = str(aSecond)
  if len(tFirst) != len(tSecond):
    return False
  return tSecond in tFirst + tFirst

def is_square(aNum):
  tSqrt = int(math.sqrt(aNum) + .5)
  return aNum == tSqrt * tSqrt

def largest_product_in_series(aSeries, aLength):
  assert(len(aSeries) >= aLength)
  tProduct = 1
  for tCount in range(aLength):
    tProduct = tProduct * aSeries[tCount]
  tLargest = tProduct
  for tCount in range(aLength + 1, len(aSeries) - aLength):
    tProduct = 1
    for tCount in range(tCount, tCount + aLength):
	    tProduct = tProduct * aSeries[tCount]
    if tProduct > tLargest:
      tLargest = tProduct
  return tLargest

def longest_arithmetically_increasing_sequence(aList):
  if len(aList) < 2:
    return aList
  tMaxLen = 1
  tMaxDiff = aList[1] - aList[0]
  tMaxStart = aList[0]
  for tLow in range(len(aList)):
    for tHigh in range(tLow + 1, len(aList)):
      tDiff = aList[tHigh] - aList[tLow]
      tCount = 0
      while aList[tLow] + tDiff * (tCount + 1) in aList:
        tCount = tCount + 1
      if tCount > tMaxLen:
        tMaxLen = tCount
        tMaxDiff = tDiff
        tMaxStart = aList[tLow]
  tSequence = []
  for tCount in range(0, tMaxLen + 1):
    tSequence.append(tMaxStart + tMaxDiff * tCount)
  return tSequence

def lowest_common_multiple(aNumbers):
  tCommonFactors = []
  for tNumber in aNumbers:
    tFactors = Primes.factors(tNumber)
    for tFactor in tFactors:
      while tCommonFactors.count(tFactor) < tFactors.count(tFactor):
        tCommonFactors.append(tFactor)
  return product(tCommonFactors)

def make_change(aCoins, aTotal):
  tNumCoins = len(aCoins)
  tSolution = numpy.zeros((aTotal + 1, tNumCoins))
  for tIndex in range(tNumCoins):
    tSolution[0][tIndex] = 1
  for tLoop in range(1, aTotal + 1):
    for tIndex in range(tNumCoins):
      tX = 0
      if tLoop - aCoins[tIndex] >= 0:
        tX = tSolution[tLoop - aCoins[tIndex]][tIndex]
      tY = 0
      if tIndex > 0:
        tY = tSolution[tLoop][tIndex - 1]
      tSolution[tLoop][tIndex] = tX + tY
  return int(tSolution[aTotal][tNumCoins - 1])

def number_spiral_sum(aRow):
  if aRow == 0:
    return 1
  else:
    n = 2 * aRow + 1
    return 4*(n ** 2) - 12*(aRow)

def num_digits(aNum):
  return len(str(aNum))

def points_on_slope(aRise, aRun):
  tPoints = 1 + greatest_common_divisor(aRise, aRun)
  return tPoints

def product(aList):
  return reduce(lambda x, y: x * y, aList)

def proper_divisors(num):
  divisors = []
  for x in range(1, num):
    if num % x == 0:
      divisors.append(x)
  return divisors

def quad_prime(aA, aB):
  tFunc = lambda x: x**2 + aA * x + aB
  tCount = 0
  while isPrime(tFunc(tCount)):
    tCount = tCount + 1
  return tCount

def resilience(aDenom):
  #FIXME Infeasible.
  assert(aDenom > 1)
  tNumers = range(1, aDenom)
  tTests = map(lambda x: coprime(aDenom, x), tNumers)
  tTemp = len(filter(lambda x: x, tTests))
  return float(tTemp) / (aDenom - 1)

@functools.lru_cache(maxsize=None)
def rod_cuts(aLength, aCutSize):
  tCurry = (aLength, aCutSize)
  tCuts = 1
  for tIndex in range(aLength - aCutSize):
    #TODO This is wrong obviously.
    tCuts = tCuts + rod_cuts(aLength - aCutSize, aCutSize)
  return tCuts

def square_sum(aNum):
  tSum = 0
  for tCount in range(1, aNum + 1):
    tSum = tSum + tCount
  return tSum ** 2

def sum_digits(aInt):
  tStr = str(aInt)
  tSum = 0
  for tChar in tStr:
    tSum = tSum + int(tChar)
  return tSum

def sum_squares(aNum):
  tSum = 0
  for tCount in range(1, aNum + 1) :
    tSum = tSum + (tCount ** 2)
  return tSum

def triangle_max_path(aTriangle):
  tTriangle = list(reversed(aTriangle))
  tMax = tTriangle[0][:]
  for tRow in range(len(tTriangle)):
    if tRow > 0:
      tCurRow = tTriangle[tRow]
      print(tRow)
      for tCol in range(len(tCurRow)):
        tMax[tCol] = tCurRow[tCol] + max(tMax[tCol], tMax[tCol + 1])
  tTriangle.reverse()
  return tMax[0]

def largest_grid_product(aGrid):
  #FIXME: That's not how lists of lists work in python.
  tRows = len(aGrid)
  tMaxProduct = -float("inf")
  for tY in range(tRows):
    tRow = aGrid[tY]
    tCols = len(tRow)
    for tX in range(tCols):
      if tY < tRows - 4:
        tProduct = product([aGrid[tY][tX], aGrid[tY+1][tX],
                               aGrid[tY+2][tX], aGrid[tY+3][tX]])
        tMaxProduct = max(tMaxProduct, tProduct)
      if tX < tCols - 4:
        tProduct = product([aGrid[tY][tX], aGrid[tY][tX+1],
                               aGrid[tY][tX+2], aGrid[tY][tX+3]])
        tMaxProduct = max(tMaxProduct, tProduct)
      if tY < tRows - 4 and tX < tCols - 4:
        tProduct = product([aGrid[tY][tX], aGrid[tY+1][tX+1],
                               aGrid[tY+2][tX+2], aGrid[tY+3][tX+3]])
        tMaxProduct = max(tMaxProduct, tProduct)
      if tY < tRows - 4 and tX - 4 >= 0:
        tProduct = product([aGrid[tY][tX], aGrid[tY+1][tX-1],
                               aGrid[tY+2][tX-2], aGrid[tY+3][tX-3]])
        tMaxProduct = max(tMaxProduct, tProduct)
  return tMaxProduct

def main():
  print("REDACTED")

if __name__ == "__main__":
  main()
