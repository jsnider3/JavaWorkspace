import datetime
import math
import numpy
import pdb

class Collatz:
  def __init__(self):
    self._Depths = {1 : 1}
  
  def next(self, aInt):
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

  def maxDepth(self):
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

  def dijkstra(self, aSource):
    tDist = {}
    tPrev = {}
    tUnvisited = []
    #TODO Fixme
    for tKey in self._EdgeMap.keys():
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

  def dumpEdges(self):
    print(self._EdgeMap)

  @staticmethod
  def fromMatrix(aMatrix):
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
          tGraph.setEdge(tLeft, tVertex, tWeight)
        if tY > 0 and tUp >= 0:
          tGraph.setEdge(tUp, tVertex, tWeight)
        if tX < tRows - 1 and tRight < tNumVertices:
          tGraph.setEdge(tRight, tVertex, tWeight)
        if tY < tRows - 1 and tDown < tNumVertices:
          tGraph.setEdge(tDown, tVertex, tWeight)
    return tGraph
  
  def setEdge(self, aX, aY, aWeight):
    if aX in self._EdgeMap:
      self._EdgeMap[aX][aY] = int(aWeight)
    else:
      self._EdgeMap[aX] = {aY : int(aWeight)}

  def shortestPath(self, aSource, aDest):
    (tWeight, tTree) = self.dijkstra(aSource)
    tPath = []
    tPrev = aDest
    tDist = tWeight[tPrev]
    while tPrev != aSource:
      tPath.append(tPrev)
      tTemp = tPrev
      tPrev = tTree[tPrev]
      tDist = tWeight[tTemp]# - tWeight[tPrev]
    tPath.append(tPrev)
    tPath.reverse()
    return tPath

#########################

def alphabetScore(aWord):
  aWord = aWord.lower()
  tSum = 0
  for tChar in aWord:
    tCharVal = ord(tChar) - ord('a') + 1
    assert(tCharVal > -1)
    tSum = tSum + tCharVal
  return tSum

def champernowne(aDigit):
  assert(aDigit > 0)
  tCount = 1
  tStr = ""
  while len(tStr) < aDigit:
    tStr = tStr + str(tCount)
    tCount = tCount + 1
  return int(tStr[aDigit - 1])

def coprime(aFirst, aSecond):
  return greatestCommonDivisor(aFirst, aSecond) == 1 

def fibonacci (aTerm):
  tFirst = 0
  tSecond = 1
  tCount = 1
  while tCount < aTerm:
    tNext = tFirst + tSecond
    tFirst = tSecond
    tSecond = tNext
    tCount = tCount + 1
  return tSecond

def findLargestPalindromeProduct(aMin, aMax):
	tFirst = aMax
	tLargest = 1
	while tFirst >= aMin:
		for tSecond in xrange(tFirst + 1, aMax + 1):
			tProduct = tFirst * tSecond
			if isPalindrome(tProduct) and tProduct > tLargest:				
				tLargest = tProduct
		tFirst = tFirst - 1
	return tLargest

def findPythagTriplet(aSum):
  tMax = int(aSum / 3) + 1
  for tA in xrange(1, aSum):
    for tB in xrange(tA + 1, aSum - tA + 1):
      for tC in xrange(tB + 1, aSum - tA - tB + 1):
        if tA ** 2 + tB ** 2 == tC ** 2:
          if tA + tB + tC == 1000:
            return [tA, tB, tC]

def findRightTriangles(aPerim):
  tTriangles = []
  for tA in range(1, aPerim/3):
    for tB in range(1, aPerim/2):
      tC = math.sqrt(tA ** 2 + tB ** 2)
      if tC.is_integer() and tA + tB + tC == aPerim:
        tTriangles.append((tA, tB, tC))
  return tTriangles

def firstIncorrectTerm(aApprox, aFunc):
  tCount = 1
  while aApprox(tCount) == aFunc(tCount):
    tCount = tCount + 1
  return aApprox(tCount)

def fitPolynomial(aTerms):
  tXes = range(1, len(aTerms) + 1)
  return lambda x: int(.5 + numpy.polyval(
                        numpy.polyfit(tXes, aTerms, len(aTerms) - 1), x))

def greatestCommonDivisor(aFirst, aSecond):
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

def groupIntoEquivalencyClasses(aList, aEquals):
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

def isAnagram(aFirst, aSecond):
  tFirst = str(aFirst)
  tSecond = str(aSecond)
  for tChar in tFirst + tSecond:
    if tFirst.count(tChar) != tSecond.count(tChar):
      return False
  return True

def isArithmeticallyIncreasing(aList):
  assert(len(aList) > 1)
  tList = sorted(aList)
  tDiff = tList[1] - tList[0]
  for tIndex in xrange(0, len(tList) - 1):
    tNewDiff = tList[tIndex + 1] - tList[tIndex]
    if tNewDiff != tDiff:
      return False
  return True

def isPalindrome(aArg):
	tStr = str(aArg)
	tLen = len(tStr)
	for tIndex in range(tLen):
		if tStr[tIndex] != tStr[tLen - (tIndex + 1)]:
			return False
	return True

def isPowerOf(aNum, aBase):
  tCount = 0
  while aBase ** tCount < aNum:
    tCount = tCount + 1
  return aBase ** tCount == aNum

def isPrime(aInt):
  if aInt < 2:
    return False
  for tCount in xrange(2, int(aInt ** (.5) + 1)):
    if aInt % tCount == 0:
      return False
  return True

def isRotation(aFirst, aSecond):
  tFirst = str(aFirst)
  tSecond = str(aSecond)
  if len(tFirst) != len(tSecond):
    return False
  return tSecond in tFirst + tFirst

def isSquare(aNum):
  tSqrt = int(math.sqrt(aNum) + .5)
  return aNum == tSqrt * tSqrt

def largestProductInSeries(aSeries, aLength):
  assert(len(aSeries) >= aLength)
  tProduct = 1
  for tCount in range(aLength):
    tProduct = tProduct * aSeries[tCount]
  tLargest = tProduct
  for tCount in xrange(aLength + 1, len(aSeries) - aLength):
    tProduct = 1
    for tCount in xrange(tCount, tCount + aLength):
	    tProduct = tProduct * aSeries[tCount]
    if tProduct > tLargest:
      tLargest = tProduct
  return tLargest

def listProduct(aList):
  return reduce(lambda x, y: x * y, aList)

def longestArithmeticallyIncreasingSequence(aList):
  if len(aList) < 2:
    return aList
  tMaxLen = 1
  tMaxDiff = aList[1] - aList[0]
  tMaxStart = aList[0]
  for tLow in range(len(aList)):
    for tHigh in xrange(tLow + 1, len(aList)):
      tDiff = aList[tHigh] - aList[tLow]
      tCount = 0
      while aList[tLow] + tDiff * (tCount + 1) in aList:
        tCount = tCount + 1
      if tCount > tMaxLen:
        tMaxLen = tCount
        tMaxDiff = tDiff
        tMaxStart = aList[tLow]
  tSequence = []
  for tCount in xrange(0, tMaxLen + 1):
    tSequence.append(tMaxStart + tMaxDiff * tCount)
  return tSequence

def lowestCommonMultiple(aNumbers):
  tCommonFactors = []
  for tNumber in aNumbers:
    tFactors = primeFactors(tNumber)
    for tFactor in tFactors:
      while tCommonFactors.count(tFactor) < tFactors.count(tFactor):
        tCommonFactors.append(tFactor)
  return listProduct(tCommonFactors)

def makeChange(aCoins, aTotal):
  tNumCoins = len(aCoins)
  tSolution = numpy.zeros((aTotal + 1, tNumCoins))
  for tIndex in range(tNumCoins):
    tSolution[0][tIndex] = 1
  for tLoop in xrange(1, aTotal + 1):
    for tIndex in range(tNumCoins):
      tX = 0
      if tLoop - aCoins[tIndex] >= 0:
        tX = tSolution[tLoop - aCoins[tIndex]][tIndex]
      tY = 0
      if tIndex > 0:
        tY = tSolution[tLoop][tIndex - 1]
      tSolution[tLoop][tIndex] = tX + tY
  return int(tSolution[aTotal][tNumCoins - 1])

def nthPrime(aNth):
  assert(aNth > 0)
  tPrime = 1
  tCount = 2
  tFound = 0
  while tFound < aNth:
    if isPrime(tCount):
      tPrime = tCount
      tFound = tFound + 1
    tCount = tCount + 1
  return tPrime

def numDigits(aNum):
  return len(str(aNum))

def pointsOnSlope(aRise, aRun):
  tPoints = 1 + greatestCommonDivisor(aRise, aRun)
  return tPoints

def primeFactors(aInt):
  if(aInt == 0):
    return []
  if(aInt == 1):
    return [1]
  for tCount in xrange(2, aInt + 1):
    if (aInt % tCount) == 0:
      tFactors = primeFactors(aInt / tCount)
      tFactors.append(tCount)
      return tFactors
  return [aInt]

def primes():
  tCount = 2
  while True:
    if isPrime(tCount):
      yield tCount
    tCount = tCount + 1

def primesInRange(aMin, aMax):
  tPrimes = []
  for tCount in xrange(aMin, aMax):
    if isPrime(tCount):
      tPrimes.append(tCount)
  return tPrimes  

def primesLessThan(aMax):
  return primesInRange(2, aMax)

def resilience(aDenom):
  #FIXME Infeasible.
  assert(aDenom > 1)
  tNumers = range(1, aDenom)
  tTests = map(lambda x: coprime(aDenom, x), tNumers)
  tTemp = len(filter(lambda x: x, tTests))
  return float(tTemp) / (aDenom - 1)

tMemoCuts = {}

def rodCuts(aLength, aCutSize):
  #FIXME Buggy
  tCurry = (aLength, aCutSize)
  if tCurry in tMemoCuts:
    return tMemoCuts.get(tCurry)
  tCuts = 1
  for tIndex in range(aLength - aCutSize):
    tCuts = tCuts + rodCuts(aLength - aCutSize, aCutSize)
  tMemoCuts[tCurry] = tCuts
  return tCuts

def squareSum(aNum):
  tSum = 0
  for tCount in xrange(1, aNum + 1):
    tSum = tSum + tCount
  return tSum ** 2

def sumDigits(aInt):
  tStr = str(aInt)
  tSum = 0
  for tChar in tStr:
    tSum = tSum + int(tChar)
  return tSum

def sumSquares(aNum):
  tSum = 0
  for tCount in xrange(1, aNum + 1) :
    tSum = tSum + (tCount ** 2)
  return tSum

def quadPrime(aA, aB):
  tFunc = lambda x: x**2 + aA * x + aB
  tCount = 0
  while isPrime(tFunc(tCount)):
    tCount = tCount + 1
  return tCount

print("REDACTED")

