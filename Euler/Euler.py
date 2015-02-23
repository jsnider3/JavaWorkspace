import math
import numpy

def alphabetScore(aWord):
  aWord = aWord.lower()
  tSum = 0
  for tChar in aWord:
    tCharVal = ord(tChar) - ord('a') + 1
    assert(tCharVal > -1)
    tSum = tSum + tCharVal
  return tSum

def digits(aNum):
  return len(str(aNum))

def isArithmeticallyIncreasing(aList):
  assert(len(aList) > 1)
  tList = sorted(aList)
  tDiff = tList[1] - tList[0]
  for tIndex in xrange(0, len(tList) - 1):
    tNewDiff = tList[tIndex + 1] - tList[tIndex]
    if tNewDiff != tDiff:
      return False
  return True

def fibonacci (aTerm):
  #tPhi = (1 + math.sqrt(5))/2
  #return int((tPhi ** aTerm - (-tPhi) ** (-aTerm))/math.sqrt(5))
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

def isPalindrome(aArg):
	tStr = str(aArg)
	tLen = len(tStr)
	for tIndex in range(tLen):
		if tStr[tIndex] != tStr[tLen - (tIndex + 1)]:
			return False
	return True

def isPrime(aInt):
  for tCount in xrange(2, int(aInt ** (.5) + 1)):
    if aInt % tCount == 0:
      return False
  return True

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

def primesInRange(aMin, aMax):
  tPrimes = []
  for tCount in xrange(aMin, aMax):
    if isPrime(tCount):
      tPrimes.append(tCount)
  return tPrimes  

def primesLessThan(aMax):
  return primesInRange(2, aMax)

def squareSum(aNum):
  tSum = 0
  for tCount in xrange(1, aNum + 1):
    tSum = tSum + tCount
  return tSum ** 2

def sumSquares(aNum):
  tSum = 0
  for tCount in xrange(1, aNum + 1) :
    tSum = tSum + (tCount ** 2)
  return tSum

print("REDACTED")
