def alphabetScore(aWord):
  aWord = aWord.lower()
  tSum = 0
  for tChar in aWord:
    tCharVal = ord(tChar) - ord('a') + 1
    assert(tCharVal > -1)
    tSum = tSum + tCharVal
  return tSum

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

def lowestCommonMultiple(aNumbers):
  tCommonFactors = []
  for tNumber in aNumbers:
    tFactors = primeFactors(tNumber)
    for tFactor in tFactors:
      while tCommonFactors.count(tFactor) < tFactors.count(tFactor):
        tCommonFactors.append(tFactor)
  return listProduct(tCommonFactors)
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

def primesLessThan(aMax):
  tPrimes = []
  for tCount in xrange(2, aMax):
    if isPrime(tCount):
      tPrimes.append(tCount)
  return tPrimes  

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
