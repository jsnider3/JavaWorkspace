def sumSquares(aNum):
  tSum = 0
  for tCount in xrange(1, aNum + 1) :
    tSum = tSum + (tCount ** 2)
  return tSum

def squareSum(aNum):
  tSum = 0
  for tCount in xrange(1, aNum + 1):
    tSum = tSum + tCount
  return tSum ** 2

def primeFactors(aInt):
  for tCount in xrange(2, aInt + 1):
    if aInt % tCount == 0:
      return primeFactors(aInt / tCount).append(tCount)    
  return []

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

def primesLessThan(aMax):
  tPrimes = []
  for tCount in xrange(2, aMax):
    if isPrime(tCount):
      tPrimes.append(tCount)
  return tPrimes  

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

def findLargestPalindromeProduct(aMin, aMax):
	tFirst = aMax
	tLargest = 1
	while tFirst >= aMin:
		for tSecond in xrange(tFirst + 1, aMax + 1):
			tProduct = tFirst * tSecond
			if isPalindrome(tProduct) and tProduct > tLargest:
				print(str(tProduct) + " is the product of " + str((tFirst, tSecond)) + ".")				
				tLargest = tProduct
		tFirst = tFirst - 1
	return tLargest
assert(isPalindrome(101))
print(findLargestPalindromeProduct(100, 999))
