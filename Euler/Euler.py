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

print(sum(primesLessThan(2000000)))
