def SumSquares(aNum):
  tSum = 0
  for tCount in xrange(1, aNum + 1) :
    tSum = tSum + (tCount ** 2)
  return tSum

def SquareSum(aNum):
  tSum = 0
  for tCount in xrange(1, aNum + 1):
    tSum = tSum + tCount
  return tSum ** 2

def PrimeFactors(aInt):
  for tCount in xrange(2, aInt + 1):
    if aInt % tCount == 0:
      return [tCount] + PrimeFactors(aInt / tCount)    
  return []

print(max(PrimeFactors(600851475143)))
