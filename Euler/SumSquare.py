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

tSumSquare = SumSquares(100)
tSquaredSum = SquareSum(100)
print("Sum of squares is " + str(tSumSquare))
print("Square of sum is " + str(tSquaredSum))
print("Difference is " + str(tSumSquare - tSquaredSum))
