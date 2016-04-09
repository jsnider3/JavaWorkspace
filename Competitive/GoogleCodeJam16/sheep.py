'''
  Solve the counting sheep problem
  from Google Code Jam 2016.

  @author: Josh Snider

'''

''' Given a start >= 0, print n such that
    [start, start * 2, start * 3, ... n] contains
    all the digits 0-9.

    I've convinced myself that there is a finite
    solution for all starts > 0.'''
def count_sheep(start):
  seen = set([])
  if start == 0:
    return None
  count = start
  while True:
    for d in str(count):
      seen.add(d)
    if len(seen) == 10:
      return count
    count += start

def digits(num):
  return [d for d in str(num)]

if __name__ == '__main__':
  numtests = int(input(''))
  for n in range(numtests):
    res = count_sheep(int(input('')))
    if res == None:
      res = "INSOMNIA"
    res = str(res)
    print ('Case #' + str(n+1) + ": " + res)
