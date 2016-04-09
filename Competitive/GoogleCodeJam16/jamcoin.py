'''
  Solve the coin jam problem
  from Google Code Jam 2016.

  @author: Josh Snider

'''


class Primes(object):
  ''' Utility class for generating primes, checking
      the primality of numbers, and exploring other
      features of primes.'''

  cache = set([2, 3])

  def __contains__(self, num):
    if num < 2:
      return False
    if num in self.cache:
      return True
    if num % 2 == 0:
      return False
    for count in self.cache:
      if num % count == 0:
        return False
    for count in range(max(self.cache), int(num ** (.5) + 1), 2):
      if num % count == 0:
        return False
    return True

  def __iter__(self):
    for num in sorted(list(self.cache)):
      yield num
    count = max(self.cache) + 2
    while True:
      if count in self:
        if not count in self.cache:
          self.cache.add(count)
        yield count
      count += 2

  def composites(self, start, end):
    ''' Quickly generate composites in [start, end]. '''
    itr = iter(self)
    p = next(itr)
    while p <= end:
      if p >= start:
        yield p
        for nxt in self.composites(start, end // p):
          yield nxt * p
      p = next(itr)

  def low_factor(self, num):
    ''' Return num's lowest factor. '''
    if num == 0 or num == 1:
      return []
    for count in self:
      if count > int(num ** .5) + 2:
        break
      elif (num % count) == 0:
        return count
    return num

primes = Primes()

class JamCoin(object):

  def __init__(self, val, proof):
    self.val = val
    self.proof = proof

def bin_generator(size):
  ''' Return the binary numbers with
      n digits that start and end
      with '1'. '''
  assert size > 2
  for num in xrange(2 ** (size - 2)):
    yield '1' + bin(num)[2:].zfill(size - 2) + '1'

def is_jamcoin(bstr):
  '''
    Does a binary string represent a jam coin?
  '''
  jamcoin = True
  easy_reqs = (len(bstr) == bstr.count('0') + bstr.count('1') and
               bstr[0] == '1' and bstr[-1] == '1')
  if easy_reqs:
    for base in range(2, 11):
      if int(bstr, base) in primes:
        jamcoin = False
        break
  return jamcoin and easy_reqs

def make_jamcoins(size, count):
  vals = []
  for n in bin_generator(size):
    if is_jamcoin(n):
      vals.append(n)
      if len(vals) == count:
        break
  coins = []
  for val in vals:
    coins.append(JamCoin(val, make_jamproof(val)))
  return coins

def make_jamproof(num):
  return [primes.low_factor(int(num, base)) for base in range(2, 11)]

if __name__ == '__main__':
  numtests = int(input(''))
  for n in range(numtests):
    [size, count] = [int(s) for s in raw_input('').split(' ')]
    coins = make_jamcoins(size, count)
    print ('Case #' + str(n+1) + ":")
    for coin in coins:
      print (coin.val + " " + " ".join(str(p) for p in coin.proof))
