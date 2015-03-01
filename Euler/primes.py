class Primes(object):

  _List = set([2, 3])

  def consecutive_sum_max(self, num):
    '''Return num as a list of consecutive primes
       with maximum length. This is the same as
       starting with the lowest prime possible. 
       This method works, but is infeasible for
       large numbers.'''
    start = 1
    stop = 1
    accum = [self[start]]
    while True:
      if accum[-1] >= num:
        return 0
      if sum(accum) < num:
        stop += 1
        accum.append(self[stop])
        pass
      elif sum(accum) > num:
        accum.remove(accum[0])
        start += 1
      elif sum(accum) == num:
        return len(accum)

  def consecutive_sum_max_length(self, cutoff):
    start = 1
    maxtot = 2
    maxlen = 1
    while self[start] < cutoff/maxlen:
      total = self[start]
      length = 1
      while total < cutoff:
        total += self[start + length]
        length += 1
        if total < cutoff and length > maxlen and total in self:
          maxlen = length
          maxtot = total
          print((total,maxlen))
      start += 1
    return (maxtot, maxlen)


  def __contains__(self, num):
    if num < 2:
      return False
    if num in self._List:
      return True
    if num % 2 == 0:
      return False
    for tCount in self._List:
      if num % tCount == 0:
        return False
    for tCount in range(max(self._List), int(num ** (.5) + 1), 2):
      if num % tCount == 0:
        return False
    return True

  def __getitem__(self, key):
    if isinstance(key, int):
      return self.getitem_int(key)
    elif isinstance(key, slice):
      if key.start < 1 or key.stop < 1:
        raise TypeError("Invalid index.")
      else:
        return [self.getitem_int(i) for i in 
                range(*key.indices(max(key.start, key.stop)))]
    else:
      raise TypeError("Invalid index.")

  def getitem_int(self, key):
    #TODO Slow af.
    if key < 1:
      raise IndexError("There is no prime[{0}]".format(key))
    #elif key == 1:
      '''gen = iter(self)
      prime = 2
      for _ in range(key):
        prime = next(gen)
      assert(prime is not None)
      return prime'''
    #  return 2
    primelist = list(self._List)
    primelist.sort()
    if key < len(self._List):
      return primelist[key - 1]
    tPrime = primelist[-1]
    count = primelist[-1]
    found = len(self._List) - 1
    while found < key:
      if count in self:
        tPrime = count
        found += 1
        self._List.add(tPrime)
      count += 2
    return tPrime
  
  def __iter__(self):
    for num in self._List:
      assert(num is not None)
      yield num
    count = max(self._List) + 2
    while True:
      if count in self:
        if not count in self._List:
          self._List.add(count)
        assert(count is not None)
        yield count
      count += 2

  @staticmethod
  def factors(aInt):
    if aInt == 0 :
      return []
    if aInt == 1 :
      return []
    for tCount in Primes().less_than(int(aInt ** .5) + 1):
      if (aInt % tCount) == 0:
        tFactors = Primes.factors(aInt // tCount)
        tFactors.append(tCount)
        return tFactors
    return [aInt]

  def in_range(self, aMin, aMax):
    for tCount in range(aMin, aMax):
      if tCount in self:
        yield tCount

  def is_circular(self, aPrime):
    assert(aPrime in self)
    tStr = str(aPrime)
    tStr = tStr[-1] + tStr[:-1]
    while int(tStr) != aPrime:
      if not int(tStr) in self:
        return False
      tStr = tStr[-1] + tStr[:-1]
    return True

  def less_than(self,aMax):
    return self.in_range(2, aMax)
  
  @staticmethod 
  def num_divisors(aNum):
    tPrimeFactors = Primes.factors(aNum)
    tSet = set(tPrimeFactors)
    tFactors = 1
    for tFactor in tSet:
      tFactors *= tPrimeFactors.count(tFactor) + 1
    return tFactors

  def truncatable(self, aNum):
    if not aNum in self:
      return False
    tStr = str(aNum)
    if "2" in tStr or "4" in tStr or "6" in tStr or "8" in tStr:
      return False
    while tStr:
      if not int(tStr) in self:
        return False
      tStr = tStr[1:]
    tNum = aNum
    while tNum:
      if not tNum in self:
        return False
      tNum = tNum // 10
    return True
