''' Wrapped module for Primes class'''

import pdb

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

  def __getitem__(self, key):
    if isinstance(key, int):
      return self.nth_prime(key)
    elif isinstance(key, slice):
      if key.start < 1 or key.stop < 1:
        raise TypeError("Invalid index.")
      else:
        return [self.nth_prime(i) for i in
                range(*key.indices(max(key.start, key.stop)))]
    else:
      raise TypeError("Invalid index.")

  def __iter__(self):
    #pdb.set_trace()
    for num in sorted(list(self.cache)):
      yield num
    count = max(self.cache) + 2
    while True:
      if count in self:
        if not count in self.cache:
          self.cache.add(count)
        yield count
      count += 2

  def __len__(self):
    raise NotImplementedError("There are infinite primes.")

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
      elif sum(accum) < num:
        stop += 1
        accum.append(self[stop])
      elif sum(accum) > num:
        accum.remove(accum[0])
        start += 1
      elif sum(accum) == num:
        return len(accum)

  def consecutive_sum_max_length(self, cutoff):
    '''Return the prime less than cutoff
       which can be expressed as the maximum
       number of other primes.'''
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
      start += 1
    return (maxtot, maxlen)

  def coprime(self, first, second):
    ''' check if first and second are coprimes.'''
    return fractions.gcd(first, second) == 1

  def countdown(self, start):
    ''' Return all the primes <= start from
        greatest to least. '''
    while start > 1:
      if start in self:
        yield start
      start -= 1

  def factors(self, num):
    '''Factorize num and return them in sorted order.'''
    if num == 0 or num == 1:
      return []
    for count in self.less_than(num // 2 + 1):
      if (num % count) == 0:
        factors = self.factors(num // count)
        factors.append(count)
        return factors[::-1]
    return [num]

  def full_repetend(self):
    for n in self:
      if is_primitive_root(n, 10):
        yield n

  def in_range(self, start, limit):
    '''List of primes in [start, limit)'''
    #TODO Optimize
    for count in range(start, limit):
      if count in self:
        yield count

  def is_circular(self, prime):
    '''As defined by Project Euler 35.'''
    assert prime in self
    strn = str(prime)
    strn = strn[-1] + strn[:-1]
    while int(strn) != prime:
      if not int(strn) in self:
        return False
      strn = strn[-1] + strn[:-1]
    return True

  def less_than(self, limit):
    '''List of primes < limit.'''
    return self.in_range(2, limit)

  def nth_prime(self, key):
    '''Get the nth prime, with 2 being the 1st.'''
    #TODO Slow af.
    if key < 1:
      raise IndexError("There is no prime[{0}]".format(key))
    primelist = list(self.cache)
    primelist.sort()
    if key < len(self.cache):
      return primelist[key - 1]
    prime = primelist[-1]
    count = primelist[-1]
    found = len(self.cache) - 1
    while found < key:
      if count in self:
        prime = count
        found += 1
        self.cache.add(prime)
      count += 2
    return prime

  @staticmethod
  def num_divisors(num):
    '''Return the number of numbers that evenly divide num.'''
    all_factors = Primes.factors(num)
    uniq_factors = set(all_factors)
    factors = 1
    for factor in uniq_factors:
      factors *= all_factors.count(factor) + 1
    return factors

  def truncatable(self, num):
    '''As defined by Project Euler 37.'''
    strn = str(num)
    if not num in self or len(strn) == 1:
      return False
    mid = strn[1:-1]
    if '0' in mid or '2' in mid or '4' in mid or '6' in mid or '8' in mid:
      return False
    while strn:
      if not int(strn) in self:
        return False
      strn = strn[1:]
    while num:
      if not num in self:
        return False
      num = num // 10
    return True
