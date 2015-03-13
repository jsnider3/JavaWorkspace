import euler
import itertools
import unittest

class Tests(unittest.TestCase):
  
  def test_choices(self):
    count = 0
    for n in range(1, 101):
      for r in range(1, n + 1):
        if euler.choose(n, r) > 1000000: 
          count += 1
    assert(count == 4075)

  def test_hexagonals(self):
    hexes = euler.Hexagonals()
    taketen = itertools.islice(hexes, 0, 10, 1)
    correct = [1, 6, 15, 28, 45, 66, 91, 120, 153, 190]
    taketen = list(taketen)
    assert(taketen == correct)

  def test_is_circular(self):
    primes = euler.Primes()
    assert(2 in primes)
    assert(primes.is_circular(2))
    assert(primes.is_circular(971))
    assert(not primes.is_circular(999953))

  def test_consecutive_sum_max(self):
    primes = euler.Primes()
    assert(2 == primes.consecutive_sum_max(5))
    assert(0 == primes.consecutive_sum_max(11))
    assert(6 == primes.consecutive_sum_max(41))

  def test_pentagonals(self):
    pents = euler.Pentagonals()
    taketen = itertools.islice(pents, 0, 10, 1)
    correct = [1, 5, 12, 22, 35, 51, 70, 92, 117, 145]
    taketen = list(taketen)
    assert(taketen == correct)

  def test_primes(self):
    primes = euler.Primes()
    taketen = primes[1:11]
    correct = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
    taketen = list(taketen)
    assert(taketen == correct)
    for x in range(1, 11):
      assert(primes[x] == correct[x - 1])
    #assert(primes[10001] == 104743)
  
  def test_pandigitals(self):
    pan = euler.Pandigitals()
    assert pan.get_pandigital_product(7254)
    assert pan.is_pandigital_multiple(192384576)
    assert pan.is_pandigital_multiple(932718654)
    count = 0
    for n in range(100, 100000):
      prod = pan.get_pandigital_product(n)
      if prod is not None:
        strn = str(prod[0]) + str(prod[1]) + str(prod[2])
        if len(strn) == 9:
          count += n
    assert count == 45228

  def test_romans(self):
    roman = euler.RomanNumeral("VI")
    assert int(roman) == 6
    roman = euler.RomanNumeral("IV")
    assert int(roman) == 4
    roman = euler.RomanNumeral("XIV")
    assert int(roman) == 14
    roman = euler.RomanNumeral("VIII")
    assert int(roman) == 8

  def test_shared_members(self):
    iters = [iter(euler.Triangulars()), iter(euler.Pentagonals()),
             iter(euler.Hexagonals())]
    shareds = euler.shared_members(iters)
    assert(next(shareds) == 1)
    assert(next(shareds) == 40755)
    assert(next(shareds) == 1533776805)

  def test_totient(self):
    correct = [(2, 1), (3, 2), (4, 2),
               (5, 4), (6, 2), (7, 6),
               (8, 2), (9, 6), (10, 4)]
    for (k, v) in correct:
      assert euler.totient(k) == v

  def test_triangulars(self):
    tries = euler.Triangulars()
    taketen = itertools.islice(tries, 0, 10, 1)
    correct = [0, 1, 3, 6, 10, 15, 21, 28, 36, 45]
    taketen = list(taketen)
    assert(taketen == correct)

  def test_various(self):
    assert euler.substring_div43(1406357289)

if __name__ == '__main__':
  unittest.main()

