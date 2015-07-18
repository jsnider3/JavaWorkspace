import hacklib
import itertools
import unittest

class Tests(unittest.TestCase):

  def test_choices(self):
    count = 0
    for n in range(1, 101):
      for r in range(1, n + 1):
        if hacklib.choose(n, r) > 1000000:
          count += 1
    assert(count == 4075)

  def test_hexagonals(self):
    hexes = hacklib.Hexagonals()
    taketen = itertools.islice(hexes, 0, 10, 1)
    correct = [1, 6, 15, 28, 45, 66, 91, 120, 153, 190]
    taketen = list(taketen)
    assert(taketen == correct)

  def test_is_circular(self):
    primes = hacklib.Primes()
    assert(primes.is_circular(2))
    assert(primes.is_circular(971))
    assert(not primes.is_circular(999953))

  def test_consecutive_sum_max(self):
    primes = hacklib.Primes()
    assert(2 == primes.consecutive_sum_max(5))
    assert(0 == primes.consecutive_sum_max(11))
    assert(6 == primes.consecutive_sum_max(41))

  def test_pentagonals(self):
    pents = hacklib.Pentagonals()
    taketen = itertools.islice(pents, 0, 10, 1)
    correct = [1, 5, 12, 22, 35, 51, 70, 92, 117, 145]
    taketen = list(taketen)
    assert(taketen == correct)

  def test_matrix(self):
    grid = [[0,1,2,3],
            [4,5,6,7],
            [8,9,0,1],
            [2,3,4,5]]
    mat = hacklib.Matrix(grid)
    mat.zero()
    zeroed = [[0,0,0,0],
              [0,5,0,7],
              [0,0,0,0],
              [0,3,0,5]]
    assert(mat.mat == zeroed)
    grid = [[0,1,2,3],
            [11,12,13,4],
            [10,15,14,5],
            [9,8,7,6]]
    mat = hacklib.Matrix(grid)
    mat.rotate()
    correct = [[9,10,11,0],
               [8,15,12,1],
               [7,14,13,2],
               [6,5,4,3]]
    assert(mat.mat == correct)
    grid = [[1,2,3],
            [4,5,6],
            [7,8,9]]
    mat = hacklib.Matrix(grid)
    mat.rotate()
    correct = [[7,4,1],
               [8,5,2],
               [9,6,3]]
    assert(mat.mat == correct)

  def test_primes(self):
    primes = hacklib.Primes()
    taketen = primes[1:11]
    correct = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
    taketen = list(taketen)
    assert(taketen == correct)
    for x in range(1, 11):
      assert(primes[x] == correct[x - 1])
    #assert(primes[10001] == 104743)

  def test_pandigitals(self):
    pan = hacklib.Pandigitals()
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

  def test_reorder_chars(self):
    cases = [('the theater', 'hte hteater'),
             ('interesting stories', 'interesting stories'),
             ('authentic atthic', 'auhtentic ahttic'),
             ('thththththththt', 'hhhhhhhtttttttt')]
    for (h,v) in cases:
      assert hacklib.reorder_chars(h, 'h', 't') == v

  def test_romans(self):
    roman = hacklib.RomanNumeral("VI")
    assert int(roman) == 6
    roman = hacklib.RomanNumeral("IV")
    assert int(roman) == 4
    roman = hacklib.RomanNumeral("XIV")
    assert int(roman) == 14
    roman = hacklib.RomanNumeral("VIII")
    assert int(roman) == 8

  def test_shared_members(self):
    iters = [iter(hacklib.Triangulars()), iter(hacklib.Pentagonals()),
             iter(hacklib.Hexagonals())]
    shareds = hacklib.shared_members(iters)
    assert(next(shareds) == 1)
    assert(next(shareds) == 40755)
    assert(next(shareds) == 1533776805)

  def test_totient(self):
    correct = [(2, 1), (3, 2), (4, 2),
               (5, 4), (6, 2), (7, 6),
               (8, 4), (9, 6), (10, 4)]
    #for (k, v) in correct:
    #  assert hacklib.totient(k) == v
    #assert hacklib.resilience(12) == 4/11

  def test_triangulars(self):
    tries = hacklib.Triangulars()
    taketen = itertools.islice(tries, 0, 10, 1)
    correct = [0, 1, 3, 6, 10, 15, 21, 28, 36, 45]
    taketen = list(taketen)
    assert(taketen == correct)

  def test_various(self):
    assert hacklib.substring_div43(1406357289)
    assert hacklib.unique_str('abcdefghijklmnopqrstuvwxyz')
    assert (not hacklib.unique_str('aacdefghijklmnopqrstuvwxyz'))

if __name__ == '__main__':
  unittest.main()

