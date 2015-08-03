#!/usr/bin/env python3
from bst import BinarySearchTree
from graph import Graph
import hacklib
import itertools
from pandigitals import Pandigitals
from romannumeral import RomanNumeral
import strings
import unittest

class Tests(unittest.TestCase):

  def test_accumulate(self):
    inp = [1, 12, 5, 111, 200, 1000, 10]
    orig = [1, 12, 5, 111, 200, 1000, 10]
    take = hacklib.accumulate(inp, 50)
    assert inp == orig
    assert take == [1, 5, 10, 12]

  def test_balanced_array(self):
    assert hacklib.balanced_array([1, 2, 3]) == None
    assert hacklib.balanced_array([1, 2, 3, 3]) == 2

  def test_bst(self):
    nums = [1, 2, 3, 4, 5, 6, 7]
    bst = BinarySearchTree.from_array(nums)
    for x in nums:
      assert x in bst
    assert nums == list(iter(bst))
    assert bst.height() == 3
    assert bst.is_balanced()
    rand = sum([bst.random_node() for _ in range(100)])
    #assert 350 < rand < 450
    bst = bst.remove(5)
    assert len(bst) == 6
    for x in nums:
      assert x in bst or x == 5
    unbalanced = BinarySearchTree(0)
    for x in nums:
      unbalanced.add(x)
    assert not unbalanced.is_balanced()

  def test_choices(self):
    count = 0
    for n in range(1, 101):
      for r in range(1, n + 1):
        if hacklib.choose(n, r) > 1000000:
          count += 1
    assert count == 4075

  def test_closest_numbers(self):
    assert hacklib.closest_numbers([5, 4, 3, 2]) == [(2,3),(3,4),(4,5)]

  def test_common_substring(self):
    assert strings.common_substring('hello', 'world')
    assert not strings.common_substring('hi', 'world')

  def test_consecutive_sum_max(self):
    primes = hacklib.Primes()
    assert(2 == primes.consecutive_sum_max(5))
    assert(0 == primes.consecutive_sum_max(11))
    assert(6 == primes.consecutive_sum_max(41))

  def test_decents(self):
    decs = hacklib.Decents()
    assert not decs.largest_of_len(1)
    assert decs.largest_of_len(3) == 555
    assert decs.largest_of_len(5) == 33333
    assert decs.largest_of_len(11) == 55555533333

  def test_dels_for_anagram(self):
    assert strings.dels_for_anagram('cde', 'abc') == 4

  def test_Palindrome_by_reduction(self):
    pals = hacklib.Palindrome()
    assert pals.by_reduction('abc') == 2
    assert pals.by_reduction('abcba') == 0
    assert pals.by_reduction('abcd') == 4
    assert pals.by_reduction('cba') == 2

  def test_Palindrome_from_anagram(self):
    pals = hacklib.Palindrome()
    assert pals.from_anagram('aaabbbb')
    assert not pals.from_anagram('cdefghmnopqrstuvw')
    assert pals.from_anagram('cdcdcdcdeeeef')

  def test_fibonacci(self):
    fibs = [0, 1, 1, 2, 3, 5, 8, 13, 21]
    for x in range(len(fibs)):
      assert hacklib.fibonacci(x) == fibs[x]
    fibseq = hacklib.ArithSequence(0, 1)
    assert 5 in fibseq
    assert 7 not in fibseq
    assert 8 in fibseq

  def test_funny(self):
    assert strings.is_funny('acxz')
    assert not strings.is_funny('bcxz')
    assert not strings.is_funny('ivvkxq')
    assert not strings.is_funny('ivvkx')

  def test_graph(self):
    graf = Graph()
    graf.set_edge(0,1)
    graf.set_edge(1,2)
    graf.set_edge(2,0)
    graf.set_edge(2,3)
    graf.set_edge(3,4)
    graf.set_edge(5,6)
    assert graf.connected(5, 6)
    assert graf.connected(0, 3)
    assert not graf.connected(0, 6)

  def test_hexagonals(self):
    hexes = hacklib.Hexagonals()
    taketen = itertools.islice(hexes, 0, 10, 1)
    correct = [1, 6, 15, 28, 45, 66, 91, 120, 153, 190]
    taketen = list(taketen)
    assert taketen == correct

  def test_is_circular(self):
    primes = hacklib.Primes()
    assert primes.is_circular(2)
    assert primes.is_circular(971)
    assert not primes.is_circular(999953)

  def test_is_pan(self):
    assert strings.is_pan('ABCDS1234Y')
    assert not strings.is_pan('ABCD12345Y')
    assert not strings.is_pan('avBCDS1234Y')

  def test_kth(self):
    assert hacklib.kth_element([0, 1, 2, 4, 6, 5, 3], 3) == 3

  def test_line_cover(self):
    assert len(hacklib.line_cover([1, 2, 3, 17, 10], 4)) == 3

  def test_matrix(self):
    grid = [[0, 1, 2, 3],
            [4, 5, 6, 7],
            [8, 9, 0, 1],
            [2, 3, 4, 5]]
    mat = hacklib.Matrix(grid)
    mat.zero()
    zeroed = [[0, 0, 0, 0],
              [0, 5, 0, 7],
              [0, 0, 0, 0],
              [0, 3, 0, 5]]
    assert mat.mat == zeroed
    grid = [[0, 1, 2, 3],
            [11, 12, 13, 4],
            [10, 15, 14, 5],
            [9, 8, 7, 6]]
    mat = hacklib.Matrix(grid)
    mat.rotate()
    correct = [[9, 10, 11, 0],
               [8, 15, 12, 1],
               [7, 14, 13, 2],
               [6, 5, 4, 3]]
    assert mat.mat == correct
    grid = [[1, 2, 3],
            [4, 5, 6],
            [7, 8, 9]]
    mat = hacklib.Matrix(grid)
    mat.rotate()
    correct = [[7, 4, 1],
               [8, 5, 2],
               [9, 6, 3]]
    assert mat.mat == correct
    grid = [[11, 2, 4],
            [4, 5, 6],
            [10, 8, -12]]
    assert hacklib.Matrix(grid).diag_diff() == -15
    grid = [['1','1','1','2'],
            ['1','9','1','2'],
            ['1','8','9','2'],
            ['1','2','3','4']]
    corr = [['1','1','1','2'],
            ['1','X','1','2'],
            ['1','8','X','2'],
            ['1','2','3','4']]
    mat = hacklib.Matrix(grid)
    mat.local_maxes()
    assert mat.mat == corr

  def test_max_subarray(self):
    assert sum(hacklib.max_subarray([1, 2, 3, 4])) == 10
    assert sum(hacklib.max_subarray([2, -1, 2, 3, 4, -5])) == 10

  def test_no_repeats(self):
    assert len(list(hacklib.no_repeats('AAAA'))) == 1
    assert len(list(hacklib.no_repeats('BBBBB'))) == 1
    assert len(list(hacklib.no_repeats('ABABABAB'))) == 8
    assert len(list(hacklib.no_repeats('BABABA'))) == 6
    assert len(list(hacklib.no_repeats('AAABBB'))) == 2

  def test_pair_diffs(self):
    pairs = hacklib.pair_diffs([1,5,3,4,2],2)
    assert len(pairs) == 3

  def test_pair_sums(self):
    pairs = hacklib.pair_sums([1, 4, 5, 3, 2], 4)
    assert len(pairs) == 1
    assert list(pairs)[0] == (0,3)
    pairs = hacklib.pair_sums([2, 2, 4, 3], 4)
    assert len(pairs) == 1
    assert list(pairs)[0] == (0,1)
    pairs = hacklib.pair_sums([678, 227, 764, 37, 956,
      982, 118, 212, 177, 597, 519, 968, 866, 121, 771, 343, 561], 295)
    assert len(pairs) == 1
    assert list(pairs)[0] == (6, 8)
    pairs = hacklib.pair_sums([5, 75, 25], 100)
    assert len(pairs) == 1
    assert list(pairs)[0] == (1, 2)

  def test_pandigitals(self):
    pan = Pandigitals()
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

  def test_pangram(self):
    assert strings.is_pangram(
      'We promptly judged antique ivory buckles for the next prize')
    assert not strings.is_pangram(
      'We promptly judged antique ivory buckles for the prize')

  def test_partition(self):
    arr = [3, 9, 8, 5 , 10, 3, 3, 2, 4]
    par = [2, 3, 3, 3, 9, 8, 5, 10, 4]
    assert hacklib.partition(arr) == par

  def test_pentagonals(self):
    pents = hacklib.Pentagonals()
    taketen = itertools.islice(pents, 0, 10, 1)
    correct = [1, 5, 12, 22, 35, 51, 70, 92, 117, 145]
    taketen = list(taketen)
    assert taketen == correct

  def test_primes(self):
    primes = hacklib.Primes()
    taketen = primes[1:11]
    correct = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
    taketen = list(taketen)
    assert taketen == correct
    for x in range(1, 11):
      assert primes[x] == correct[x - 1]
    assert primes[10001] == 104743

  def test_reorder_chars(self):
    cases = [('the theater', 'hte hteater'),
             ('interesting stories', 'interesting stories'),
             ('authentic atthic', 'auhtentic ahttic'),
             ('thththththththt', 'hhhhhhhtttttttt')]
    for (h, v) in cases:
      assert hacklib.reorder_chars(h, 'h', 't') == v

  def test_romans(self):
    roman = RomanNumeral("VI")
    assert int(roman) == 6
    roman = RomanNumeral("IV")
    assert int(roman) == 4
    roman = RomanNumeral("XIV")
    assert int(roman) == 14
    roman = RomanNumeral("VIII")
    assert int(roman) == 8

  def test_shared_members(self):
    iters = [iter(hacklib.Triangulars()), iter(hacklib.Pentagonals()),
             iter(hacklib.Hexagonals())]
    shareds = hacklib.shared_members(iters)
    assert next(shareds) == 1
    assert next(shareds) == 40755
    assert next(shareds) == 1533776805

  def test_shared_prefix(self):
    assert strings.shared_prefix('abcdef', 'abcde') == 'abcde'
    assert strings.shared_prefix('abcdef', 'bcde') == ''

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
    assert taketen == correct

  def test_various(self):
    assert hacklib.substring_div43(1406357289)
    assert strings.unique_str('abcdefghijklmnopqrstuvwxyz')
    assert (not strings.unique_str('aacdefghijklmnopqrstuvwxyz'))

  def test_visual_insert(self):
    out = hacklib.visual_insert([2, 4, 6, 8], 3)
    correct = ['2 4 6 8 8',
               '2 4 6 6 8',
               '2 4 4 6 8',
               '2 3 4 6 8']
    assert out == correct

  def test_xor_maximum(self):
    assert hacklib.xor_maximum(1, 10) == 15

  def test_zip_array_sum(self):
    assert hacklib.zip_array_sum([2, 1, 3], [7, 8, 9], 10)
    assert not hacklib.zip_array_sum([1, 2, 2, 1], [3, 3, 3, 4], 5)

if __name__ == '__main__':
  unittest.main()

