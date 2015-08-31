#!/usr/bin/env python3
from bst import BinarySearchTree
import fractions
from graph import Graph
import hacklib
import itertools
from pandigitals import Pandigitals
from primes import Primes
from romannumeral import RomanNumeral
import strings
import unittest

class Tests(unittest.TestCase):

  def test_abundants(self):
    abund = hacklib.Abundants()
    test = hacklib.take(abund, 5)
    assert test == [12, 18, 20, 24, 30]
    assert 20 in abund
    assert 102 in abund
    assert 118 not in abund

  def test_accumulate(self):
    inp = [1, 12, 5, 111, 200, 1000, 10]
    orig = [1, 12, 5, 111, 200, 1000, 10]
    take = hacklib.accumulate(inp, 50)
    assert inp == orig
    assert take == [1, 5, 10, 12]

  def test_amicables(self):
    assert hacklib.get_amicable_pair(220) == (220, 284)
    assert hacklib.get_amicable_pair(284) == (220, 284)
    assert hacklib.get_amicable_pair(219) == None

  def test_and_product(self):
    assert hacklib.and_product(12, 15) == 12
    assert hacklib.and_product(2, 3) == 2
    assert hacklib.and_product(13, 8) == 8

  def test_assign_candies(self):
    assert hacklib.assign_candies([1, 2, 2]) == 4
    assert hacklib.assign_candies([5, 4, 3, 2, 1]) == 15
    assert hacklib.assign_candies([1, 2, 3, 2, 1]) == 9
    assert hacklib.assign_candies([2, 4, 2, 6, 1, 7, 8, 9, 2, 1]) == 19
    with open('assign_candies_input01.txt') as fil:
      lines = fil.readlines()
      ranks = [int(line) for line in lines[1:]]
      assert hacklib.assign_candies(ranks) == 33556

  def test_balanced_array(self):
    assert hacklib.balanced_array([1, 2, 3]) == None
    assert hacklib.balanced_array([1, 2, 3, 3]) == 2

  def test_binary_search(self):
    assert hacklib.binary_search([1], 1) == 0
    assert hacklib.binary_search([1], 2) == None
    assert hacklib.binary_search([1, 2, 3, 4], 4) == 3
    assert hacklib.binary_search([0, 1, 2, 3, 4], 0) == 0

  def test_bitstring_fillin(self):
    assert hacklib.bitstring_fillin('?') == ['0', '1']
    assert hacklib.bitstring_fillin('1') == ['1']
    assert hacklib.bitstring_fillin('?1?0') == ['0100', '0110',
                                                '1100', '1110']

  def test_bitstring_or(self):
    assert hacklib.bitstring_or('10101', '11100') == '11101'
    assert hacklib.bitstring_or('11010', '00101') == '11111'

  def test_british_number_string(self):
    assert (strings.british_number_string(4734) ==
            'four thousand seven hundred and thirty four')
    assert (strings.british_number_string(3214) ==
            'three thousand two hundred and fourteen')
    assert (strings.british_number_string(5600) ==
            'five thousand six hundred')

  def test_bst(self):
    assert BinarySearchTree.from_array([]) is None
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
    bst = bst.remove(3)
    assert len(bst) == 5
    for x in nums:
      assert x in bst or x == 5 or x == 3
    bst = bst.remove(4)
    assert len(bst) == 4
    for x in nums:
      assert x in bst or x == 5 or x == 3 or x == 4
    bst.add(0)
    assert len(bst) == 5
    assert list(bst.pre_order()) == [2, 1, 0, 6, 7]
    assert list(bst.post_order()) == [0, 1, 7, 6, 2]
    for x in list(bst.pre_order()):
      bst = bst.remove(x)
    unbalanced = BinarySearchTree(0)
    for x in nums:
      unbalanced.add(x)
    assert not unbalanced.is_balanced()

  def test_case_insensitive_contains(self):
    assert strings.case_insensitive_contains('hello Python', 'pYtHoN')
    assert strings.case_insensitive_contains('hello Python', 'lo P')
    assert not strings.case_insensitive_contains('hello Python', 'lo Pi')

  def test_caesar(self):
    assert strings.caesar('middle-Outz', 2) == 'okffng-Qwvb'

  def test_champernowne(self):
    for num in range(1, 10):
      assert hacklib.champernowne(num) == num
    assert hacklib.champernowne(100) == 5
    assert hacklib.champernowne(1000) == 3
    assert hacklib.champernowne(10000) == 7
    assert hacklib.champernowne(100000) == 2
    assert hacklib.champernowne(1000000) == 1

  def test_choices(self):
    count = 0
    for n in range(1, 101):
      for r in range(1, n + 1):
        if hacklib.choose(n, r) > 1000000:
          count += 1
    assert count == 4075

  def test_closest_numbers(self):
    assert hacklib.closest_numbers([5, 4, 3, 2]) == [(2,3),(3,4),(4,5)]
    assert hacklib.closest_numbers([8, 6, 4, 9, 2]) == [(8, 9)]

  def test_collatz(self):
    col = hacklib.Collatz()
    assert col.depth(13) == 10
    for num in range(1, 1000001):
      col.depth(num)
    assert col.max_depth() == 837799

  def test_common_elements(self):
    lns = [list('labcdde'), list('baccd'), list('eeabg')]
    assert len(hacklib.common_elements(*lns)) == 2
    assert hacklib.common_elements(['a'], ['b'], ['c']) == set([])

  def test_common_substring(self):
    assert strings.common_substring('hello', 'world')
    assert not strings.common_substring('hi', 'world')

  def test_consecutive_sum_max(self):
    primes = Primes()
    assert(2 == primes.consecutive_sum_max(5))
    assert(0 == primes.consecutive_sum_max(11))
    assert(6 == primes.consecutive_sum_max(41))

  def test_consecutive_sum_max_length(self):
    primes = Primes()
    assert primes.consecutive_sum_max_length(50) == (41, 6)

  def test_convergents(self):
    euler = hacklib.Convergents.of_e()
    test = hacklib.take(euler, 4)
    assert test[0] == fractions.Fraction(2, 1)
    assert test[1] == fractions.Fraction(3, 1)
    assert test[2] == fractions.Fraction(8, 3)
    assert test[3] == fractions.Fraction(11, 4)

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

  def test_extract_order(self):
    assert hacklib.extract_order(['y', 'z', 'xy']) == 'yzx'
    assert hacklib.extract_order(['ba', 'ab', 'cb']) == 'bac'
    assert hacklib.extract_order(['ba', 'ab', 'cb']) == 'bac'
    assert hacklib.extract_order(['ba', 'cb', 'ca', 'cc']) == 'bac'
    assert hacklib.extract_order(['ba', 'ca', 'cb']) == 'abc'

  def test_factors(self):
    primes = Primes()
    assert primes.factors(0) == []
    assert primes.factors(1) == []
    assert primes.factors(2) == [2]
    assert primes.factors(6) == [2, 3]
    assert primes.factors(8) == [2, 2, 2]
    assert primes.factors(29) == [29]

  def test_fibonacci(self):
    fibs = [0, 1, 1, 2, 3, 5, 8, 13, 21]
    fibseq = hacklib.ArithSequence(0, 1)
    taketen = hacklib.take(fibseq, len(fibs))
    for x in range(len(fibs)):
      assert taketen[x] == fibs[x]
    assert 5 in fibseq
    assert 7 not in fibseq
    assert 8 in fibseq
    assert 144 in fibseq

  def test_fillings(self):
    assert hacklib.fillings([1, 4, 2, 5, 1, 2, 3]) == 5
    assert hacklib.fillings([1, 2, 3, 2, 1]) == 0

  def test_find_pythag_triplet(self):
    (first, secnd, third) = hacklib.find_pythag_triplet(176)
    assert (first, secnd, third) == (48, 55, 73)

  def test_find_right_triangles(self):
    tries = hacklib.find_right_triangles(90)
    assert tries == [(9, 40, 41), (15, 36, 39)]

  def test_flat_index(self):
    arr = [[0, 1], [2, 3, 4], [5, 6, 7, 8], [], [9]]
    for num in range(10):
      assert hacklib.flat_index(arr, num) == num

  def test_freq_sort(self):
    assert hacklib.freq_sort('aabbbccde') == ['b', 'a', 'c', 'd', 'e']

  def test_funny(self):
    assert strings.is_funny('acxz')
    assert not strings.is_funny('bcxz')
    assert not strings.is_funny('ivvkxq')
    assert not strings.is_funny('ivvkx')

  def test_goldbach(self):
    assert hacklib.goldbach(33)
    assert not hacklib.goldbach(5777)

  def test_graph(self):
    graf = Graph()
    graf.set_edge(0,1)
    graf.set_edge(1,2)
    graf.set_edge(2,0)
    graf.set_edge(2,3)
    graf.set_edge(3,4)
    graf.set_edge(5,6)
    assert len(graf) == 7
    assert graf.connected(5, 6)
    assert graf.connected(0, 3)
    assert not graf.connected(0, 6)

  def test_group_into_equivalency_classes(self):
    modthree = lambda x, y: x % 3 == y % 3
    groups = hacklib.group_into_equivalency_classes(list(range(1, 10)),
                                                    modthree)
    assert groups == [[1, 4, 7], [2, 5, 8], [3, 6, 9]]

  def test_hexagonals(self):
    hexes = hacklib.Hexagonals()
    taketen = itertools.islice(hexes, 0, 10, 1)
    correct = [1, 6, 15, 28, 45, 66, 91, 120, 153, 190]
    taketen = list(taketen)
    assert taketen == correct
    assert 120 in hexes
    assert 119 not in hexes

  def test_is_anagram(self):
    assert strings.is_anagram('abcedd', 'decbad')
    assert not strings.is_anagram('abcedd', 'ecbad')
    assert not strings.is_anagram('abcedd', 'dgecbad')

  def test_is_circular(self):
    primes = Primes()
    assert primes.is_circular(2)
    assert primes.is_circular(971)
    assert not primes.is_circular(999953)

  def test_is_pan(self):
    assert strings.is_pan('ABCDS1234Y')
    assert not strings.is_pan('ABCD12345Y')
    assert not strings.is_pan('avBCDS1234Y')

  def test_is_power_of(self):
    assert hacklib.is_power_of(27, 3)
    assert not hacklib.is_power_of(96, 10)

  def test_k_split(self):
    assert hacklib.k_split('ABCDEFGHI', 3) == ['ABC', 'DEF', 'GHI']

  def test_knapsack_isopriced(self):
    assert hacklib.knapsack_isopriced([1, 6, 9], 12) == 12
    assert hacklib.knapsack_isopriced([3, 4, 4, 4, 8], 9) == 9

  def test_kth(self):
    assert hacklib.kth_element([0, 1, 2, 4, 6, 5, 3], 3) == 3

  def test_largest_palindrome_product(self):
    pals = hacklib.Palindrome()
    assert pals.find_largest_product(100, 999) == 906609

  def test_largest_permutation(self):
    assert hacklib.largest_permutation([4, 2, 3, 5, 1], 1) == [5, 2, 3, 4, 1]
    assert hacklib.largest_permutation([2, 1, 3], 1) == [3, 1, 2]
    assert hacklib.largest_permutation([2, 1], 1) == [2, 1]

  def test_least_common_multiple(self):
    assert hacklib.least_common_multiple([4, 6]) == 12
    assert hacklib.least_common_multiple([3, 6, 7]) == 42

  def test_line_cover(self):
    assert len(hacklib.line_cover([1, 2, 3, 17, 10], 4)) == 3

  def test_list_cover(self):
    assert hacklib.list_cover([[0], [1], [2], [3], [4]]) == hacklib.Segment(0, 4)
    assert hacklib.list_cover([[0, 4], [2], [-4, 3]]) == hacklib.Segment(2, 4)

  def test_lonely_member(self):
    assert hacklib.lonely_member([1, 2, 3, 1, 2, 0, 3]) == 0

  def test_make_change(self):
    assert hacklib.make_change([1, 2, 3], 4) == 4
    assert hacklib.make_change([2, 5, 3, 6], 10) == 5
    assert hacklib.make_change([2, 5, 3, 6], 0) == 1

  def test_matrix(self):
    grid = [[0, 1, 2, 3],
            [4, 5, 6, 7],
            [8, 9, 0, 1],
            [2, 3, 4, 5]]
    mat = hacklib.Matrix(grid)
    assert len(mat) == 16
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
    assert not (mat.is_col_sorted() or mat.is_row_sorted())
    grid = [[1, 2, 3],
            [4, 5, 6],
            [7, 8, 9]]
    mat = hacklib.Matrix(grid)
    assert mat.is_col_sorted() and mat.is_row_sorted()
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
    big  = [['1','1','1','2'],
            ['1','9','1','2'],
            ['1','8','9','2'],
            ['1','2','3','4']]
    smal = [['1','1','1'],
            ['1','9','1'],
            ['1','8','9']]
    assert hacklib.Matrix(big).find(hacklib.Matrix(smal))
    smal = [['1','1','1','2'],
            ['1','9','1','2'],
            ['1','8','9','2'],
            ['1','2','3','4']]
    assert hacklib.Matrix(big).find(hacklib.Matrix(smal))
    big  = [['1','1','1'],
            ['1','9','1'],
            ['1','8','9']]
    assert not hacklib.Matrix(big).find(hacklib.Matrix(smal))

  def test_max_subarray(self):
    assert sum(hacklib.max_subarray([1, 2, 3, 4])) == 10
    assert sum(hacklib.max_subarray([2, -1, 2, 3, 4, -5])) == 10
    assert sum(hacklib.max_subarray([-1, -2, -3, -4])) == -1

  def test_naturals(self):
    nats = hacklib.Naturals()
    takefive = hacklib.take(nats, 5)
    assert takefive == [1, 2, 3, 4, 5]
    assert 12 in nats
    assert -5 not in nats
    assert 12.3 not in nats

  def test_nodes_with_branching(self):
    assert hacklib.nodes_with_branching(1, 7) == 8
    assert hacklib.nodes_with_branching(2, 7) == 57
    assert hacklib.nodes_with_branching(3, 7) == 7*7*7 + 7*7 + 7 + 1

  def test_no_repeats(self):
    assert len(list(hacklib.no_repeats('AAAA'))) == 1
    assert len(list(hacklib.no_repeats('BBBBB'))) == 1
    assert len(list(hacklib.no_repeats('ABABABAB'))) == 8
    assert len(list(hacklib.no_repeats('BABABA'))) == 6
    assert len(list(hacklib.no_repeats('AAABBB'))) == 2

  def test_number_spiral_sum(self):
    correct = [(0, 1), (1, 24), (2, 76)]
    for (k, v) in correct:
      assert hacklib.number_spiral_sum(k) == v

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
    assert not pan.is_pandigital_multiple(932718644)
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
    correct = [1, 5, 12, 22, 35, 51, 70, 92, 117, 145]
    for num in correct:
      assert num in pents
    taketen = hacklib.take(pents, 10)
    assert taketen == correct
    assert 6 not in pents

  def test_pentagonal_pairs(self):
    pents = hacklib.Pentagonals()
    assert not pents.pair(22, 70)
    assert pents.pair(pents[1020], pents[2167])

  def test_permutation_set(self):
    lst = list(hacklib.permutation_set([1, 1, 2, 3]))
    assert len(lst) == 12
    assert lst == hacklib.uniq(lst)

  def test_points_in_triangle(self):
    assert hacklib.points_in_triangle([2, 3], [6, 9], [10, 160]) == 289

  def test_points_on_slope(self):
    pass

  def test_possible_ends(self):
    assert hacklib.possible_ends(3, 1, 2) == [2, 3, 4]
    assert hacklib.possible_ends(4, 10, 100) == [30, 120, 210, 300]
    assert hacklib.possible_ends(4, 10, 10) == [30]

  def test_primes(self):
    primes = Primes()
    assert 0 not in primes
    assert 1 not in primes
    taketen = primes[1:11]
    correct = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
    taketen = list(taketen)
    assert taketen == correct
    for x in range(1, 11):
      assert primes[x] == correct[x - 1]
    assert 104744 not in primes
    assert primes[10001] == 104743
    itr = iter(primes)
    for _ in range(10100):
      next(itr)
    assert list(primes.countdown(29))[::-1] == correct

  def test_product(self):
    assert hacklib.product([1,2,3,4,5]) == 5*4*3*2

  def test_quad_prime(self):
    assert hacklib.quad_prime(1, 41) == 40

  def test_reorder_chars(self):
    cases = [('the theater', 'hte hteater'),
             ('interesting stories', 'interesting stories'),
             ('authentic atthic', 'auhtentic ahttic'),
             ('thththththththt', 'hhhhhhhtttttttt')]
    for (h, v) in cases:
      assert hacklib.reorder_chars(h, 'h', 't') == v

  def test_resilience(self):
    assert hacklib.resilience(12) == 4.0/11
    assert hacklib.resilient_search(4.0/10) == 12

  def test_romans(self):
    roman = RomanNumeral("VI")
    assert int(roman) == 6
    roman = RomanNumeral("IV")
    assert int(roman) == 4
    roman = RomanNumeral("XIV")
    assert int(roman) == 14
    roman = RomanNumeral("VIII")
    assert int(roman) == 8
    assert str(RomanNumeral.from_int(6)) == "VI"
    assert int(RomanNumeral("VIIIIIIIIII")) == 15

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

  def test_split_index(self):
    pals = hacklib.Palindrome()
    assert pals.split_index('aaab') == 3
    assert pals.split_index('baaa') == 0
    assert pals.split_index('aaa') == None

  def test_squarechains(self):
    chain = hacklib.SquareChain()
    chain.get_end(145) == 89
    chain.get_end(44) == 1

  def test_squares(self):
    squares = hacklib.Squares()
    takefive = hacklib.take(squares, 5)
    for num in takefive:
      assert num in squares
    assert takefive == [1, 4, 9, 16, 25]
    assert 17 not in takefive
    assert -1 not in takefive

  def test_stock_maximize(self):
    assert hacklib.stock_maximize([5, 3, 2]) == 0
    assert hacklib.stock_maximize([1, 2, 100]) == 197
    assert hacklib.stock_maximize([1, 3, 1, 2]) == 3

  def test_successor(self):
    assert hacklib.successor(['a', 'b']) == ['b', 'a']
    assert hacklib.successor(['b', 'b']) == None
    assert hacklib.successor(['h', 'e', 'f', 'g']) == ['h', 'e', 'g', 'f']
    assert hacklib.successor(['d', 'h', 'c', 'k']) == ['d', 'h', 'k', 'c']
    assert hacklib.successor(['d', 'k', 'h', 'c']) == ['h', 'c', 'd', 'k']

  def test_sum_squares(self):
    assert hacklib.sum_squares(1) == 1
    assert hacklib.sum_squares(2) == 5
    assert hacklib.sum_squares(3) == 14

  def test_swap_case(self):
    assert (strings.swap_case("Please sWaP this pHRASE PLeas!")
                           == "pLEASE SwAp THIS Phrase plEAS!")

  def test_tile_four_by_n(self):
    correct = [(1, 1), (2, 1), (3, 1),
               (4, 2), (5, 3), (6, 4),
               (7, 5), (8, 7), (9, 10)]
    for (k, v) in correct:
      assert hacklib.tile_four_by_n(k) == v

  def test_topo_sort(self):
    graf = Graph()
    graf.set_edge(0, 1)
    graf.set_edge(1, 2)
    graf.set_edge(2, 3)
    graf.set_edge(3, 4)
    graf.set_edge(4, 5)
    graf.set_edge(5, 6)
    assert graf.topo_sort() == [0, 1, 2, 3, 4, 5, 6]

  def test_totient(self):
    correct = [(2, 1), (3, 2), (4, 2),
               (5, 4), (6, 2), (7, 6),
               (8, 4), (9, 6), (10, 4)]
    for (k, v) in correct:
      assert hacklib.totient(k) == v

  def test_triangulars(self):
    tries = hacklib.Triangulars()
    taketen = hacklib.take(tries, 10)
    correct = [0, 1, 3, 6, 10, 15, 21, 28, 36, 45]
    assert taketen == correct
    assert 28 in tries
    assert 29 not in tries
    assert 27 not in tries
    assert -6 not in tries

  def test_two_power_rank(self):
    correct = [(1, 0), (2, 1), (3, 1), (4, 2), (5, 1),
               (6, 2), (7, 2), (8, 3), (9, 1), (10, 2)]
    for (k, v) in correct:
      assert hacklib.two_power_rank(k) == v

  def test_tuple_to_num(self):
    assert hacklib.tuple_to_num((1,2,3)) == 123

  def test_uniq(self):
    assert hacklib.uniq('ABCADEFGGB') == ['A', 'B', 'C', 'D', 'E', 'F', 'G']

  def test_various(self):
    assert hacklib.substring_div43(1406357289)
    assert strings.unique_str('abcdefghijklmnopqrstuvwxyz')
    assert not strings.unique_str('aacdefghijklmnopqrstuvwxyz')

  def test_visual_insert(self):
    out = hacklib.visual_insert([2, 4, 6, 8], 3)
    correct = ['2 4 6 8 8',
               '2 4 6 6 8',
               '2 4 4 6 8',
               '2 3 4 6 8']
    assert out == correct

  def test_word_location_map(self):
    document = "many microsoft employees own an microsoft xbox"
    res = hacklib.word_location_map(document)
    assert res == {"many" : [0],
                   "microsoft" : [1, 5],
                   "employees" : [2],
                   "own" : [3],
                   "an" : [4],
                   "xbox" : [6]}

  def test_xor_maximum(self):
    assert hacklib.xor_maximum(1, 10) == 15

  def test_xor_sansa(self):
    assert hacklib.xor_sansa([1, 2, 3]) == 2
    assert hacklib.xor_sansa([4, 5, 7, 5]) == 0

  def test_zip_array_sum(self):
    assert hacklib.zip_array_sum([2, 1, 3], [7, 8, 9], 10)
    assert not hacklib.zip_array_sum([1, 2, 2, 1], [3, 3, 3, 4], 5)

if __name__ == '__main__':
  unittest.main()

