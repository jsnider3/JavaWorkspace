from decimal import *
import math
import unittest

def choose(n, k):
    choice = 0
    if k >= 0 and n >= k:
        ntok = Decimal(1)
        ktok = Decimal(1)
        for t in range(1, min(k, n - k) + 1):
            ntok *= n
            ktok *= t
            n -= 1
        choice = ntok / ktok
    return choice
    #else:
    #    return Decimal(0)

def unconnected_graphs(nodes, edges, cache={}):
    ''' How many graphs with a given number of nodes and edges
        are not connected? '''
    unconnected_graphs = Decimal(0)
    if (nodes, edges) in cache:
        unconnected_graphs = cache[(nodes, edges)]
    else:
      for num in range(nodes - 1):
        choice = choose(nodes - 1, num)
        accum = Decimal(0)
        top = (nodes - 1 - num) * (nodes - 2 - num) / 2
        for part in range(min(edges + 1, top + 2)):
          accum += (choose(top, part) *
                    possible_tunnels(num + 1, edges - part))
        unconnected_graphs += choice * accum
      cache[(nodes, edges)] = unconnected_graphs
    return unconnected_graphs

def possible_tunnels(nodes, edges, cache={}):
    ''' How many connected graphs are there with a given
          number of distinct nodes and a given number of edges.'''
    #Maybe this is number of possible edges choose number of actual edges?
    # Then subtract out the non-connected things.
    #Test cases are (2, 1), (4, 3), (20,125)
    #assert nodes < 20 or edges > 125
    ways = Decimal(0)
    if edges < nodes - 1 or edges > nodes * (nodes - 1) / 2:
      pass #ways = Decimal(0)
    elif edges == nodes - 1:
      ways = Decimal(nodes ** (nodes - 2))
    elif (nodes,edges) in cache:
      ways = cache[(nodes, edges)]
    else:
      poss_graphs = choose(nodes * (nodes - 1) / 2, edges)
      ways = poss_graphs - unconnected_graphs(nodes, edges)
      cache[(nodes, edges)] = ways
    return ways

def answer(nodes, edges):
  getcontext().prec = 90

  return '{:f}'.format(possible_tunnels(nodes, edges).to_integral_exact())
#  return str(possible_tunnels(nodes, edges).to_integral_exact())

class Tests(unittest.TestCase):
  def test_tunnel_possibilities(self):
    nodes = 1
    edges = 0
    count = 0
    wrong = 0
    with open('possible_tunnels_tests.txt') as tests:
      for line in tests:
        [_, corr] = [int(s) for s in line.split()]
        test = answer(nodes, edges)
        print(nodes, edges, test, corr)
        if test != str(corr):
          wrong += 1
        assert test == str(corr)
        edges += 1
        count += 1
        if edges > nodes * (nodes - 1) / 2:
          nodes += 1
          edges = nodes - 1
    print('wrong', wrong)

if __name__ == '__main__':
  unittest.main()

