import math
import unittest

def choose(n, r):
    ''' return n choose r. '''
    #Imprecise
    if n < r:
        return 0
    else:
        return fact(n)/(fact(r) * fact(n - r))

def fact(n):
    prod = 1
    for num in range(2, n+1):
        prod *= num
    return prod

def unconnected_graphs(nodes, edges, unc_graph_cache={}):
    ''' How many graphs with a given number of nodes and edges
        are not connected? '''
    unconnected_graphs = 0
    if (nodes, edges) in unc_graph_cache:
        unconnected_graphs = unc_graph_cache[(nodes, edges)]
    else:
        for num in range(nodes - 1):
            accum = 0
            top = (nodes - 1 - num) * (nodes - 2 - num) / 2
            stop = min(edges + 1, top + 2)
            for val in range(stop):
                accum += (choose(top, val) *
                          possible_tunnels(num + 1, edges - val))
            unconnected_graphs += choose(nodes - 1, num) * accum
        unc_graph_cache[(nodes, edges)] = unconnected_graphs
    return unconnected_graphs

def possible_tunnels(nodes, edges, cache={}):
    ''' How many connected graphs are there with a given
          number of distinct nodes and a given number of edges.'''
    #Maybe this is number of possible edges choose number of actual edges?
    # Then subtract out the non-connected things.
    #Test cases are (2, 1), (4, 3), (20,125)
    #assert nodes < 20 or edges > 125
    ways = 1
    max_edges = nodes * (nodes - 1) / 2
    if edges < nodes - 1 or edges > max_edges:
        ways = 0
    elif edges + 1 == nodes:
        ways = nodes ** (nodes - 2)
    elif edges >= choose(nodes - 1, 2) + 1:
        ways = choose(choose(nodes, 2), edges)
    elif (nodes, edges) in cache:
        ways = cache[(nodes, edges)]
    else:
        possible_graphs = choose(max_edges, edges)
        ways = possible_graphs - unconnected_graphs(nodes, edges)
        cache[(nodes, edges)] = ways
    return (ways)

class Tests(unittest.TestCase):
  def test_tunnel_possibilities(self):
    nodes = 1
    edges = 0
    count = 0
    with open('possible_tunnels_tests.txt') as tests:
      for line in tests:
        [_, corr] = [int(s) for s in line.split()]
        test = possible_tunnels(nodes, edges)
        print(nodes, edges, test, corr)
        assert test == corr
        edges += 1
        count += 1
        if edges > nodes * (nodes - 1) / 2:
          nodes += 1
          edges = nodes - 1

if __name__ == '__main__':
  unittest.main()

