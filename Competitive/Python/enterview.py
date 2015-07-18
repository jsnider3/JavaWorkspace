#!/usr/bin/env python3
'''
  @see: HackerRank -> Data Structures -> Advanced -> Direct Connections
  @author: Josh Snider
  TODO: Currently times out. time says it takes 2.44 seconds with input5.
'''


def main():
  numtests = int(input(''))
  for _ in range(numtests):
    numcities = int(input(''))
    coords = [int(s) for s in input('').split()]
    pops = [int(s) for s in input('').split()]
    cities = zip(coords,pops)
    cities = sorted(cities)
    cost = 0
    count = len(cities)
    weights = {}
    for x in range(count - 1):
      for y in range(x+1, count):
        #(d1, p1) = city1
        #(d2, p2) = city2
        dist = abs(cities[x][0] - cities[y][0])
        size = max(cities[x][1], cities[y][1])
        cost += dist * size
    print(cost % 1000000007)

if __name__ == '__main__':
  main()
