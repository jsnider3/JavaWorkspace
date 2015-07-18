/*
  see: HackerRank -> Data Structures -> Advanced -> Direct Connections
  Author: Josh Snider
  This is faster than the python, but both too slow and wrong.
*/
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

std::vector<int>
getIntLine() {
  std::string line;
  std::getline(std::cin, line);
  std::istringstream in(line, std::istringstream::in);
  int n;
  std::vector<int> v;
  while (in >> n) {
    v.push_back(n);
  }
  return v;
}

int main() {
  int numtests = getIntLine()[0];
  for (int x = 0; x < numtests; x++) {
    int numcities = getIntLine()[0];
    auto coords = getIntLine();
    auto pops = getIntLine();
    int cost = 0;
    for (int f = 0; f < numcities; f++) {
      for (int s = f + 1; s < numcities; s++) {
        int dist = std::abs(coords[f]-coords[s]);
        int pipes = std::max(pops[f], pops[s]);
        cost += dist * pipes;
        if (cost >=  1000000007) {
            cost -=  1000000007;
        }
      }
    }
    std::cout << cost << std::endl;
  }
  return 0;
}
