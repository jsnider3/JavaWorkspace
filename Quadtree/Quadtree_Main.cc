#include "City.hh"
#include "Quadtree.hh"
#include <stdio.h>
#include <iostream>

int main()
{
  City *tChicago = new City("Chicago", 41, 87);
  Quadtree tTree = Quadtree(*tChicago);
  City *tCleveland = new City("Cleveland", 41, 81);
  tTree.AddCity(*tCleveland);
  City *tDayton = new City("Dayton", 39, 84);
  tTree.AddCity(*tDayton);
  City *tLouisville = new City("Louisville", 38, 85);
  tTree.AddCity(*tLouisville);
  City *tNashville = new City("Nashville", 36, 87);
  tTree.AddCity(*tNashville);
  City *tAtlanta = new City("Atlanta", 34, 84);
  tTree.AddCity(*tAtlanta);
  City *tPittsburgh = new City("Pittsburgh", 40, 79);
  tTree.AddCity(*tPittsburgh);
  City *tWashington = new City("Washington", 38, 77);
  tTree.AddCity(*tWashington);
  City *tNewYork = new City("New York", 40, 74);
  tTree.AddCity(*tNewYork);
  City *tMontreal = new City("Montreal", 45, 73);
  tTree.AddCity(*tMontreal);
  std::cout << "TODO: List cities within a radius of a city." << std::endl;
}

