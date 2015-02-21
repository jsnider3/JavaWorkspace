#include "City.hh"
#include "Quadtree.hh"
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <vector>

int main()
{
  Quadtree *tTree = NULL;
  std::string tLine;
  std::ifstream tFile("Quadtree.in");
  std::vector<City*> tCities;
  while (getline(tFile,tLine))
  {
    std::cout << tLine << std::endl;
    char tName[20];
    int tLat;
    int tLon;
    sscanf(tLine.c_str(), "%s, %d, %d\n", tName, &tLat, &tLon);
    City *tCity = new City(tName, tLat, tLon);
    tCities.push_back(tCity);
    if (tTree == NULL)
    {
      tTree = new Quadtree(*tCity);
    }
    else
    {
      tTree->AddCity(*tCity);
    }
  }
  tFile.close();
  std::cout << "TODO: List cities within a radius of a city." << std::endl;
  std::vector<City> tClose;
  tTree->GetPointsInRange(tClose, *tCities[0], 50000);
  std::cout << "The following cities are close to " << tCities[0]->GetName()
    << "." << std::endl;
  for (const auto &tCloseCity : tClose)
  {
    std::cout << tCloseCity.GetName() << ", " << std::endl;
  } 
  delete tTree;
  for (auto *tCity : tCities)
  {
    delete tCity;
  }  
}

