#include "City.hh"
#include "Quadtree.hh"
#include "assert.h"
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

int main()
{
  Quadtree *tTree = NULL;
  std::string tLine;
  std::ifstream tFile("Quadtree.in");
  std::vector<City*> tCities;
  while (getline(tFile,tLine))
  {
    std::stringstream tConverter(tLine);
    std::string tName;
    int tLat;
    int tLon;
    /*int tRead = sscanf(tLine.c_str(), "%s, %d, %d", tName, &tLat, &tLon);
    assert(tRead == 3);*/
    tConverter >> tName >> tLat >> tLon;
    City *tCity = new City(tName, tLat, tLon);
    assert(!tConverter.fail());
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
  std::vector<City> *tClose = new std::vector<City>();
  tTree->GetPointsInRange(tClose, *tCities[0], 500);
  std::cout << "The following cities are close to " << tCities[0]->GetName()
    << ":" << std::endl;
  for (const City tCloseCity : *tClose)
  {
    std::cout << "  " + tCloseCity.GetName() << std::endl;
  } 
  delete tTree;
  for (auto *tCity : tCities)
  {
    delete tCity;
  }  
}

