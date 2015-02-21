#include "Quadtree.hh"
#include "City.hh"
#include <iostream>

/*
* Constructor
*/
Quadtree::Quadtree(City &aCity)
  :
  _City(aCity),
  _NW(NULL),
  _NE(NULL),
  _SW(NULL),
  _SE(NULL)
{

}

/*
* Destructor
*/
Quadtree::~Quadtree()
{

}

/*
* Make a new Quadtree node from aCity and add it to the tree.
*/
void
Quadtree::AddCity(City &aCity)
{
  std::cout << "TODO: Quadtree::AddCity" << std::endl;

}
