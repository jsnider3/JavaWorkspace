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
*
*/
Quadtree::~Quadtree()
{

}

/*
*
*/
void
Quadtree::AddCity(City &aCity)
{
  switch (_City.GetDirection(aCity))
  {
    case City::eNORTH_WEST:
      if (_NW == NULL)
      {
        _NW = new Quadtree(aCity);
      }
      else
      {
        _NW->AddCity(aCity);
      }
      break;
    case City::eNORTH_EAST:
       if (_NE == NULL)
      {
        _NE = new Quadtree(aCity);
      }
      else
      {
        _NE->AddCity(aCity);
      }
      break;
    case City::eSOUTH_WEST:
      if (_SW == NULL)
      {
        _SW = new Quadtree(aCity);
      }
      else
      {
        _SW->AddCity(aCity);
      }
      break;
    case City::eSOUTH_EAST:
      if (_SE == NULL)
      {
        _SE = new Quadtree(aCity);
      }
      else
      {
        _SE->AddCity(aCity);
      }
      break;
  }
}
