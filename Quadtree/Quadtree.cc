#include "Quadtree.hh"
#include "City.hh"
#include <iostream>

/*
* 
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
  delete _NW;
  delete _NE;
  delete _SW;
  delete _SE;
}

/*
*
*/
void
Quadtree::AddCity(City &aCity)
{
  Quadtree **tDirection = GetChild(_City.GetDirection(aCity));
  if (*tDirection == NULL)
  {
    *tDirection = new Quadtree(aCity);
  }
  else
  {
    (*tDirection)->AddCity(aCity);
  }
}

/*
*
*/
Quadtree**
Quadtree::GetChild(City::Direction aDirection)
{
  switch (aDirection)
  {
    case City::eNORTH_WEST:
      return &_NW;
    case City::eNORTH_EAST:
      return &_NE;
    case City::eSOUTH_WEST:
      return &_SW;
    case City::eSOUTH_EAST:
      return &_SE;
    default:
      std::cout << "Fuck you" << std::endl;
      return &_SE;
  }
}

/*
*
*/
//TODO Can be optimized.
void
Quadtree::GetPointsInRange(std::vector<City> *aCities, City &aCity, int aRange)
{
  //std::cout << "Quadtree::GetPointsInRange() " << _City.GetName() << std::endl;
  if (aCity.GetDistance(_City))
  {
    aCities->push_back(_City);
  }
  if (_NW != NULL)
  {
    _NW->GetPointsInRange(aCities, aCity, aRange);
  }
  if (_NE != NULL)
  {
    _NE->GetPointsInRange(aCities, aCity, aRange);
  }
  if (_SW != NULL)
  {
    _SW->GetPointsInRange(aCities, aCity, aRange);
  }
  if (_SE != NULL)
  {
    _SE->GetPointsInRange(aCities, aCity, aRange);
  }
}
