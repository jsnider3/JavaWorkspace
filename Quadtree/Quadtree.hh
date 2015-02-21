#ifndef _Quadtree_hh
#define _Quadtree_hh

#include "City.hh"
#include <vector>

class Quadtree
{
  public:

  /*
  * Constructor
  */
  Quadtree(City &aCity);

  /*
  * Destructor
  */
  ~Quadtree(); 

  /*
  * Make a new Quadtree node from aCity and add it to the tree.
  */
  void
  AddCity(City &aCity);

  /*
  * Get the child in the given direction.
  */
  Quadtree**
  GetChild(City::Direction aDirection);

  /*
  * Get the child in the given direction.
  */
  void
  GetPointsInRange(std::vector<City> aCities, City &aCity, int aRange);

  private:

  City &_City;
  Quadtree *_NW;
  Quadtree *_NE;
  Quadtree *_SW;
  Quadtree *_SE;
};

#endif //_Quadtree_hh
