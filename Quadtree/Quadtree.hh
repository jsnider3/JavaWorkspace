#ifndef _Quadtree_hh
#define _Quadtree_hh

class City;

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

  private:
  City &_City;
  Quadtree *_NW;
  Quadtree *_NE;
  Quadtree *_SW;
  Quadtree *_SE;
};

#endif //_Quadtree_hh
