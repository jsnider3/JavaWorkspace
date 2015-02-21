#ifndef _Quadtree_hh
#define _Quadtree_hh

class City;

class Quadtree
{
  public:

  /*
  *
  */
  Quadtree(City &aCity);

  /*
  *
  */
  ~Quadtree(); 

  /*
  *
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
