#ifndef _City_hh
#define _City_hh
#include <string>

class City
{

  public:

  enum Direction
  {
    eNORTH_WEST,
    eNORTH_EAST,
    eSOUTH_WEST,
    eSOUTH_EAST
  };

  /*
  * Constructor
  */
  City(std::string aName, int aLat, int aLon);

  /*
  * Destructor
  */
  ~City();

  /*
  * Equals.
  */
  bool
  operator==(const City &aCity) const;

  /*
  * Not equals.
  */
  bool
  operator!=(const City &aCity) const;

  /*
  * Return what direction aCity is in.
  */
  Direction
  GetDirection(City &aCity) const;

  /*
  * Returns _Name.
  */
  std::string
  GetName() const;

  /*
  * Returns the distance to aCity.
  */
  double
  GetDistance(City &aCity) const;

  private:
  const std::string _Name;
  const int         _Lat;
  const int         _Lon;

};

#endif //_City_hh 
