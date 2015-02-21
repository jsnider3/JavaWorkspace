#ifndef _City_hh
#define _City_hh
#include <string>

class City
{

  public:

  /*
  *
  */
  City(std::string aName, int aLat, int aLon);

  /*
  *
  */
  ~City();

  private:
  std::string _Name;
  int         _Lat;
  int         _Lon;

};

#endif //_City_hh 
