#include "City.hh"
#include <iostream>
#include "math.h"

/*
*
*/
City::City(std::string aName, int aLat, int aLon)
  :
  _Name(aName),
  _Lat(aLat),
  _Lon(aLon)
{
  //std::cout << aName << "={" << aLat << ", " << aLon << "}" << std::endl;
}

/*
*
*/
City::~City()
{

}

/*
*
*/
bool
City::operator==(const City &aCity) const
{
  return _Name == aCity._Name &&
         _Lat == aCity._Lat &&
         _Lon == aCity._Lon;
}

/*
*
*/
bool
City::operator!=(const City &aCity) const
{
  return !(*this == aCity);
}
/*
*
*/
City::Direction
City::GetDirection(City &aCity) const
{
  bool tNorth = _Lat < aCity._Lat;
  bool tEast = _Lon < aCity._Lon;
  if (tNorth)
  {
    if (tEast)
    {
      std::cout << aCity.GetName() << " is northeast of " << _Name << std::endl;
      return eNORTH_EAST;
    }
    else
    {
      std::cout << aCity.GetName() << " is northwest of " << _Name << std::endl;
      return eNORTH_WEST;
    }
  }
  else
  {
    if (tEast)
    {
      std::cout << aCity.GetName() << " is southeast of " << _Name << std::endl;
      return eSOUTH_EAST;
    }
    else
    {
      std::cout << aCity.GetName() << " is southwest of " << _Name << std::endl;
      return eSOUTH_WEST;
    }
  }
}

/*
*
*/
std::string
City::GetName() const
{
  return _Name;
}

/*
*
*/
double
City::GetDistance(City &aCity) const
{
  double kEarthRadius = 6371.0;
  double tLat1 = _Lat * M_PI / 180.0;
  double tLon1 = _Lon * M_PI / 180.0;
  double tLat2 = aCity._Lat * M_PI / 180.0;
  double tLon2 = aCity._Lon * M_PI / 180.0;
  double tDist = kEarthRadius * acos(sin(tLat1) * sin(tLat2) + 
          cos(tLat1) * cos(tLat2) * cos(tLon2 - tLon1));
  return tDist;
}
