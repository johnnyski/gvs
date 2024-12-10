#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "defs.h"
#include <time.h>


// This function returns the latitude and longitude (in degrees)
// of a point x m east of (clat, clon) and y m north of (clat, clon).
// clat and clon are assumed to be in degrees.
// modified from Joe's program, calculations do not assume
// that the earth is flat SEY 22 Aug 1996
static const float pi = 3.141592654;
static const float earth_rad_zeb = 6378000.0; // radius of earth in zebra
// clon and clat are in degrees
// deltax and deltay are in meters
int map_sphere(const float clat, const float clon,
        const float deltax, const float deltay,
        float &lat, float &lon) 
{
    double deltalat, deltalon;

    // do latitude first theta = s/R
    deltalat = deltay/earth_rad_zeb;
    lat = deltalat * 180.0/pi + clat;

    // now long, have to adjust the R
    // use cos of clat
    deltalon = deltax/(earth_rad_zeb * cos (clat * pi / 180.0));
    lon = deltalon * 180.0/pi + clon;

    // center the longitude from -180 to 180
    lon = (float)fmod(lon+540.0,360.0) - 180;

  return(1);
}


// need for timetagging diag
// adapted from /home/disk/anvil/joe/lib/src/JetSys  
// program JetSys::getCurrentDate
int getCurrentGMTDate(char datestr[13])
			   
{
int zeroPad(char dig2[2], int number);

    int t_year,t_mon,t_day,t_hour,t_min,t_sec;
    char year[2], mon[2],day[2],hour[2],min[2],sec[2];
  time_t tt = time(0);
  struct tm *tmc = gmtime(&tt);
 
  t_year = tmc->tm_year;
  t_mon  = tmc->tm_mon + 1;                  // months go from 0 - 11
  t_day  = tmc->tm_mday;
  t_hour = tmc->tm_hour;
  t_min  = tmc->tm_min;
  t_sec  = tmc->tm_sec;

    // add the appropriate 0 padding
    zeroPad(year,t_year);
    zeroPad(mon,t_mon);
    zeroPad(day,t_day);
    zeroPad(hour,t_hour);
    zeroPad(min,t_min);
    zeroPad(sec,t_sec);
  
  sprintf(datestr,"%s%s%s.%s%s%s",year,mon,day,hour,min,sec);
  return(1);
}

int zeroPad(char dig2[2], int number)
{
if (number < 10)
  sprintf(dig2,"0%d",number);
else
   sprintf(dig2,"%d",number);
return(1);
} // end zeroPad

// convert from lat long to x y using the zebra conventions
// adapted from cvt_ToXY in /zebra/lib/convert.c
// uses equidistant cylindrical (rectangular projection)
// arithmetic is in mod(180 deg) the greatest distance
// between two longitudes is +/- 180 (PI R) and the greatest
// distance between two latitudes is +/- 90 deg (PI R)
// assumes lat and lon in degrees
// clat and clon are the zeb origin
// returns x and y in km
int latlongtoXY(const float clat, const float clon,
        const float lat, const float lon,
        double &x, double &y) 
{
    
    double deltalat, deltalon;

    // find the angle difference in radians
    deltalat = lat * (pi/180) - (clat * (pi/180));
    deltalon = lon * (pi/180) - (clon * (pi/180));

    // handle the dateline
    if (fabs(deltalon) > pi) {
	if (deltalon < 0) 
	  deltalon +=2*pi;
	else
	  deltalon -=2*pi;
    }

    x = earth_rad_zeb/1000 * cos (clat) * deltalon;
    y = earth_rad_zeb/1000 * deltalat;

    return(1);
}

// given x and y km find the nearest pixel in array indexes
// indexes are between 0 and (arsize-1)
int xytoArrayIndex(const double xkm, const double ykm,
		   int arsize, float pixelkm, 
		   int flip, int &index_x, int &index_y)
{
    float centerindex = (arsize/2) -0.5;
    index_x =  irint(centerindex+(xkm/pixelkm));
    if (flip) {
	index_y = irint(centerindex-(ykm/pixelkm));
    }
    else {
	index_y = irint(centerindex+(ykm/pixelkm));
    }

    if ((index_x < 0) || (index_x > (arsize-1)) ||
	(index_y < 0) || (index_y > (arsize-1))) {
	return(0); // out of bounds
    }
    else return(1);

}


	       
		
