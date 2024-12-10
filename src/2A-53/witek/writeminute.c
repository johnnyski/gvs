#include "rainest.h"
#include <stdlib.h>

extern int    latd, latm, lats, lond, lonm, lons;
extern int    yearp,monthp,dayp,minp,hourp, secp;
extern float  Rc[155][155];
extern char   rrmap_fname[256];
extern int    verbose;

#define N 151
int system(const char *string);

/********************************************************************/
void writeminute(void)
/********************************************************************/
{
 extern void write_rrmap2hdf(float rrmap[155][155],  int hour, int min, 
				int sec, char *lat_str, char *lon_str, char *hdf_filename);

 char lat_str[51], lon_str[51];

 sprintf(lat_str, "%d:%d:%d", latd, latm, lats);
 sprintf(lon_str, "%d:%d:%d", lond, lonm, lons);

 if (verbose) 
   fprintf(stderr, "Writing rrmap to hdf file <%s>...\n", rrmap_fname);


 /* write output to hdf file*/
 write_rrmap2hdf(Rc, hourp, minp, secp, lat_str, lon_str, rrmap_fname);
}
