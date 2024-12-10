
#include <stdlib.h>
#include <stdio.h>

#include "rainest.h"

extern char   input_fname[256];
extern int    latd, latm, lats, lond, lonm, lons;
extern int    yearp,monthp,dayp,minp,hourp, secp;
extern int    iflag;
extern int    indexmp; 
extern int    indexcl;
extern int    ipass;
extern int    shiftmin;
extern int    icounter, countday;
extern int    daytot,ytot,montot;
extern int    day5d,y5d,mon5d;
extern int    counttot;
extern int system(const char *string);
extern sweep_header_t sweep;

void products(void)
{
  FILE   *fp;

  /* Open the file with the reflectivity data */
  if ((fp=fopen(input_fname,"r"))==NULL)
    {
    fprintf(stderr,"ERROR: could not open %s\n", input_fname);
    exit(-1);
    }
  setvbuf(fp,NULL,_IOFBF,(size_t)(4096));     /* for IO efficiency */

  while (!feof(fp))
    {
    readSweepHeader(fp,&sweep);               /* Read the next sweep's header.      */                    
    if(sweep.number == 0)                     /* The output filename is based       */
      {                                       /* on the first sweep's time (min,    */
      yearp=sweep.year;                       /* hour) and date (year,month,day).   */
      monthp=sweep.month;
      dayp=sweep.day;
      hourp=sweep.hour;
      minp=sweep.minute;
      secp=sweep.second;
	  latd=sweep.latd;
	  latm=sweep.latm;
	  lats=sweep.lats;
	  lond=sweep.lond;
	  lonm=sweep.lonm;
	  lons=sweep.lons;
	}
    instfield(fp);                            /* Integrate the rainfall vertically.  */
  }
  accumulation();     /* Generate the rainfall products.     */  

  fclose(fp);
}
