#include "rainest.h"
extern int    icounter;
extern int    iflag;
extern int    ipass;
extern int    countday;
extern int    counttot;
extern float  Rpm[360][221],Rph[152][152],Rpd[152][152];
extern float  Rp5d[152][152],Rpt[152][152];

extern float  Npm[360][221],Nph[152][152];

/***********************************************************/
void settozero(void)
/***********************************************************/
{
 int i,j;

   for(i=0;i<=359;i++)
     for(j=0;j<=219;j++)
       {
       Rpm[i][j]  = 0.0;
       Npm[i][j]  = 0.0;
       }

   for(i=0;i<=150;i++)
     for(j=0;j<=150;j++)
       {
       Rph[i][j]  = 0.0;
       Rpd[i][j]  = 0.0;
       Rp5d[i][j] = 0.0;
       Rpt[i][j]  = 0.0;
       Nph[i][j]  = 0.0;
       }

  countday=0;
  counttot=0;
  icounter=1;
  iflag=1;
  ipass=0;
}
