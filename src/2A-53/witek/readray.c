#include "rsl.h"
#include "rainest.h"

extern  int    Ndays,icounter,iray;
extern int    yearp,monthp,dayp,minp,hourp;
extern int    day5d,y5d,mon5d;
extern int    daytot,ytot,montot;
extern int    iflag;
extern int    ipass;
extern int    countday;
extern int    counttot;
extern int    indexmp; 
extern int    indexcl;
extern int    categ[360][221],hcateg[10][221][11]; 
extern int    map[CLASS_NX][CLASS_NY];

extern int    wn_search,wn_steps,wflag;
extern double wt1[151][151],wt2[151][151],wt3[151][151];
extern float  Zthre,As,Bs,Ass,Bss,ac,bc,Acc,Bcc;
extern float  htopc,htops,hbots,hbotc;
extern float  arc,brc,ars,brs;
extern float  value[2000],range_res;
extern float  Rp[360][221],Rpm[360][221],Rph[152][152],Rpd[152][152];
extern float  Rp5d[152][152],Rpt[152][152];
extern float  Npm[360][221],Nph[152][152];
extern float  Rc[155][155];

extern sweep_header_t sweep;
extern ray_header_t   ray; 

void readray(FILE *fp1)
/* This subroutine reads the ray for each azimuth and it assignes
   it to an aray value[]. For Darwin it averages to 0.9 km resolution.
   This subroutine is part of The University of Iowa team NASA/TRMM 
   AIW project.
   Author: E.N. Anagnostou, Iowa Institute of Hydraulic Research, 1994. 
*/  
{
 int   i,j,k,bins,nint;
 float XX,val[2000],ZZT;
 
 readAzimuth(fp1,sweep,&ray,val);          /* get one azimuth */
 iray = (int)(ray.azimuth+0.5);            /* "DEG" 0=North, 90=East, -90=West */
 if(iray >= 360) iray=iray-360;
 nint=(int)(1.0/range_res);
 ZZT=(float)pow((double)10.0,(double)(Zthre/10.0));
 bins = (int)(ray.nbins/(float)nint);      /* Number of bins normaly equal to  */
 if(bins > 220) bins=220;

 for(i=0;i<=219;i++)
   value[i] = 0.0;

 for(i=0;i<=bins-1;i++)
   {
   for(j=0;j<=(nint-1);j++)
     {
     k=i*nint+j;
     if(val[k] == BADVAL)
       XX=0.0;
     else if(val[k] == RFVAL || val[k] == APFLAG || val[k] == NOECHO)
       XX=0.0;
     else
       XX=(float)pow((double)10.0,(double)(val[k]/10.0));
    value[i] += XX/(nint*1.0);
    }
  if(value[i] < ZZT) value[i]=0.0;
  }
}
