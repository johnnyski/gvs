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
extern int    radar_index; 
extern int    shiftmin;  
extern int    categ[360][221]; 
extern int    map[CLASS_NX][CLASS_NY];
extern int    elvmx;
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
#define  BUFFER_SIZE 8192

void instfield(FILE *fp)
/* This subroutine does the vertical integration of the transformed  
   variable (R=AZ^B) and produce the vertically integrated liquid water.
   It is based on The University of Iowa teams rainfall estimation 
   procedure.
   Author: E.N. Anagnostou, Iowa Institute of Hydraulic Research,
           The University of Iowa, 1994. 
*/   
{
 FILE   *fp1;
 char   filename[MAXFILELENGTH],filename1[MAXFILELENGTH];
 int    i,j,k,yp;
 float  height,dis,XX,rdis;
 double phi;

 phi=(double)(3.14159*sweep.elevation/180.0);       /* elevation angle */

 if(radar_index == 2) rdis=range_res*3.0;           /* Range resolution */
 else	              rdis=range_res;

/* Read the Conv/Strat classification map for this instantaneous field */
 if(indexcl == 1)
   {
   if(sweep.number == 0)
     {
     yp=yearp-1900; /* Modified to hold for every year */
     sprintf(filename,"BER.%02d%02d%02d.%02d%02d.csmap",yp,monthp,dayp,hourp,minp);
     sprintf(filename1,"%s/%s",CLASSES_PATH,filename);
     if((fp1=fopen(filename1,"r")) == NULL) goto L9;
     setvbuf(fp1,NULL,_IOFBF,(size_t)BUFFER_SIZE);
   /*  readClassificationMap(fp1,map); */
     carttopol();
     fclose(fp1);
     }
   }
 else
   {
L9: for(i=0;i<=359;i++)
     for(j=0;j<=199;j++)
       categ[i][j]=2;
    }
 
/* Scan the sweep.nonzero azimuths */ 
 for(i=0;i<=sweep.nonzero-1;i++)
   {
   readray(fp);                                /* read the ray values for this azimuth */
   if(sweep.number > elvmx) continue;          /* go up to the Nth elevation angle */

   /* Scan the ray to produce rainfall rates*/  
   for(k=0;k<=199;k++)
     {
     dis=(float)(k+0.5)*rdis;                  /* radar cells distance from the radar    */
     Ass=As*(1.0+ars*dis/150.0);               /* Stratiform range dependent A parameter */
     Bss=Bs*(1.0+brs*dis/150.0);               /* Stratiform range dependent B parameter */
     Acc=ac*As*(1.0+arc*dis/150.0);            /* Convective range dependent A parameter */
     Bcc=bc*Bs*(1.0+brc*dis/150.0);            /* Convective range dependent B parameter */
     j=(int)(dis*(float)cos(phi)/rdis);        /* Correction due to 0.9 km range resolution */
     height=dis*(float)sin(phi)+(float)pow((double)(dis*(float)cos(phi)),(double)2.0)/17000.0;

     /* Compute and accumulate the transformed variable R=(Z/A)^B, R is in mm/h */ 
     if(categ[iray][j] < 2 && height <= htops)
       {
       if(value[k] > 0.0)
         XX=Ass*(float)pow((double)value[k],(double)(1.0/Bss));
       else
         XX=0.0; 
       Rpm[iray][j] += XX;
       Npm[iray][j] += 1.0;
       }

     if(categ[iray][j] < 2 && height > htops && sweep.number == 0)
       {
       if(value[k] > 0.0)
         XX=Ass*(float)pow((double)value[k],(double)(1.0/Bss));
       else
         XX=0.0; 
       Rpm[iray][j] += XX;
       Npm[iray][j] += 1.0;
       }

     if(categ[iray][j] == 2 && height <= htopc)
       {
       if(value[k] > 0.0)
         XX=Acc*(float)pow((double)value[k],(double)(1.0/Bcc));
       else
         XX=0.0; 
       Rpm[iray][j] += XX;
       Npm[iray][j] += 1.0;
       }
  
     if(categ[iray][j] == 2 && height > htopc && sweep.number == 0)
       {
       if(value[k] > 0.0)
         XX=Acc*(float)pow((double)value[k],(double)(1.0/Bcc));
       else
         XX=0.0; 
       Rpm[iray][j] += XX;
       Npm[iray][j] += 1.0;
       }
     }
   }
}
