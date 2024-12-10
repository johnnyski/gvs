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
extern char   carttopol_tbl[100],poltocart_tbl[100],height_tbl[100];

extern sweep_header_t sweep;
extern ray_header_t   ray; 

/***********************************************************/
void carttopol(void)
/***********************************************************/
/* This subroutine uses a look-up table to transform fields from
   cartesian to polar coordinates.  The look-up table has been 
   created by an independent program.
   Author: Emmanouil N. Anagnostou, Iowa Institute of Hydraulic Research,
           The University of Iowa, 1994.
*/ 
{
 FILE  *fp;
 float weight,val,zcat;
 int   i,j,ii,jj,k1,K;
 int   x,y;

 fp=fopen(carttopol_tbl,"r");
 if (fp == NULL) {
   printf("Can't open %s\n",carttopol_tbl);
   exit(-1);
 }
 for(i=0;i<=359;i++)
   for(j=0;j<=219;j++)
     categ[i][j]=0;
 
 for(i=1;i<=360;i++)
   for(j=1;j<=167;j++)
     {
     val=0.0;
     zcat=0.0;
     fscanf(fp,"%d %d %d",&ii,&jj,&K);
     for(k1=1;k1<=K;k1++)
       {
       fscanf(fp,"%f %d %d",&weight,&x,&y);
       if(x <= 151 && y <= 151)
         {
         if(map[x-1][y-1] > 0)
           {
           val   += weight*map[x-1][y-1];
           zcat  += weight;
           }
         }
       }
     if(zcat > 0.0)
       categ[i-1][j-1]=(int)(val/zcat);
     }
 fclose(fp);
}
