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
extern int    categ[360][221]; 
extern int    map[CLASS_NX][CLASS_NY];

extern int    wn_search,wn_steps,wflag;
extern double wt1[151][151],wt2[151][151],wt3[151][151];
extern float  Zthre,As,Bs,Ass,Bss,ac,bc,Acc,Bcc;
extern float  htopc,htops,hbots,hbotc;
extern float  arc,brc,ars,brs;
extern float  value[2000];
extern float  Rp[360][221],Rpm[360][221],Rph[152][152],Rpd[152][152];
extern float  Rp5d[152][152],Rpt[152][152];
extern float  Npm[360][221],Nph[152][152];
extern float  Rc[155][155];
extern char   poltocart_tbl[100],carttopol_tbl[100],height_tbl[100];

extern sweep_header_t sweep;
extern ray_header_t   ray; 

/***********************************************************/
void poltocart(void)
/***********************************************************/
/* This subroutine uses a look-up table to transform fields from
   polar to cartecian coordinates.  The look-up table has been 
   created by an independent program.
   Author: Emmanouil N. Anagnostou, Iowa Institute of Hydraulic Research,
           The University of Iowa, 1994.
*/
{
 FILE  *fp;
 float weight;
 int   i,j,ii,jj,k1,K;
 int   theta,range;
 int   N=151;

 fp=fopen(poltocart_tbl,"r");
 if (fp == NULL) {
   printf("Can't open %s\n",poltocart_tbl);
   exit(-1);
 }
 for(i=1;i<=N;i++)
   for(j=1;j<=N;j++)
     {
     Rc[i][j]=0.0;
     fscanf(fp,"%d %d %d",&ii,&jj,&K);
     for(k1=1;k1<=K;k1++)
       {
       fscanf(fp,"%f %d %d",&weight,&theta,&range);
       Rc[i][j] += weight*Rp[theta][range];
       }
     }

 fclose(fp);
}
