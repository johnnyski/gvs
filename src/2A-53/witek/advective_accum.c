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

extern sweep_header_t sweep;
extern ray_header_t   ray; 

#define NN 151

int advective_accum(void)
/***
Advective accumulation procedure. 
Input parameters:
  wt1,wt2   - two consecutive rainfall  maps (wt2 is the current one), 
  wn_search - search area for calculation of the rainfield movement by 
              cross-correlation in terms of the map grid steps. Area 
              from -wn_search to +wn_search in both directions is searched.  
  wn_steps  - number of subintervals of the time interval between 'wt1' and 
              'wt2' for accumulation purposes. 
  wflag     - ON/OFF flag for the advection procedure.  For wflag=0 only the 
              map wt2 is substituted to wt3 and the procedure quits. 
Output parameter:
  wt3 - resulting accumulation map. 

Written by: G.J. Ciach, Iowa Inst. of Hydraulic Research, 2/12/95. 
***/

{
  double  eps, dbl_NN, cov_max,cov,cov_n;
  int     n,iy,ix,iy1,ix1,iy2,ix2,iym,ixm,iy_beg,iy_end,ix_beg,ix_end,idiy,idix;
  double  acc[NN][NN],acc_n[NN][NN];
  double  ac1,ac2, a1,a2, diy1,dix1, diy2,dix2, aiy1,aix1, aiy2,aix2;

  if(!wflag) {
    for(iy=0;iy<NN;iy++) for(ix=0;ix<NN;ix++) wt3[iy][ix]=wt2[iy][ix];
    return(0);
  }

  eps=0.001;
  dbl_NN=(double)(NN-1);
  for(iy=0;iy<NN;iy++) for(ix=0;ix<NN;ix++) acc[iy][ix]=acc_n[iy][ix]=0.0;

  cov_max=-1e9; iym=ixm=0;
  for(iy1=-wn_search;iy1<=wn_search;iy1++) for(ix1=-wn_search;ix1<=wn_search;ix1++) {
    cov=cov_n=0.0;
    iy_beg=-iy1; if(iy_beg<0) iy_beg=0;  iy_end=NN-iy1; if(iy_end>NN) iy_end=NN;
    ix_beg=-ix1; if(ix_beg<0) ix_beg=0;  ix_end=NN-ix1; if(ix_end>NN) ix_end=NN;
    for(iy=iy_beg;iy<iy_end;iy++) for(ix=ix_beg;ix<ix_end;ix++) {
      iy2=iy+iy1; ix2=ix+ix1; 
      cov+=wt1[iy][ix]*wt2[iy2][ix2]; cov_n++;
    }
    if(cov_n) {cov/=cov_n; if(cov>cov_max) {cov_max=cov; iym=iy1; ixm=ix1;}}
  }
  idiy=iym; idix=ixm;

  for(n=0;n<=wn_steps;n++) {
    a1=(double)n/(double)wn_steps; a2=1.0-a1;
    ac1=1.0-a1; ac2=1.0-a2;
    diy1=a1*(double)idiy; dix1=a1*(double)idix; 
    diy2=a2*(double)idiy; dix2=a2*(double)idix; 
    for(iy=0;iy<NN;iy++) for(ix=0;ix<NN;ix++) {
      aiy1=(double)iy-diy1; aix1=(double)ix-dix1; 
      if(aiy1>eps && aiy1<dbl_NN-eps && aix1>eps && aix1<dbl_NN-eps) {
        iy1=(int)(aiy1+0.5); ix1=(int)(aix1+0.5);
        acc[iy][ix]+=ac1*wt1[iy1][ix1];  acc_n[iy][ix]+=ac1;
      }
      aiy2=(double)iy+diy2; aix2=(double)ix+dix2;
      if(aiy2>eps && aiy2<dbl_NN-eps && aix2>eps && aix2<dbl_NN-eps) {
        iy2=(int)(aiy2+0.5); ix2=(int)(aix2+0.5);
        acc[iy][ix]+=ac2*wt2[iy2][ix2];  acc_n[iy][ix]+=ac2;
      }
    }
  }

  for(iy=0;iy<NN;iy++) for(ix=0;ix<NN;ix++) {
    wt3[iy][ix]=0.0; if(acc_n[iy][ix]>eps) wt3[iy][ix]=acc[iy][ix]/acc_n[iy][ix]; 
  }
  return(1);
}/*advective_accum*/
/*** END ************************************************************************/

