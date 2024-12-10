#include "rainest.h"

extern int    yearp,monthp,dayp,minp,hourp;
extern float  Rp[360][221],Rpm[360][221],Rph[152][152],Rpd[152][152];
extern int    iflag;
extern double wt1[151][151],wt2[151][151],wt3[151][151];
extern float  Rc[155][155];
extern sweep_header_t sweep;
extern float  Npm[360][221],Nph[152][152];
extern int    indexmp; 
extern int    indexcl;
extern int    ipass;
extern int    shiftmin;
extern float  Rp5d[152][152],Rpt[152][152];
extern int    icounter, countday;
extern int    daytot,ytot,montot;
extern int    day5d,y5d,mon5d;
extern int    counttot;

void accumulation(void)
/* This subroutine transforms the rainfall products from polar to 
   Cartesian coordinates and computes the instantaneous, hourly,  
   dayly and monthly accumulations. It also includes an advection correction
   scheme, which is applied on the instanteneous maps which lag 10 minutes.
   It is based on The University of Iowa teams rainfall estimation framework. 
   Author: E.N. Anagnostou, Iowa Institute of Hydraulic Research,
           The University of Iowa, 1994. 
*/   
{
  int      i,j,dif;
  float    disx,disy,diss;

/* The first time we enter this subroutine */
     if(iflag == 1)
       {
       /* Compute the vertically integrated instanteneous rainfall field in polar  */
       /* coordinates. Initialization of the inst. field for the next accumulation */ 
       for(i=0;i<=359;i++)                     
         for(j=0;j<=199;j++)
           {
           if(Npm[i][j] != 0.0)
	     Rp[i][j]=(float)(Rpm[i][j]/Npm[i][j]);
           else
	     Rp[i][j]=(float)0.0;
           Rpm[i][j]=0.0;
           Npm[i][j]=0.0;
           }

       poltocart();    /* Transform from polar to cartesian */

       /* Out of boundary assignment (-99.0) */

       for(i=1;i<=151;i++)                     
         for(j=1;j<=151;j++)
           {
           disx=(float)pow((double)(i*2.0-151.0),2.0);
           disy=(float)pow((double)(j*2.0-151.0),2.0);
           diss=(float)pow((double)(disx+disy),0.5);
           if(diss <= 151.0)
             Rc[i][j] = Rc[i][j];
           else
             Rc[i][j] = -99.0;
           }

       for(i=0;i<=150;i++)                     
         for(j=0;j<=150;j++)
           wt1[i][j]=(double)Rc[i+1][j+1];

       if(indexmp == 1)      /* Write only the instantaneus maps */
         {
         iflag=1;
         writeminute();
         }
       else if(indexmp == 2) /* Write only the hour/day/5days/month acc. */
	 {
         iflag=2;         
         writeminute();
         }
       else
	 iflag=2;
       goto L2;
       }

/* All the other times that we enter this subroutine */
     if(iflag == 2)
       {
       /* Compute the vertically integrated instanteneous rainfall field in polar  */
       /* coordinates. Initialization of the inst. field for the next accumulation */ 
       for(i=0;i<=359;i++)                     
         for(j=0;j<=199;j++)
           {
           if(Npm[i][j] != 0.0)
	     Rp[i][j]=(float)(Rpm[i][j]/Npm[i][j]);
           else
	     Rp[i][j]=(float)0.0;
           Rpm[i][j]=0.0;
           Npm[i][j]=0.0;
           }

       /* Transform from polar to cartecian coordinates */
       poltocart();

       /* Write the instantaneous field into a file */
       if(indexmp == 2) writeminute();
       
       /* Compute the time lag between the instanteneous fields */
       dif=(int)(sweep.minute-minp);

       /* If the time lag is grater than 10 minutes then take only the */
       /* current map in the calculations, no advection correction */
       if(dif > 11)
         {
         for(i=0;i<=150;i++)                     
           for(j=0;j<=150;j++)
             {
             wt3[i][j]=(double)Rc[i+1][j+1];
             wt1[i][j]=(double)Rc[i+1][j+1];
             }
         }
       /* if time lag less or equal to 10 minutes do advection correction */
       else
         {
         for(i=0;i<=150;i++)                     
           for(j=0;j<=150;j++)
             wt2[i][j]=(double)Rc[i+1][j+1];
         advective_accum();
         for(i=0;i<=150;i++)
           for(j=0;j<=150;j++)
             wt1[i][j]=wt2[i][j];
         }                          
       }

     /* Accumulate the instantaneous maps to compute the hourly accumulations */
     for(i=0;i<=150;i++)                     
       for(j=0;j<=150;j++)
         {
	 Rph[i][j] += (float)wt3[i][j];
         Nph[i][j] += 1.0;
         }

     if(sweep.hour != hourp) ipass = 1;   /* Check if we are within the hour */

     /* Compute the hourly accumulated rainfall field (minutes shift)       */
     if(sweep.minute > shiftmin && ipass == 1)  
       {
       for(i=0;i<=150;i++)                     
         for(j=0;j<=150;j++)
           {
	   Rc[i+1][j+1] = Rph[i][j]/Nph[i][j];
           Rpd[i][j]   += Rc[i+1][j+1];
           Rp5d[i][j]  += Rc[i+1][j+1];
           Rpt[i][j]   += Rc[i+1][j+1];
           } 
       icounter = icounter+1;  /* update the hourly rainfield counter        */
       ipass=0;                /* set index ipass=0 to not re-enter this     */
                               /* before the next hour comes.                */   
/*   Set the hourly rainfall matrix to zero for the new accumulation         */          
       for(i=0;i<=150;i++)                     
         for(j=0;j<=150;j++)
           {
           Rph[i][j]=0.0;
           Nph[i][j]=0.0;
           }
       }

     /* Compute the dayly accumulated rainfall fields         */
     if(sweep.day != dayp)
       {
       if(countday == 0) {day5d=dayp; y5d=yearp; mon5d=monthp;}
       if(counttot == 0) {daytot=dayp; ytot=yearp; montot=monthp;}
       countday += 1;
       counttot += 1;
       for(i=0;i<=150;i++)                     
         for(j=0;j<=150;j++)
	   Rc[i+1][j+1]=Rpd[i][j];           
       writeday();             /* write into the file in ASCII form          */  
/*   Set the dayly rainfall matrix to zero for the new accumulation          */          
       for(i=0;i<=150;i++)                     
         for(j=0;j<=150;j++)   
           Rpd[i][j]=0.0;
       }

     /* Compute the 5 days accumulated rainfall fields         */
     if(countday == 5)
       {
       countday=0;
       for(i=0;i<=150;i++)                     
         for(j=0;j<=150;j++)
	   Rc[i+1][j+1]=Rp5d[i][j];             
       write5day();            /* write into the file in ASCII form          */  
/*   Set the dayly rainfall matrix to zero for the new accumulation          */          
       for(i=0;i<=150;i++)                     
         for(j=0;j<=150;j++)     
           Rp5d[i][j]=0.0;
       }

     /* Compute the monthly accumulated rainfall fields         */
     if(counttot == 25)
       {
       counttot=0;
       for(i=0;i<=150;i++)                     
         for(j=0;j<=150;j++)
	   Rc[i+1][j+1]=Rpt[i][j];            
       writemonth();           /* write into the file in ASCII form          */  
/*   Set the dayly rainfall matrix to zero for the new accumulation          */          
       for(i=0;i<=150;i++)                     
         for(j=0;j<=150;j++)   
           Rpt[i][j]=0.0;
       }

  L2:  yearp=sweep.year;
}
