#include "rainest.h"
#include <stdlib.h>
#define N 151
extern int    yearp,monthp,dayp,minp,hourp;
extern float  Rc[155][155];

/********************************************************************/
void writeday(void)
/********************************************************************/
{
 FILE  *fp;
 char  filename[MAXFILELENGTH],filename1[MAXFILELENGTH];
 int   i,j,yp;

 yp=yearp-1900; /* Modified to hold for every year */
 sprintf(filename,"%02d%02d%02d.1day.rrmap",yp,monthp,dayp);
 sprintf(filename1,"%s/%s",PRODUCT_PATH1,filename);

 fp=fopen(filename1,"w");

 for(i=1;i<=N;i++)
   {
   for(j=1;j<=N;j++)
     fprintf(fp,"%7.1f",Rc[i][j]);
   fprintf(fp,"\n");
   }

 fclose(fp);
}
