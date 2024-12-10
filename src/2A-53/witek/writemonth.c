#include "rainest.h"
#include <stdlib.h>
extern int    daytot,ytot,montot;
extern float  Rc[155][155];

#define N 151
int system(const char *string);

/********************************************************************/
void writemonth(void)
/********************************************************************/
{
 FILE  *fp;
 char  filename[MAXFILELENGTH],filename1[MAXFILELENGTH];
 int   i,j,yp;

 yp=ytot-1900; /* Modified to hold for every year */
 sprintf(filename,"%02d%02d%02d.30day.rrmap",yp,montot,daytot);
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




