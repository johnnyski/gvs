#include <stdio.h>

int main(int argc, char**argv)
{
  if (argc < 3) return(0);

  int xdim = 151;
  int ydim = 151;

  int geomap[151][151];

/*
  FILE *fp = fopen(argv[1], "r");
  if (!fp)
    {
      fprintf(stderr, "Sorry, can't open input file %s\n",
	      argv[1]);
      return(1);
    }
*/

  // This file contains 201x201 pixels at 2km resolution with center at
  // pixel (101,101).  We extract a 151x151 grid centered at 101x101.
  // Hence, we throw away 25 pixels on every edge of the map.
  int tmap[201][201];
  {
    for (int j = 0; j < 201; j++)
      {
	for (int i = 0; i < 201; i++)
	  {
//	    fscanf(fp, "%d", &tmap[j][i]);
	    int i2 = i - 25;
	    int j2 = j - 25;
	    if (i2 < 0) continue;
	    if (j2 < 0) continue;
	    if (i2 >= xdim) continue;
	    if (j2 >= ydim) continue;
	    geomap[ydim - 1 - j2][i2] = 1;
	    if ((j==100) &&(i==100))
	    fprintf(stderr, "%d %d\n", ydim -1-j2, i2);
	  }
      }
  }
  
//  fclose(fp);

  FILE *asciif = fopen("ascii.map", "w");

  {
    for (int j = 0; j < ydim; j++)
      {
        char line[151*10];
	for (int i = 0; i < xdim; i++)
	  {
	    int lf = geomap[j][i];

	    //line[2*i+1] = ' ';
	    if (geomap[j][i] == 1)
	      {
		line[i] = '1';
	      }
	    else
	      {
		line[i] = '0';
	      }

	    fprintf(asciif, "%1d ", lf);
	  }
	fprintf(stdout, "%s\n", line);
	fprintf(asciif, "\n");
	fflush(stdout);
      }
  }
  
  fclose(asciif);

  {
    FILE *fp2 = fopen(argv[2], "w");
    if (fp2)
      {
	fwrite(&xdim, sizeof(xdim), 1, fp2);
	fwrite(&ydim, sizeof(ydim), 1, fp2);
	fwrite(geomap, sizeof(geomap), 1, fp2);
	fclose(fp2);
      }
    else
      {
	fprintf(stderr, "Error creating geographical map file.\n");
      }
  }

  return(0);
}
