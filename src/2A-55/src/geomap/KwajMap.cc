#include <stdio.h>

int main(int argc, char**argv)
{
  /* commented-out for (possibly) temporary fix
  if (argc < 2) return(0);
  */

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
  {
    for (int j = 0; j < 201; j++)
      {
	for (int i = 0; i < 201; i++)
	  {
	    int i2 = i - 25;
	    int j2 = j - 25;
	    if (i2 < 0) continue;
	    if (j2 < 0) continue;
	    if (i2 >= xdim) continue;
	    if (j2 >= ydim) continue;
	    geomap[ydim - 1 - j2][i2] = 0;
	    if ((j==100) &&(i==100))
	    fprintf(stderr, "%d %d\n", ydim -1-j2, i2);
	  }
      }
  }
  
//  fclose(fp);

  FILE *asciif = fopen("kwaj_landsea_new.map.ascii", "w");
  

  {
    for (int j = 0; j < ydim; j++)
      {
	for (int i = 0; i < xdim; i++)
	  {
	    int lf = geomap[j][i];

	    fprintf(asciif, "%1d ", lf);
	  }
	fprintf(asciif, "\n");
      }
  }
  
  fclose(asciif);

  {
    /* commented-out for (possibly) temporary fix
    FILE *fp2 = fopen(argv[1], "w");
     *  Note: a permanent fix would be to make this program like the others in
     *  geomap directory, which involves a series of modifications.  The easy modification
     *  is to pass filename arguments to this program.  The more difficult is to make
     *  the necessary changes for autoconf and automake to have the maps generated during
     *  install, as they are for the other products.
     */

    FILE *fp2 = fopen("kwaj_landsea_new.map.bin", "w");  /* (possibly) temporary fix */
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
