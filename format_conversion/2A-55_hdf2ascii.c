#include <stdio.h>
#include <signal.h>
/*
 * By: John Merritt
 *     Applied Research Corporation
 *     Landover, MD
 *     John.Merritt@gsfc.nasa.gov
 */

#include <gv_utils.h>
/* TSDIS toolkit include files*/
/* setenv TSDISTK /usr/local/toolkit_2.5 */
#include "IO_GV.h"

/* From volume.h */
#define MAX_X_DIM       160               /* maximum x-array dimension */
#define MAX_Y_DIM       160               /* maximum y-array dimension */
#define MAX_Z_DIM        17               /* maximum z-array dimension */

static void handler(int sig)
{
  fprintf(stderr, "Got signal %d. Abort.\n", sig);
  if (sig == SIGINT || sig == SIGKILL || sig == SIGSTOP) {
	exit (-2);
  }
  exit(-1);
}
int main(int argc, char **argv)
{
  int i, j, k;
  L2A_55_SINGLE_RADARGRID hdf_product;
  IO_HANDLE               hdf_fd;
  int                     dataType;
  char                    filemode;
  int                     status;
  char *hdffile;
  int zdim=13, xydim=151;
  char out[30];
  FILE *of;


  set_signal_handlers();
  if (argc != 2) {
	fprintf(stderr,"Usage (%s): %s 2A-55.hdf\n", PROG_VERSION, argv[0]);
	exit(-1);
  }


  /*
  	typedef struct
  {
    RADARGRID_STR			radarGridStr;
    TIME_STR			tktime;
    int16				threeDreflect_scale[13][151][151];
    float32			threeDreflect[13][151][151];
    int16				vertProfile_scale[13][12];
    float32			vertProfile[13][12];
    int32				cfadData[13][86][12];
  } L2A_55_SINGLE_RADARGRID;
  */
  dataType = TK_L2A_55S;
  filemode = TK_READ_ONLY;
  hdffile  = argv[1];
  status = TKopen(hdffile, dataType, filemode, &hdf_fd);
  
  if (status != TK_SUCCESS) {
	fprintf(stderr, "TKopen failed for file <<%s>>.\n", hdffile);
	exit(-1);
  }

  /* READ the hdf product. */
  status = TKreadGrid(&hdf_fd, &hdf_product);
  if (status != TK_SUCCESS) {
	fprintf(stderr, "TKwriteGrid failed.  Current file is <<%s>>.\n", hdffile);
	exit(-1);
  }


  /* 3D reflectivities. -- works 6/10/96 */
  for (k = 0; k < zdim; ++k) {
	sprintf(out, "hdfslice%2.2d.dat", k);
	of = fopen(out,"w");
	  for (i = 0; i < xydim; ++i) {
		for (j = 0; j < xydim; ++j) {
		  if (hdf_product.threeDreflect[k][i][j] > TK_DEFAULT)
			fprintf(of, " %.2f", hdf_product.threeDreflect[k][i][j]);
		  else 
			fprintf(of, " %.2f", -99.0);
	  }
	  fprintf(of, "\n");
	}
	fclose(of);
  }
  
  /* CFAD */
  for (j = 0; j < 12; ++j) {
	sprintf(out, "hdfcfad%2.2d.dat", j);
	of = fopen(out,"w");
	for (i = 0; i < 86; ++i) {
	  for (k = 0; k < zdim; ++k)
		fprintf(of, "%6d", hdf_product.cfadData[k][i][j]);
	  fprintf(of,"\n");
	}
	fclose(of);
  }

  /* Vertical Profile */
  sprintf(out, "hdfvprof.dat");
  of = fopen(out,"w");
  for (j = 0; j < 12; ++j) {
	for (k = 0; k < zdim; ++k)
	  if (hdf_product.vertProfile[k][j] > TK_DEFAULT)
		fprintf(of, " %8.2f", hdf_product.vertProfile[k][j]);
	  else 
		fprintf(of, " %8.2f", -99.0);
	fprintf(of,"\n");
  }
  fclose(of);

  if (TKclose(&hdf_fd) == TK_FAIL) {
	fprintf(stderr, "TKclose failed.  Current file is <<%s>>.\n", hdffile);
	exit(-1);
  }

} 


  
