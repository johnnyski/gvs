/*
 * Accumulate 2A-55 cfad's in preparation for 3A-55 generation.
 *
 * By John H. Merritt
 *    Space Applications Corporation
 *    Vienna VA
 *    10/7/97
 *
 */

/*
 * Usage:
 *
 *    accu_cfad [-v] infile intermediate
 *
 * This program can be run stand-alone, but, it is typcially
 * invoked from the 3A-55 script via the BEGIN or MIDDLE flag.
 * See 3A-55 for more information.
 */

#include <stdio.h>
#ifdef __sgi
  #include <getopt.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <math.h>


void usage(char **argv)
{
  fprintf(stderr,"Usage (%s)\n", PROG_VERSION);
  fprintf(stderr, "   %s  [-v] infile intermediate\n", argv[0]);
  fprintf(stderr, "\n");
  fprintf(stderr, "  'infile' is a concatination of cfads (ascii)\n");
  fprintf(stderr, "  'intermediate' is an ascii format and represents\n");
  fprintf(stderr, "  a concatination of all cfads.\n");
  fprintf(stderr, "\n");
  fprintf(stderr, " * This program can be run stand-alone, but, it is typcially\n");
  fprintf(stderr, "invoked from the 3A-55 script via the BEGIN or MIDDLE flag.\n");
  fprintf(stderr, "See 3A-55 for more information.\n");
  return;
}

void process_args(int argc, char **argv,
				  char **infile,
				  char **intermediate,
				  int  *verbose)

{
  extern char *optarg;
  extern int optind, opterr, optopt;


  int c;
  
  while ((c = getopt(argc, argv, "v")) != -1)
	switch (c) {
	case 'v': *verbose = 1;  break;
	case '?': usage(argv); exit(-1); break;
	default:  break;
	}

/* must have 2 files listed */
  if (argc - optind != 2) {
	usage(argv);
	exit(-1);
  }

  *infile        = strdup(argv[optind]);
  *intermediate  = strdup(argv[optind+1]);

}

typedef int Cfads[13][86][12];

int main (int argc, char **argv)
{

  char *intermediate;
  char *infile;
  Cfads inter_cfads;
  int  verbose;
  FILE *fp;
  int i, j, k, ival;
  int zdim=13;

  verbose = 0;
  process_args(argc, argv,
			   &infile,
			   &intermediate,
			   &verbose);
  
  /*
   * If the intermediate contains the total accumulation of 
   * points for the monthly cfad.  Use 3A-55ascii_to_hdf when
   * outputting to HDF.  The END flag specified on the
   * 3A-55 command (see 3A-55) triggers the execution of 3A-55ascii_to_hdf.
   */

  /* ASCII format for intermediate. */
  fp = fopen(intermediate,"r");
  /* First time around the intermediate accumulation file may not
   * exist.  Ignore: initialize to zero.
   */
  if (fp == NULL) {
	memset(&inter_cfads, 0, sizeof(inter_cfads));
	if (verbose) {
	  fprintf(stderr, "Warning: ");
	  perror(intermediate);
	}
  } else {
	/* CFAD */
	for (j = 0; j < 12; ++j) {
	  for (i = 0; i < 86; ++i) {
		for (k = 0; k < zdim; ++k)
		  fscanf(fp, "%6d", &inter_cfads[k][i][j]);
		fscanf(fp,"\n");
	  }
	}
	fclose(fp);
  }

  /* The input file 'infile' contains all 13 cfads (a concatination).
   */
  fp = fopen(infile,"r");
  for (j = 0; j < 12; ++j) {
	for (i = 0; i < 86; ++i) {
	  for (k = 0; k < zdim; ++k) {
		fscanf(fp, "%6d", &ival);
		inter_cfads[k][i][j] += ival;
	  }
	  fscanf(fp,"\n");
	}
  }

  fclose(fp);
  
  /* Output: ASCII format for intermediate cfads. */
  fp = fopen(intermediate,"w");
  for (j = 0; j < 12; ++j) {
	for (i = 0; i < 86; ++i) {
	  for (k = 0; k < zdim; ++k)
		fprintf(fp, "%6d", inter_cfads[k][i][j]);
	  fprintf(fp,"\n");
	}
  }
  fclose(fp);

  exit(0);
}
