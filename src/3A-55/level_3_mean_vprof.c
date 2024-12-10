/*
 * Compute mean for the 2A-55 vertical profile tables.
 * The input is the intermediate file that contains all the sums
 * and counts.  The intermediate file is used by 'accu_vprof'.
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
 *    mean_vprof [-v] inter_acc_vprof_file
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
  fprintf(stderr, "   %s  [-v] infile\n", argv[0]);
  fprintf(stderr, "\n");
  fprintf(stderr, "  Write to stdout the vertical profile.\n");
  fprintf(stderr, "\n");
  fprintf(stderr, " * This program can be run stand-alone, but, it is typcially\n");
  fprintf(stderr, "invoked from the 3A-55 script via the END flag.\n");
  fprintf(stderr, "See 3A-55 for more information.\n");
  return;
}

void process_args(int argc, char **argv,
				  char **infile,
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

/* must have 1 files listed */
  if (argc - optind != 1) {
	usage(argv);
	exit(-1);
  }

  *infile   = strdup(argv[optind]);

}

typedef double Vprofile[13][12];

typedef struct {
  Vprofile z;
  int      count[13][12];
} Intermediate_Vprofile;


int main (int argc, char **argv)
{

  char *infile;
  int  verbose;
  Intermediate_Vprofile i_vprof;
  float  dbz;
  FILE *fp;
  int j, k;

  verbose = 0;
  process_args(argc, argv,
			   &infile,
			   &verbose);
  
  /* 'infile' is the intermediate file used in 'accu_vprof'
   * Output to stdout.
   */

  /* 
   * If the intermediate file contains the total accumulation of 
   * points for the monthly vertical profile followed by a count for
   * averaging.  The output is used by 3A-55ascii_to_hdf when the END
   * flag specified on the 3A-55 command (see 3A-55).
   */

  /*
   * To compute average:
   *
   *   1. Values are Z.
   *   2. Compute average on Z.
   *   3. Convert to averaged Z to dBZ.
   *
   */


  /* Binary format for intermediate vprofile of Z (doubles & counts). */
  fp = fopen(infile,"r");
  /* First time around the intermediate accumulation file may not
   * exist.  Ignore: initialize to zero.
   */
  if (fp == NULL) {
	fprintf(stderr, "Error: ");
	perror(infile);
	exit(-1);
  } else {
	fread(&i_vprof, sizeof(i_vprof), 1, fp);
	fclose(fp);
  }

  /* The input file contains Z.
   * Compute the average and convert to dBZ.
   */
  for (j = 0; j < 12; ++j) {
	for (k = 0; k < 13; ++k) {
	  if (i_vprof.count[k][j] == 0)
		dbz = -99.00;
	  else
		dbz = 10 * log10(i_vprof.z[k][j]/i_vprof.count[k][j]);
	  printf(" %8.2f", dbz);
	}
	printf("\n");
  }
  exit(0);
}
