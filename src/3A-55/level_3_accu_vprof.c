/*
 * Accumulate 2A-55 vertical profile tables in preparation for 3A-55 generation.
 *
 * By John H. Merritt
 *    Space Applications Corporation
 *    Vienna VA
 *    10/6/97
 *
 */

/*
 * Usage:
 *
 *    accu_vprof [-v] infile inter_acc_vprof_file
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
  fprintf(stderr, "   %s  [-v] infile inter_acc_vprof_file\n", argv[0]);
  fprintf(stderr, " * This program can be run stand-alone, but, it is typcially\n");
  fprintf(stderr, "invoked from the 3A-55 script via the BEGIN or MIDDLE flag.\n");
  fprintf(stderr, "See 3A-55 for more information.\n");
  return;
}

void process_args(int argc, char **argv,
				  char **infile,
				  char **inter_acc_vprof_file,
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

  *infile               = strdup(argv[optind]);
  *inter_acc_vprof_file  = strdup(argv[optind+1]);

}

typedef double Vprofile[13][12];

typedef struct {
  Vprofile z;
  int      count[13][12];
} Intermediate_Vprofile;


int main (int argc, char **argv)
{

  char *inter_acc_vprof_file;
  char *infile;
  int  verbose;
  Intermediate_Vprofile i_vprof;
  float  dbz;
  FILE *fp;
  int j, k;

  verbose = 0;
  process_args(argc, argv,
			   &infile,
			   &inter_acc_vprof_file,
			   &verbose);
  
  /* 'infile' is where we get information for 'inter_vos_times_file'
   */

  /* 
   * If the inter_acc_vprof_file contains the total accumulation of 
   * points for the monthly vertical profile followed by a count for
   * averaging by 3A-55ascii_to_hdf when the END flag specified on the
   * 3A-55 command (see 3A-55).
   */

  /*
   * To compute average:
   *
   *   1. Convert dBZ to Z.
   *   2. Save accumulated Z in intermediate file w/ count (n).
   *   3. Compute average on Z.
   *   4. Convert to averaged Z to dBZ.
   *
   * Only steps 1 and 2 are done by this program.
   * Steps 3 and 4 are handled by '3A-55_calculate_mean_vprofile'.
   */


  /* Binary format for intermediate vprofile of Z (doubles & counts). */
  fp = fopen(inter_acc_vprof_file,"r");
  /* First time around the intermediate accumulation file may not
   * exist.  Ignore: initialize to zero.
   */
  if (fp == NULL) {
	memset(&i_vprof, 0, sizeof(i_vprof));
	if (verbose) {
	  fprintf(stderr, "Warning: ");
	  perror(inter_acc_vprof_file);
	}
  } else {
	fread(&i_vprof, sizeof(i_vprof), 1, fp);
	fclose(fp);
  }

  /* The input file 'infile' contains dBZ values which must be
   * converted to Z before they are added to 'i_vprof'.  Then,
   * output the new 'i_vprof'.
   */
  /* Read Vertical Profile: convert and merge w/ intermediate data. */
  fp = fopen(infile,"r");
  for (j = 0; j < 12; ++j) {
	for (k = 0; k < 13; ++k) {
	  fscanf(fp, " %f", &dbz);
	  if (dbz > -99.00) {
		i_vprof.z[k][j] += pow((double)10.0, (double)(dbz/10.0));
		i_vprof.count[k][j]++;
	  }
	}
	fscanf(fp,"\n");
  }
  fclose(fp);

  /* Output: Binary format for intermediate vprofile of Z. */
  fp = fopen(inter_acc_vprof_file,"w");
  fwrite(&i_vprof, sizeof(i_vprof), 1, fp);
  fclose(fp);

  exit(0);
}
