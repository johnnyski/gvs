#include <stdio.h>
/*
 * Take a granule HDF level 2 file and dump the metadata (binary form)
 * to stdout.
 *
 * By: John Merritt
 *     Space Applications Corporation
 *     Vienna, VA
 *     John.H.Merritt@gsfc.nasa.gov
 */

/* TSDIS toolkit include files*/
/* setenv TSDISTK /usr/local/toolkit */
#include "IO_GV.h"
#include "gvs_metadata.h"
#ifdef __sgi
  #include <getopt.h>
#endif
#include <stdio.h>

#include <stdlib.h>
#include <string.h>


void usage(char **argv)
{
  fprintf(stderr,"Usage (%s)\n", PROG_VERSION);
  fprintf(stderr, " %s infile\n",argv[0]);
}

void process_args(int argc, char **argv, char **infile)
{
  extern char *optarg;
  extern int optind, opterr, optopt;

  int c;
  
  while ((c = getopt(argc, argv, "")) != -1)
	switch (c) {
	case '?': usage(argv); exit(-1); break;
	default:  break;
	}

/* must have 1 files listed */
  if (argc - optind != 1) {
	usage(argv);
	exit(-1);
  }

/* Can use strdup, if desired */
/* strdup allocates memory */
  *infile = strdup(argv[optind]);
}


int main(int argc, char **argv)
{
  IO_HANDLE     hdf_fd;
  gv_metadata_t metadata;
  char          *infile;
  int           dataType;
  char          filemode;
  int           status;

  process_args(argc, argv, &infile);

  dataType = TK_L2A_55S;
  filemode = TK_READ_ONLY;

  status = TKopen(infile, dataType, filemode, &hdf_fd);
  
  if (status != TK_SUCCESS) {
	fprintf(stderr, "TKopen failed for file <<%s>>.\n", infile);
	exit(-1);
  }
  memset(&metadata, '\0', sizeof(gv_metadata_t));
  if (read_l3hdf_metadata_from_l2granule(&hdf_fd, &metadata, 0) < 0) 
	fprintf(stderr, "read error for metadata of l2 granule\n");
  fwrite(&metadata, sizeof(metadata), 1, stdout);


  if (TKclose(&hdf_fd) == TK_FAIL) {
	fprintf(stderr, "TKclose failed.  HDF file is <<%s>>.\n", infile);
	exit(-1);
  }

  exit(0);
} 


  
