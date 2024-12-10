
/******************************************************************
  "Explodes" a multi-VOS HDF file into a sequence of UF files; ie, 

	For each VOS contained in the HDF file granule, do the following:
     1. Read VOS from HDF file into a tsdis L1B_1C_GV structure.
		 2. Move VOS from tsdis L1B_1C_GV structure into RSL radar struct.
		 3. Move VOS from RSL radar structure to a UF file.

	USAGE:
		    hdf_to_uf [-v] [-u UF_output_dir] [-n names_file] HDFfile

	Each resultant UF file is named using the date/time of the VOS
	which it contains. In addition, the UF filenames (separated by a 
    newline character) are listed in names_file.


  -----------------------------------------------------------------
	 Libraries required for execution of this code :
      -ltsdistk                    : tsdis toolkit
      -lmfhdf -ldf -ljpeg -lz      : HDF
      -lrsl                        : rsl
      -lm                          : C math

  -----------------------------------------------------------------
		mike.kolander@trmm.gsfc.nasa.gov
		(301) 286-1540
*******************************************************************/

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>

#include <TKerrHandle.h>
#include <TS_GVFC_61.h>
#ifdef __sgi
  #include <getopt.h>
#endif

/* RSL function and structure definitions. */
#include "rsl.h"

extern Radar *RSL_hdf_to_radar(char *infile);
extern int getopt(int argc, char * const argv[], const char *optstring);
extern void doexit_handler(int sig);

char *thisprog = "";

/*************************************************************/
/*                                                           */
/*                         main                              */
/*                                                           */
/*************************************************************/
int main(int argc, char *argv[])
{
/* 
	 Checks content and validity of the command line which invoked this
	 function. If valid, calls RSL functions to do the required 
	 level_1 processing.

	 Returns: 0 if successful.
           -1 if failed.
		       -2 interrupted.
*/
	extern char *optarg;
	extern int optopt;
	extern int optind;
	extern int optopt;
	int c, year;
	int prod_version_num = 1;
	int verbose=0;
	Radar *radar;
	char site[8], uffile[256];
	char output_dir[256], outfile[256], *tmp_outfile;
	FILE *outfile_fp = stdout;

	if (argc < 2)
	{
	USAGE:
		fprintf(stderr, "\nHDF_to_UF file format converter... USAGE (%s):\n\n", PROG_VERSION);
		fprintf(stderr, "%s [-v] [-u UF_output_dir] [-n names_file] HDFfile\n", argv[0]);
		fprintf(stderr, "\n  Options:\n");
		fprintf(stderr, "\t-v              : verbose\n");
		fprintf(stderr, "\t-u UF_output_dir: Default: write UF files into ./\n");
		fprintf(stderr, "\t-n names_file   : Default: write names of UF files created to stdout.\n\n");
		TKreportWarning(W_GVFC_H2U_BAD_USAGE);
		exit(-1);
	}

	thisprog = argv[0];
	signal(SIGINT, doexit_handler);
	signal(SIGSTOP, doexit_handler);
	signal(SIGKILL, doexit_handler);
	signal(SIGFPE, doexit_handler);
	signal(SIGILL, doexit_handler);
	signal(SIGSEGV, doexit_handler);

	RSL_radar_verbose_off(); 
	strcpy(output_dir, ".");
	strcpy(outfile, "");

	/* Read options from command line. */
	while ((c=getopt(argc, argv, "vu:n:")) != EOF)
	{
		switch (c)
		{
		case 'v':
			verbose = 1;
			RSL_radar_verbose_on();
			break;
		case 'u':
			strcpy(output_dir, optarg);
			break;
		case 'n':
		  strcpy(outfile, optarg);
		  break;
		case ':':
			fprintf(stderr, "option -%c requires an argument\n",optopt);
			goto USAGE;
		default:
			fprintf(stderr, "Option <%c> is not valid\n", c);
			goto USAGE;
		}     /* end switch */
	}  /* end while */

	/* Check for one remaining item in the command line. */
	if ((argc - optind) != 1) 
	  goto USAGE;

	if (strlen(outfile) != 0) {
	  /* Open outfile for writing the UF filenames into it if it was specified. */
	  if ((outfile_fp = fopen(outfile, "w")) == NULL) {
		perror("fopen outfile:");
		 TKreportWarning(W_GVFC_H2U_OPEN_OUTFILE_FAILED);
		exit(-1);
	  }		
	}  
	/* Pick the 4-character site designator and prod version # 
	 * out of the HDF filename. */
	if (strrchr(argv[optind], '/'))
	  tmp_outfile = strrchr(argv[optind], '/') + 1;
	else tmp_outfile = argv[optind];
	sscanf(tmp_outfile, "%*4s.%*6s.%*d.%4s.%d.", site, &prod_version_num);
	
	/* For each VOS contained in the HDF file, do the following:
     1. Read VOS from the HDF file into a RSL radar structure.
		 2. Move VOS from the RSL radar structure to a UF file.
		    The UF file is named using the site/date/time of the VOS.
	*/
	while (1)
	{
		radar = (Radar *)RSL_hdf_to_radar(argv[optind]);
		if (radar == NULL) break;

		/* Create a name for the UF file. */
		if (radar->h.year >= 2000) year = radar->h.year - 2000;
		else year = radar->h.year - 1900;
		sprintf(uffile, "%s/%.2d%.2d%.2d.%d.%s.%d.%.2d%.2d.uf.gz", 
						output_dir, year, radar->h.month, 
						radar->h.day, radar->h.hour+1, site, prod_version_num,
						radar->h.hour, radar->h.minute);
		if  (verbose)
		  fprintf(stderr, "\n******* Writing UF file %s ...\n", uffile);
		/* Move VOS from the radar structure to the compressed UF file. */
		if (verbose) RSL_radar_verbose_off();  /* Gag 'radar_to_uf' */
		RSL_radar_to_uf_gzip(radar, uffile);
		if (verbose) RSL_radar_verbose_on();
		fprintf(outfile_fp, "%s\n", uffile);
		RSL_free_radar(radar);	
	} /* end while (1) */

	if (outfile_fp != stdout)
	  fclose(outfile_fp);

	if (verbose) 
	  fprintf(stderr, "%s: Successful.\n", argv[0]);
	exit(0);
}

void clean_up()
{

}

void doexit_handler(int sig)
{
/*
  pid_t grpid;
  int i;
*/
  /* Restore STDERR and STDOUT.  This is required since both stderr and stdout
   * were redirected in the calling program, all_tape_level_1n2.
   */
  close(1);
  close(2);
  fopen("/dev/tty", "w");
  fopen("/dev/tty", "w");
  clean_up();

  fprintf(stderr, "%s: Got signal <%d>. Bye.\n", thisprog, sig);
  fflush(0);
  if (sig == SIGINT || sig == SIGKILL || sig == SIGSTOP) {
	TKreportWarning(W_GVFC_H2U_ABORT_EXEC);
	exit(-2);
  }
  TKreportWarning(W_GVFC_H2U_STOP_ON_ERROR);
  exit(-1);
}
