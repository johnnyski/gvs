/**************************************************************************
 * ghdf_to_shdf.c -- 
 *    Explode granulized HDF file to single VOS HDF file(s)
 *    for the specified product, i.e. 2A-53, 2A-54, 2A-55.
 *    It outputs a list of single VOS HDF file(s) to STDOUT.                
 *
 *
 * By:    Thuy Nguyen
 *        NASA/TRMM Office
 *        Thuy.Nguyen@gsfc.nasa.gov
 *        June 12, 1996
 *
 ***************************************************************************/

#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

/* TSDIS toolkit include files*/
#include <IO.h>
#include <IO_GV.h>
#include <TKerrHandle.h>
#include <TS_GVFC_61.h>

#include "gvs_metadata.h"
                              
#define MAX_FILENAME 256


union _grid{
	L2A_53_SINGLE_RADARGRID g2A53;
	L2A_54_SINGLE_RADARGRID g2A54;
	L2A_55_SINGLE_RADARGRID g2A55;
  };

int                     verbose;
int create_single_vos_hdf_file(char pcode, void *grid, gv_metadata_t *metadata,
							   char *outfile);
void process_argv(int argc, char **argv, char *product_code, char *output_path,
				  char *infile, int *verbose)
{
  extern int optind;
  extern char *optarg;
  extern int optopt;
  
  int c;

  if (product_code == NULL || output_path == NULL || infile == NULL ||
	  verbose == NULL) {
	TKreportWarning(W_GVFC_G2S_BAD_PARAMS);
	exit(-1);
  }
  if (argc  < 2) {
 USAGE:
	fprintf(stderr, "Usage (%s): %s [-v] [-d output_path] {3 | 4 | 5} ghdf_file\n", PROG_VERSION, argv[0]);
	fprintf(stderr, " where:\n");
	fprintf(stderr, "\t3 = 2A-53; 4 = 2A=54; 5 = 2A-55\n");
	TKreportWarning(W_GVFC_G2S_BAD_USAGE);
	exit(-1);
  }

  while ((c = getopt(argc, argv, ":vd:")) != -1) {
	switch(c) {
	case 'v':
	  *verbose = 1;  break;
	case 'd':
	  if (optarg[0] == '-') goto USAGE;
	  strcpy(output_path, optarg); break;
	case '?': fprintf(stderr, "option -%c is undefined\n", optopt);
	  goto USAGE;
    case ':': fprintf(stderr, "option -%c requires an argument\n",optopt);
	  goto USAGE;
    default: break;
    }
  }

  /* must have 2 items listed */
  if (argc - optind != 2) goto USAGE;
  *product_code =  argv[optind++][0];
  strcpy(infile, argv[optind]);

}

int degranulize_hdf(char pcode, char *output_path,  char *infile)
{
  /* explode granulized hdf file to single VOS hdf file(s) for product 
   * specified by pcode.  Single VOS HDF file(s) will be located in ./<infile>.
   */
  int i;
  int status;
  IO_HANDLE ghdf_fd;
  void *pgrid;
  union _grid grid;
  char outfile[(2*MAX_FILENAME)+10], tmp_file[MAX_FILENAME], *tmp_str, 
      *tmp_str1;

  gv_metadata_t metadata;

  if (infile == NULL || output_path == NULL) return -1;

  memset(&ghdf_fd, '\0', sizeof(IO_HANDLE));

  /* open HDF file */
  switch (pcode) {
	case '3':  /* 2A-53 */
	
	/* open hdf file */
	status = TKopen(infile, TK_L2A_53S, TK_READ_ONLY, &ghdf_fd);
	pgrid = (void *) &grid.g2A53;
	memset(pgrid, '\0', sizeof(L2A_53_SINGLE_RADARGRID));
	break;

	case '4':  /* 2A-54 */
	
	/* open hdf file */
	status = TKopen(infile, TK_L2A_54S, TK_READ_ONLY, &ghdf_fd);
	pgrid = (void *) &grid.g2A54;
	memset(pgrid, '\0', sizeof(L2A_54_SINGLE_RADARGRID));
	break;
	case '5':  /* 2A-55 */
	
	/* open hdf file */
	status = TKopen(infile, TK_L2A_55S, TK_READ_ONLY, &ghdf_fd);
	pgrid = (void *) &grid.g2A55;
	memset(pgrid, '\0', sizeof(L2A_55_SINGLE_RADARGRID));
	break;

  default:
	fprintf(stderr, "product code <%c> is unknown\n", pcode);
	break;
  }

  /* Check the Error Status */
  if (status != TK_SUCCESS) {
	fprintf(stderr, "TKopen Failef for <%s>\n", infile);
	TKreportWarning(W_GVFC_G2S_OPEN_INFILE_FAILED);
	return (-1);
  }
  /* remove path */
  if (strrchr(infile, '/')) {
	tmp_str = infile;
	tmp_str = strrchr(tmp_str, '/')+1;
	strcpy(tmp_file, tmp_str);
  }
  else
	strcpy(tmp_file, infile);
  /* remove extension */
  if (strrchr(tmp_file, '.') != NULL) {
	/* outfile: "output_path/<infile (no extension)>.<hhmm>.hdf" */
	*(strrchr(tmp_file, '.')) = '\0';
  }

  tmp_str1 = tmp_file;
  /* remove path in input filename */
  if (strrchr(tmp_file, '/') != NULL) 
	tmp_str1 = strrchr(tmp_file, '/');


  memset(&metadata, '\0', sizeof(gv_metadata_t));
  /* Read metadata from the input file */
  if (read_hdf_metadata_from_l2granule(&ghdf_fd, &metadata, verbose) < 0) {
	if (verbose) fprintf(stderr, "Failed to read metadata from the input file\n");
	return -1;
  }

  i = 0;
  /* read it and write to single VOS hdf file(s) */
  while ( TKendOfFile ( &ghdf_fd ) != TK_EOF ) {
       
	/* read grid from file */
	if (TKreadGrid(&ghdf_fd, pgrid) != TK_SUCCESS) {
	  fprintf(stderr, "TKreadGrid failed for <%s>\n", infile);
	  TKreportWarning(W_GVFC_G2S_READ_INFILE_FAILED);
	  return (-1);
	}	
	i++;
	/* type cast pgrid to 'L2A_55_SINGLE_RADARGRID *' should work for 
     * 2A-53, 2A-54, and 2A-55 since tktime is within the first two common
	 * fields.
	 */
	sprintf(outfile, "%s/%s.%2.2d%2.2d.hdf", output_path, tmp_str1, 
			((L2A_55_SINGLE_RADARGRID *)pgrid)->tktime.tkhour, 
			((L2A_55_SINGLE_RADARGRID *)pgrid)->tktime.tkminute);

	if (verbose) {
	  fprintf(stderr, "hour:min = %2.2d%2.2d\n", ((L2A_55_SINGLE_RADARGRID *)pgrid)->tktime.tkhour, 
			((L2A_55_SINGLE_RADARGRID *)pgrid)->tktime.tkminute);
	  fprintf(stderr, "creating single vos hdf <%s>\n", outfile);
	}

	if (create_single_vos_hdf_file(pcode, pgrid, &metadata, outfile) < 0) {
	  fprintf(stderr, "Failed creating svos hdf file %d\n", i);
	  TKreportWarning(W_GVFC_G2S_CREATE_SVOS_HDF_FAILED);
	  TKclose(&ghdf_fd);
	  return -1;
	}
	fprintf(stdout, "%s\n",outfile);  
  }

  /* close file */
  if (TKclose(&ghdf_fd) == TK_FAIL) {
	fprintf(stderr, "Can't close <%s>\n", infile);
	TKreportWarning(W_GVFC_G2S_CLOSE_INFILE_FAILED);
	return(-1);  
  }
  return 1;
}

int create_single_vos_hdf_file(char pcode, void *grid, gv_metadata_t *metadata,
							   char *outfile)
{
  IO_HANDLE hdf_fd;
  int status;

  if (grid == NULL || outfile == NULL || metadata == NULL) return -1;

  memset(&hdf_fd, '\0', sizeof(IO_HANDLE));

  /* open HDF file */
  switch (pcode) {
	case '3':  /* 2A-53 */
	
	/* open hdf file */
	status = TKopen(outfile, TK_L2A_53S, TK_NEW_FILE, &hdf_fd);
	break;

	case '4':  /* 2A-54 */
	status = TKopen(outfile, TK_L2A_54S, TK_NEW_FILE, &hdf_fd);
	break;
	case '5':  /* 2A-55 */
	status = TKopen(outfile, TK_L2A_55S, TK_NEW_FILE, &hdf_fd);

	break;

  default:
	fprintf(stderr, "product code <%c> is unknown\n", pcode);
	break;
  }

  /* Check the Error Status */
  if (status != TK_SUCCESS) {
	fprintf(stderr, "TKopen failed for <%s>\n", outfile);
	TKreportWarning(W_GVFC_G2S_CLOSE_OUTFILE_FAILED);
	return (-1);
  }


  /* read grid from file */
  if (TKwriteGrid(&hdf_fd, grid) != TK_SUCCESS) {
	fprintf(stderr, "TKwriteGrid failed for <%s>\n", outfile);
	TKreportWarning(W_GVFC_G2S_WRITE_OUTFILE_FAILED);
	return (-1);
  }	


  /* Reset num of vos */
  if (metadata)
	metadata->num_vos = 1;
  /* Write metadata */
  if (write_hdf_metadata_for_l2granule(&hdf_fd, metadata, verbose) < 0) {
	if (verbose) fprintf(stderr, "Failed to write metadata to %s.\n", outfile);
	TKclose(&hdf_fd);
	return -1;
  }

  /* close file */
  if (TKclose(&hdf_fd) == TK_FAIL) {
	fprintf(stderr, "Can't close <%s>\n", outfile);
	TKreportWarning(W_GVFC_G2S_CLOSE_OUTFILE_FAILED);
	return(-1);  
  }
  return 1;
} 
void handler(int sig)
{
  fprintf(stderr, "Got signal %d. Abort.\n", sig);
  if (sig == SIGINT || sig == SIGKILL || sig == SIGSTOP) {
	 TKreportWarning(W_GVFC_G2S_ABORT_EXEC);
	exit (-2);
  }
  TKreportWarning(W_GVFC_G2S_STOP_ON_ERROR);
  exit(-1);
}

int main(int argc, char **argv)
{
  char                    infile[MAX_FILENAME];
  char                    output_path[MAX_FILENAME];
  char                    product_code;

  signal(SIGINT, handler);
  signal(SIGSTOP, handler);
  signal(SIGFPE, handler);
  signal(SIGKILL, handler);
  signal(SIGILL, handler);
  signal(SIGSEGV, handler);

  verbose = 0;
  strcpy(output_path, ".");
  process_argv(argc, argv, &product_code, output_path, infile, &verbose);

  if (verbose) {
	fprintf(stderr, "infile = <%s>\n", infile);
	fprintf(stderr, "output_path = <%s>\n", output_path);
	fprintf(stderr, "product_code = <%c>\n", product_code);
  }

  
  if (degranulize_hdf(product_code, output_path, infile) < 0) {
	fprintf(stderr, "Failed to degranulize\n");
	TKreportWarning(W_GVFC_G2S_DEGRANULIZE_FAILED);
	exit(-1);
  }

  if (verbose)
	fprintf(stderr, "Degranulized was successful.\n");
  exit (0);
	
} /* main */





