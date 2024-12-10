/* 
 * 2A-54_hdf2ascii.c -- Reads 2A-54 product, convective/stratiform map,
 *        from HDF file and writes it to ascii file.
 * 
 *
 * By:    Thuy Nguyen
 *        NASA/TRMM Office
 *        Thuy.Nguyen@gsfc.nasa.gov
 *        May 14, 1996
 */

#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

/* TSDIS toolkit include files*/
#include <IO.h>
#include <IO_GV.h>
#include <TKerrHandle.h>
#include <TS_GVFC_61.h>
                              
#define MAX_FILENAME 256

/* hdf 's grid */
#define MAX_NROWS 151
#define MAX_NCOLS 151
void process_argv(int argc, char **argv, char *infile, char *outfile,
				  int *verbose)
{
  extern int optind;
  extern char *optarg;
  extern int optopt;
  
  int c;

  if (argc  < 2) {
 USAGE:
	fprintf(stderr, "Usage (%s): %s [-v] 2A-54_product_hdf 2A-54_product_ascii\n", PROG_VERSION, argv[0]);
	TKreportWarning(W_GVFC_2A54_H2A_BAD_USAGE);
	exit(-1);
  }

  while ((c = getopt(argc, argv, ":v")) != -1) {
	switch(c) {
	case 'v':
	  *verbose = 1;
	  break;
	case '?': fprintf(stderr, "option -%c is undefined\n", optopt);
	  goto USAGE;
    case ':': fprintf(stderr, "option -%c requires an argument\n",optopt);
	  goto USAGE;
    default: break;
    }
  }

  /* must have 2 files listed */
  if (argc - optind != 2) goto USAGE;
  
  strcpy(infile, argv[optind++]);
  strcpy(outfile, argv[optind]);

}


int read_data_from_hdf_file(char *hdffile, L2A_54_SINGLE_RADARGRID *grid)
{
  int status;
  IO_HANDLE               hdf_fd;

  if (grid == NULL || hdffile == NULL) return -1;

  memset(&hdf_fd, '\0', sizeof(IO_HANDLE));
  /* open hdf file */
  status = TKopen(hdffile, TK_L2A_54S, TK_READ_ONLY, &hdf_fd);
  
  /* Check the Error Status */
  if (status != TK_SUCCESS) {
	fprintf(stderr, "TKopen failed on file <%s>\n", hdffile);
	return (-1);
  }

  /* read grid from file */
  if (TKreadGrid(&hdf_fd, grid) != TK_SUCCESS) {
	fprintf(stderr, "TKreadGrid failed from file <%s>.\n", hdffile);
	return (-1);
  }	

  /* close file */
  if (TKclose(&hdf_fd) == TK_FAIL) {
	fprintf(stderr, "Can't close <%s>\n", hdffile);
	return(-1);  
  }
  return 1;
}

int write_grid_to_ascii_file(L2A_54_SINGLE_RADARGRID *grid, char *outfile)
{
  int r, c;
  FILE *fp;

  if (grid == NULL || outfile == NULL) return -1;

  /* open file */
  if ((fp = fopen(outfile, "w")) == NULL) {
	fprintf(stderr, "Can't open <%s> for writing\n", outfile);
	return -1;
  }
  
  /* write grid to file */
  for (r = 0; r < MAX_NROWS; r++) {
	for (c = 0; c < MAX_NCOLS; c++) {
	  fprintf(fp, "%d ", grid->convStratFlag[r][c]);
	}
	fprintf(fp, "\n");
  }

  /* close file */
  fclose(fp);

  return 1;
} /* write_grid_to_ascii_file */

void handler(int sig)
{
  fprintf(stderr, "Got signal %d. Abort.\n", sig);
  if (sig == SIGINT || sig == SIGKILL || sig == SIGSTOP) {
	TKreportWarning(W_GVFC_2A54_H2A_ABORT_EXEC);
	exit(-2);
  }
  TKreportWarning(W_GVFC_2A54_H2A_STOP_ON_ERROR);
  exit(-1);
}

int main(int argc, char **argv)
{
  L2A_54_SINGLE_RADARGRID l2A54Grid;
  char                    infile[MAX_FILENAME],
                          outfile[MAX_FILENAME];
  int                     verbose = 0;

  signal(SIGINT, handler);
  signal(SIGSTOP, handler);
  signal(SIGFPE, handler);
  signal(SIGKILL, handler);
  signal(SIGILL, handler);
  signal(SIGSEGV, handler);

  process_argv(argc, argv, infile, outfile, &verbose);

  if (verbose) {
	fprintf(stderr, "infile = <%s>\n", infile);
	fprintf(stderr, "outfile = <%s>\n", outfile);
  }

  memset(&l2A54Grid, '\0', sizeof(L2A_54_SINGLE_RADARGRID));
  if (read_data_from_hdf_file(infile, &l2A54Grid) < 0) {
	fprintf(stderr, "Failed to read data from file <%s>\n", infile);
	TKreportWarning(W_GVFC_2A54_H2A_READ_INFILE_FAILED);
	exit(-1);
  }

  if (verbose) {
	fprintf(stderr, "Writing data to file <%s>\n", outfile);
  }
  if (write_grid_to_ascii_file(&l2A54Grid, outfile) < 0) {
	TKreportWarning(W_GVFC_2A54_H2A_WRITE_OUTFILE_FAILED);
	exit(-1);
  }
  
  exit (0);
	
} /* main */





