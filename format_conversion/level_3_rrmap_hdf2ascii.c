/*
 * level_3_rrmap_hdf2ascii.c
 *      Create an ASCII map for 3A-53 or 3A-54 HDF file.
 *
 *
 *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * By: Thuy Nguyen
 *     Science Systems and Applications, Inc. (SSAI)
 *     NASA/TRMM Office
 *     nguyen@trmm.gsfc.nasa.gov
 *     August 28, 1997
 *
 ***********************************************************************/

#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <IO.h>
#include <IO_GV.h>

#include <gv_utils.h>

static char *this_prog = NULL;

#define MAX_ROWS  151
#define MAX_COLS  151

void process_argv(int argc, char **argv, product_type_t *prod, char **infile, char **outfile);
int  read_map_from_hdf_file(char *hdf_file, product_type_t product, int *map);
int write_map_to_file(int *map, char *fname);


static void handler(int sig)
{
  fprintf(stderr, "Got signal %d. Abort.\n", sig);
  if (sig == SIGINT || sig == SIGKILL || sig == SIGSTOP) {
	exit (-2);
  }
  exit(-1);
}


/************************************************************************/
/*                                                                      */
/*                                main                                  */
/*                                                                      */
/************************************************************************/
main (int argc, char **argv)
{
  char *infile = NULL, *outfile = NULL;
  int map[MAX_ROWS][MAX_COLS];
  product_type_t prod;

  set_signal_handlers();
  this_prog = argv[0];
  process_argv(argc, argv, &prod, &infile, &outfile);
  
  if (read_map_from_hdf_file(infile, prod, (int *) map) < 0) {
	fprintf(stderr, "Failed reading map from  file <%s>\n", infile);
	exit(-1);
  }
  if (write_map_to_file((int *) map, outfile) < 0) {
	fprintf(stderr, "Failed to write map to file <%s>\n", outfile);
	exit(-1);
  }
  exit(0);
  
}


/************************************************************************/
/*                                                                      */
/*                          process_argv                                */
/*                                                                      */
/************************************************************************/
void process_argv(int argc, char **argv, 
				  product_type_t *prod, char **infile, char **outfile)
{
  extern int optind;
  extern char *optarg;
  extern int optopt;
  
  int c;

  if (argc  < 3) {
 USAGE:
	fprintf(stderr, "Usage (%s): %s [-v] 3A-53|3A-54 level_3_rrmap_hdf_infile product_ascii_outfile\n", PROG_VERSION, argv[0]);
	exit(-1);
  }

  while ((c = getopt(argc, argv, ":v")) != -1) {
	switch(c) {
	case 'v':
	  /* NO VERBOSE IMPLEMENTED YET. */
	  break;
	case '?': fprintf(stderr, "option -%c is undefined\n", optopt);
	  goto USAGE;
    case ':': fprintf(stderr, "option -%c requires an argument\n",optopt);
	  goto USAGE;
    default: break;
    }
  }

  /* must have 3 things listed */
  if (argc - optind != 3) goto USAGE;
  
  if (strcmp(argv[optind], "3A-53") == 0)
	*prod = P3A53;
  else if (strcmp(argv[optind], "3A-54") == 0)
	*prod = P3A54;
  else {
	fprintf(stderr, "Only recognize 3A-53 and 3A-54\n");
	exit(-1);
  }
  optind++;
  *infile = (char *) strdup(argv[optind++]);
  *outfile = (char *) strdup(argv[optind]);

}

/************************************************************************/
/*                                                                      */
/*                          read_map_from_hdf_file                      */
/*                                                                      */
/************************************************************************/
int  read_map_from_hdf_file(char *hdf_file, product_type_t product, int *map) 
{
  /* Read map from hdf file for either 3A-53 or 3A-54 product.
   * Return 1 for successful; -1, otherwise.
   */
  IO_HANDLE hdf_fd;
  int data_type;
  void *pgrid;
  int r, c;
  int status;

  union _grid{
	L3A_53_SINGLE_RADARGRID g3A53;
	L3A_54_SINGLE_RADARGRID g3A54;
  } grid;
  
  if (hdf_file == NULL || map == NULL)
	return -1;

  switch (product) {
  case P3A53:
	pgrid = (void *) &grid.g3A53;
	memset(pgrid, '\0', sizeof(L3A_53_SINGLE_RADARGRID));
	data_type = TK_L3A_53S;
		
	break;
  case P3A54:
	pgrid = (void *) &grid.g3A54;
	memset(pgrid, '\0', sizeof(L3A_54_SINGLE_RADARGRID));
	data_type = TK_L3A_54S;
	break;
  default:
	return -1;
  }
  /* Open the hdf file for read. */
  memset(&hdf_fd, '\0', sizeof(IO_HANDLE));
  status = TKopen(hdf_file, data_type, TK_READ_ONLY, &hdf_fd);
  if (status != TK_SUCCESS) {
	fprintf(stderr, "%s:Error: Failed to open %s\n", this_prog, hdf_file);
	return -1;
  }

  /* Read grid and fill in map. Note: Valid rr values will be divided by 10
   * so that the ascii file produced here has the similar data representation
   * as the inter. acc. map file. Note that the values from two different maps
   * may be different since the values from the inter acc map have been type
   * casted to integer before storing in the HDF file.
   */
  if (TKreadGrid(&hdf_fd, pgrid) != TK_SUCCESS) {
	fprintf(stderr, "%s: Error: Failed to read grid from file.\n", this_prog);
	goto ERROR;
  }
  for (r = 0; r < MAX_ROWS; r++) {
	for (c = 0; c < MAX_COLS; c++) {
	  if (product == P3A53) {
		*(map+c+(r*MAX_COLS)) = (int) ((L3A_53_SINGLE_RADARGRID *) pgrid)->pentadRainfall[r][c];
	  }
	  else if (product == P3A54) {
		*(map+c+(r*MAX_COLS)) = (int) ((L3A_54_SINGLE_RADARGRID *) pgrid)->monthlyRainfall[r][c];
	  }
	  if (*(map+c+(r*MAX_COLS)) < 0)
		*(map+c+(r*MAX_COLS)) = -99;
	}
  }
  
  if (TKclose(&hdf_fd) != TK_SUCCESS) {
	fprintf(stderr, "%s:Error:Failed to close hdf file.\n", this_prog);
	return -1;
  }
  return 1;

ERROR:
  TKclose(&hdf_fd);
  return -1;
} /* read_map_from_hdf_file */


/************************************************************************/
/*                                                                      */
/*                          write_map_to_file                           */
/*                                                                      */
/************************************************************************/
int write_map_to_file(int *map, char *fname)
{
  /* Write map to fname in ascii format.
   * Return 1 for successful; -1, otherwise.
   */
  FILE *fp;
  int r, c;

  if (map == NULL || fname == NULL) return -1;

  if ((fp = fopen(fname, "w")) == NULL) {
	fprintf(stderr, "%s: Error: Failed to open file <%s>.\n", this_prog, fname);
	return -1;
  }
  
  for (r = 0; r < MAX_ROWS; r++) {
	for (c = 0; c < MAX_COLS; c++) 
	  fprintf(fp, "%d.000 ", *(map + c + (r * MAX_COLS)));
	fprintf(fp, "\n");
  }
  fclose(fp);
  return 1;
} /* write_map_to_file */
