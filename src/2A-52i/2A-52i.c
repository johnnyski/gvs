/***************************************************************************

frac_area.c


Compute rain percent for each rain rate map (151x151) resided in
an hour-granule HDF file (2A-53 product in HDF).

   rain percent = 100 * (total number of rain pixels / total number of pixels)
    where
   total number of pixels = # of pixels within the boundary.
   total number of rain pixels = # of pixels having rain rate > 0.0.

The product file (in ASCII) contains the existence data of one site for 
one month. Each line (excluding the header lines) contains existence data 
for one vos.
 i.e.,

This program writes the rain % to TSDIS' database (if it's running in TSDIS'
environment).

Date of        Time of                Distance of Closest  Date of  Time of
  VOS            VOS     % Rain  Hit     Approach (CA)       CA       CA
--------------------------------------------------------------------------------
1992-10-02  12:05:40.000    46    0        -9999.900        NULL     NULL
1992-10-02  12:11:32.000    47    1        86.974        1992-10-02 12:11:18.697


******************************************************************************


 Note:    Since satellite coincidence file contains coincidence data for
          one day, 2 satellite coincidence files (file of the previous day
          will be used if the vos_time is close to the beginning of the day;
          file of the next day will be used if the vos_time is close to the
          end of the day) may be required to do lookup for VOS having 
          begin/end times (+/- 30 mins) occur in two different days.
          
          The second sat. coin. file will be checked if no hit has been found
          in the first file (Note: the first searched file may either be
          the file from the previous day or from the specified day).
   
          Satellite coincidence filename: CT.yymmdd.


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By: Thuy Nguyen
    NASA/TRMM Office
    nguyen@trmm.gsfc.nasa.gov
    April 24, 1995

******************************************************************************
******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <signal.h>
#ifdef __sgi
  #include <getopt.h>
#endif
#include <TS_2A52_60.h>
#include <TKerrHandle.h>
#include <IO.h>
#include <IO_GV.h>
#include <gvs_sat_coin.h>
#include <gv_utils.h>

/***************************** Definitions & global vars.*********************/

#ifdef TRUE
#undef TRUE
#endif
#define TRUE        1
#define FALSE       0

/* 2A53 product */
#define HDF_MAX_ROWS 151
#define HDF_MAX_COLS 151
#define MAX_STR_LEN  20
#define MAX_FILENAME_LEN 256
#define MAX_NAME_LEN 51
#define MAX_CMD_LEN 300

#define VOS_MINS      5    /* Assumption: Time duration of a vos in minutes */


/* Vos is in the begin, middle, or end of file */
typedef enum {
  BEGIN, END, MIDDLE
} period_type_t;

typedef struct {
  DATE_STR vos_date;
  TIME_STR vos_time;
  int      rain_percent;
  int      hit;
  float    ca_distance;
  char     ca_date_str[MAX_STR_LEN];
  char     ca_time_str[MAX_STR_LEN];
} vos_info_t;

char *thisprog;
char *outfile;
int verbose;

/*********************************** Prototype********************************/
extern char *tempnam(const char *dir, const char *pfx);
extern int getopt(int argc, char * const argv[],
                  const char *optstring);
extern int strcasecmp(const char *s1, const char *s2);


int process_closest_vos(DATE_STR *granule_sdate, TIME_STR *granule_stime, 
						DATE_STR *granule_edate, TIME_STR *granule_etime, 
						int site_id, char *sat_coinc_dir,
						vos_info_t *vos_info_list, period_type_t period_type,
						char *outfile);
int write_vos_info_list_to_outfile(period_type_t period_type, 
									vos_info_t *vos_info_list, int site_id,
									char *outfile);
int set_vos_info(L2A_53_SINGLE_RADARGRID *vos_hdf_grid, DATE_STR *date,
				 vos_info_t *vos_info);
extern float compute_frac_area(L2A_53_SINGLE_RADARGRID *l2A53);
int get_info_from_infile(IO_HANDLE *hdf_fd, 
						 DATE_STR *granule_sdate, TIME_STR *granule_stime, 
						 DATE_STR *granule_edate, TIME_STR *granule_etime, 
						 int *site_id, int *num_vos);
int process_product(char *infile, char *sat_coinc_dir, int no_sat_lookup,
					period_type_t period_type, int sort_product, char *outfile);
/*********************************** Routines ********************************/

void usage(char **argv)
{
	fprintf(stderr, "Usage (%s):\n", PROG_VERSION);
	fprintf(stderr, "   %s ", argv[0]);
	fprintf(stderr, "[-v] [-O] [-S] [-a [-s satellite_coinc_dir] ] \n"
                    "\trrmap_hdf_file re_ascii_file BEGIN/MIDDLE/END\n"
			"\n\twhere,\n"
			"\t  -v: Show execution messages.\n"
			"\t  -O: Order the product file by VOS' date and time and remove duplicates \n"
            "\t      before appending. Do nothing.\n"
			"\t  -S: Process with no satellite coincident data lookup.\n"
			"\t  -a: Use ASCII satellite coincident file. Specify this for non-TSDIS\n"
			"\t       environment.  Default: Use TSDIS' database.\n"
			"\t  -s: Specify directory that contains satellite coincidence files \n"
            "\t      (CT.yymmdd). Default:  \n"
			"\t      $GVS_DATA_PATH/sat_coinc_data or /usr/local/trmm/data/sat_coinc_data\n"
			"\t      if $GVS_DATA_PATH is not set.\n"
			"\t  BEGIN/MIDDLE/END: Flag is used to determine whether to create the\n"
			"\t      output file (BEGIN) or to append the data into the existing\n"
			"\t      output file (MIDDLE/END).  Note: File will be created even \n"
            "\t      when there is no VOS entry.\n"
			"\n");

	TKreportWarning(W_2A52_BAD_USAGE);
	exit (-1);
}

static void handler(int sig)
{
  /* Restore STDERR and STDOUT.  This is required since both stderr and stdout
   * were redirected in the calling program, all_tape_level_1n2.
   */
  close(1);
  close(2);
  fopen("/dev/tty", "w");
  fopen("/dev/tty", "w");
  kill(0, sig);
  if (verbose)
	fprintf(stderr, "%s: Got signal %d. Abort.\n", thisprog, sig);
  if (sig == SIGINT || sig == SIGKILL || sig == SIGSTOP) {
	TKreportWarning(W_2A52_ABORT_EXEC);
	exit (-2);
  }
  if (verbose)
	fprintf(stderr, "%s: Failed producing %s.\n", thisprog, outfile);
  TKreportWarning(W_2A52_STOP_ON_ERROR);
  exit(-1);
}

/****************************************************************************/
/*                                                                          */
/*                                       process_args                       */
/*                                                                          */
/****************************************************************************/

void process_args(int argc, char **argv, 
				  char **infile, char **outfile, 
				  char **sat_coinc_dir, int *use_ascii_sat_coinc_file,
				  period_type_t *period_type, int *no_sat_lookup, int *sort_product)
				  
{
  extern int optind;
  extern char *optarg;
  extern int optopt;
  int c;
  struct stat fs;

  while ((c = getopt(argc, argv, "s:avSO")) != -1) {
	switch (c) {
	case 'v': verbose = TRUE; break;
	case 'O': *sort_product = TRUE; break;
	case 'S': *no_sat_lookup = TRUE; break;
	case 'a': *use_ascii_sat_coinc_file =1; break;
	case 's': if (optarg[0] == '-') usage(argv);
	  *sat_coinc_dir = strdup(optarg); break;
    case '?': fprintf(stderr, "option -%c is undefined\n", optopt);
	  usage(argv);
    case ':': fprintf(stderr, "option -%c requires an argument\n",optopt);
	  usage(argv);
    default: break;
    }
  }

  /* must have 2 file listed and BEGIN/MIDDLE/END flag. */
  if (argc - optind < 3) usage(argv);

  *infile = strdup(argv[optind++]);
  *outfile = strdup(argv[optind++]);
  if (strcasecmp(argv[optind], "BEGIN") == 0) *period_type = BEGIN;
  else if (strcasecmp(argv[optind], "MIDDLE") == 0) *period_type = MIDDLE;
  else if (strcasecmp(argv[optind], "END") == 0) *period_type = END;
  else {
	if (verbose)
	  fprintf(stderr, "Unrecognized flag <%s>\n", argv[optind]);
	TKreportWarning(W_2A52_BAD_USAGE);
	exit (-1);
  }

  /* Check if BEGIN/MIDDLE/END flag is appropriate 
   * Error if BEGIN and outfile exists or
   *       if MIDDLE/END and outfile doesnot exist.
   */
  switch((int) *period_type) {
  case BEGIN: 
	/* If outfile exist */
	if (stat(*outfile, &fs) == 0) {
	  if (verbose)
		fprintf(stderr, "Error:re_ascii_file <%s> exist. Expect MIDDLE or END flag.\n", *outfile);
	  TKreportWarning(W_2A52_BAD_USAGE);
	  exit(-1);
	}
	break;
  case MIDDLE: 
  case END:
	/* If outfile doesnot exist */
	if (stat(*outfile, &fs) == -1) {
	  if (verbose)
		fprintf(stderr, "Error: re_ascii_file <%s> doesnot exist. Should use BEGIN flag.\n", *outfile);
	  TKreportWarning(W_2A52_BAD_USAGE);
	  exit(-1);
	}
	break;
  default:
	break;
  }

}

/****************************************************************************/
/*                                                                          */
/*                                      main                                */
/*                                                                          */
/****************************************************************************/


void main(int argc, char **argv)
{
  char   *infile = NULL, *outfile = NULL;
  char   *sat_coinc_dir = NULL;
  int    use_ascii_sat_coinc_file = 0; /* Default, running in the TSDIS env. */
  period_type_t period_type;
  char   tmp_fname[MAX_FILENAME_LEN];
  char   * path;
  int    no_sat_lookup = 0;     /* Default: lookup satellite coinc. data. */
  int    sort_product = 0;      /* Do not sort the product and remove dups
								 * before appending.*/

  thisprog = argv[0];

  /* Set signals' handler */
  signal(SIGINT, handler);
  signal(SIGFPE, handler);
  signal(SIGKILL, handler);
  signal(SIGSTOP, handler);
  signal(SIGILL, handler);
  signal(SIGSEGV, handler);

  /* set default values */
  verbose = FALSE;

  /* Process args */
  process_args(argc, argv, &infile, &outfile, 
			   &sat_coinc_dir, &use_ascii_sat_coinc_file,
			   &period_type, &no_sat_lookup, &sort_product);

  if (verbose) {
	fprintf(stderr, "infile = %s\n", infile);
	fprintf(stderr, "outfile = %s\n", outfile);
  }

  if (no_sat_lookup) {
	if (verbose)
	  fprintf(stderr, "WARNING: Program attempts to process without satelitte coincident information lookup.\n");
  }
  else {
	if (use_ascii_sat_coinc_file == 1) {
	  /* Not processing in the TSDIS environment--use ascii file */
	  if (sat_coinc_dir == NULL) {
		/* Use default file */
		memset(tmp_fname, '\0', MAX_FILENAME_LEN); 
		path = (char *) getenv("GVS_DATA_PATH");
		if (path == NULL)
		  path = "/usr/local/trmm/data";
		sprintf(tmp_fname, "%s/sat_coinc_data", path);
		sat_coinc_dir = tmp_fname;
	  }
	}
	else {
	  /* Process in TSDIS environment--use TSDIS' database */
	  sat_coinc_dir = "";
	}
  }

  if (verbose) {
	if (!no_sat_lookup)
	  if (verbose)
		fprintf(stderr, "Using sat. coinc. dir: %s\n", sat_coinc_dir);
  }
  /* PRocess product */
  if (process_product(infile, sat_coinc_dir, no_sat_lookup, period_type, 
					  sort_product, outfile) < 0) {
	if (verbose)
	  fprintf(stderr, "Failed to process product.\n");
	exit(-1);
  }

  if (verbose) 
	fprintf(stderr, "%s successful\n", argv[0]);

  exit (0);
} /* main */



/****************************************************************************/
/*                                                                          */
/*                            compute_frac_area                             */
/*                                                                          */
/****************************************************************************/
float compute_frac_area(L2A_53_SINGLE_RADARGRID *l2A53)
{
  /*
   * return  frac-area. frac_area = total rain rate / total pixels
   */
  float32  rain_rate;
  int    rain_pixel_count =0;
  int    r, c;
  float  frac_area = 0.0;
  int    total_pixels = 0;   /* total # of pixels in the boundary */

  if (l2A53 == NULL) return frac_area;

  /* compute frac. area */

  for (r = 0; r < HDF_MAX_ROWS; r++) {
	for (c = 0; c < HDF_MAX_COLS; c++) {
	  rain_rate = l2A53->rainRate[r][c];
	  if (rain_rate > 0.0) 
		rain_pixel_count++;

	  if (rain_rate > TK_DEFAULT) 
		/* if rr is not  missing/bad, it is in the boundary */
		total_pixels++;
	}
  }

  if (verbose) {
	fprintf(stderr, "rain_pixel_count = %d, total_pixels = %d\n", 
			rain_pixel_count, total_pixels);
  }
  frac_area = (float) rain_pixel_count / total_pixels;

  return frac_area;
} /* compute_frac_area */

/****************************************************************************/
/*                                                                          */
/*                          sort_2A52                                       */
/*                                                                          */
/****************************************************************************/
void sort_2A52(char *outfile)
{
  /*
   * Sort and remove duplicated entries.
   */
  char   *tmp_fname;
  char   tmp_str[MAX_FILENAME_LEN];
  char   cmd[MAX_CMD_LEN];

  tmp_fname = tempnam("/tmp/", "2A52_");
  if (tmp_fname == NULL) {
    memset(tmp_str, '\0', MAX_FILENAME_LEN);
    sprintf(tmp_str, "2A52_unsorted_file_%d", getpid());
    tmp_fname = tmp_str;
  }
  sprintf(cmd, "cp %s %s;sort_2A52_and_remove_dups %s >%s; rm %s", outfile, 
  		tmp_fname, tmp_fname, outfile, tmp_fname);
  if (verbose)
    fprintf(stderr, "Executing ... <%s>\n", cmd);
  system(cmd);
} /* sort_2A52 */

/****************************************************************************/
/*                                                                          */
/*                          process_product                                 */
/*                                                                          */
/****************************************************************************/
int process_product(char *infile, char *sat_coinc_dir, int no_sat_lookup,
					period_type_t period_type, int sort_product, char *outfile)
{

  /* Process 2A-52i product.
   * sate_coinc_file may be "".
   * Return 1 for successful; -1, otherwise.
   */
  /* Algorithm:
   * 1. Create the output file and write headers if period_type = BEGIN or
   *    Open the output file to append the data if period_type = MIDDLE | END.
   * 2. open 2A53 hdf file.
   * 2'. Return if 2A53 hdf file is an empty granule.
   * 3. load one VOS from the hdf file to vos_info_list-- 
   *    file may contain more than one VOS, 
   * 4. compute frac_area for VOS 
   * 5. goto step 3 until no more vos
   * 6. If no_sat_lookup goto step 9.
   * 7. Determine if there is a satellite coincidence during this granule hour
   * 8. If there is a satellite coincidence,
   *    determine which VOS has a 'hit' during this satellite coincidence.  
   *
   *    A 'hit' occurred when the satellite
   *    was closest to the radar during coincidence.  There is a time limit
   *    of +/-30 minutes in seeking out the closest VOS.

   *    In determining which VOS has a hit (only one VOS is desired for a
   *    particular time range). we will find the 
   *    VOS having time closest to the time of the sat. coinc data.
   *    Since the time limit in seeking out the closest VOS is +/-30 mins,
   *    we need to check at VOSes from the previous granule in the product 
   *    file if it exists and make modification if neccessary.
   *
   *    Note: The distance, date, and time of closest approach(CA) for VOS
   *    are based on the date of VOS and site name (in 2A53' metadata)
   *    and time of VOS (from each vos' grid structure data).
   *
   * 9. Write vos_info_list to outfile.

   * 10. close infile & outfile
   */

  L2A_53_SINGLE_RADARGRID vos_hdf_grid;
  IO_HANDLE hdf_fd;
  int    status;
  int    rc = 1;
  int    site_id;
  int    vos_count = 0;
  int    num_vos = 0;
  vos_info_t vos_info_list[MAX_VOS];
  DATE_STR granule_sdate, granule_edate;
  TIME_STR granule_stime, granule_etime;

  if (infile == NULL || outfile == NULL) return -1;


  memset(&hdf_fd, '\0', sizeof(IO_HANDLE));
  /* open input -- hdf file */
  status = TKopen(infile, TK_L2A_53S, TK_READ_ONLY, &hdf_fd);
  if (status != TK_SUCCESS) {
	if (verbose)
	  fprintf(stderr, "Failed TKopen <%s>\n", infile);
	TKreportWarning(W_2A52_OPEN_2A53_FILE_FAILED);
    return (-1);
  }

  /* Get info from infile's metadata. */
  if (get_info_from_infile(&hdf_fd, &granule_sdate, &granule_stime, 
						   &granule_edate, &granule_etime, &site_id, 
						   &num_vos) < 0) {
	if (verbose)
	  fprintf(stderr, "Failed to get info from infile %s.\n", infile);
	TKreportWarning(W_2A52_GET_INFO_FROM_INFILE_FAILED);
	return (-1);
  }
  /* Initialize */
  memset(&vos_info_list, '\0', sizeof(vos_info_t) * MAX_VOS);

  if (num_vos < 1) {
	if (verbose)
	  fprintf(stderr, "Input file is empty granule.\n");
	write_vos_info_list_to_outfile(period_type, vos_info_list, site_id, outfile);
	return 1;
  }
  if (verbose)
	fprintf(stderr, "Computing frac. area and find sat. coin info  for all VOSs...\n");     

  /* Read each  VOS from infile and store it in vos_info_list,
   * and calculate its rain rate.
   */
  vos_count = 0;
  while (TKendOfFile (&hdf_fd) != TK_EOF && rc != -1) {
	memset(&vos_hdf_grid, '\0', sizeof(L2A_53_SINGLE_RADARGRID));

	/* Read grid and time from infile  */
	if (TKreadGrid(&hdf_fd, &vos_hdf_grid) != TK_SUCCESS) {
	  if (verbose)
		fprintf(stderr, "TKreadGrid failed from infile.\n");
	  TKreportWarning(W_2A52_READ_2A53_FILE_FAILED);
	  rc = -1;
	}       

	if (set_vos_info(&vos_hdf_grid, &granule_sdate,
					 &vos_info_list[vos_count]) < 0) {
	  if (verbose)
		fprintf(stderr, "Failed processing data for vos %d.\n", vos_count);
	  rc = -1;
	}
	vos_count++;
  }  /* while */

  /* close input file */
  if (TKclose(&hdf_fd) == TK_FAIL) {
	TKreportWarning(W_2A52_CLOSE_2A53_FILE_FAILED);
	return (-1);
  }

  /* Abort creating product if failed reading and/or processing any vos.*/
  if (rc == -1) 
	return -1;

  if (sort_product && file_exist(outfile))
	/* Sort and remove duplicated entries. */
	sort_2A52(outfile);

  if (no_sat_lookup) {
	/* No satellite coincidence lookup, just write results to file */
	write_vos_info_list_to_outfile(period_type, vos_info_list, site_id, outfile);
  }
  else {
	/* Satellite coincidence lookup is required, determine vos' hit, 
	 * and write the results to file 
	 */
	if (process_closest_vos(&granule_sdate, &granule_stime, 
							&granule_edate, &granule_etime, site_id, 
							sat_coinc_dir, vos_info_list, period_type, 
							outfile) < 0)
	  return -1;
	/* Append results to outfile. */
	write_vos_info_list_to_outfile(period_type, vos_info_list, site_id, outfile);
  }

  if (vos_count <= 1 && period_type == BEGIN && rc == -1) 
	remove(outfile); /* Remove file if it contains no data entry */

  if (period_type == END && sort_product && file_exist(outfile))
	sort_2A52(outfile); /* Sort product for last granule. */

  return rc;


} /* process_product */


/****************************************************************************/
/*                                                                          */
/*                        set_vos_info                                      */
/*                                                                          */
/****************************************************************************/
int set_vos_info(L2A_53_SINGLE_RADARGRID *vos_hdf_grid, DATE_STR *date,
				 vos_info_t *vos_info)
{
  /* Compute rain percentage and set vos_info based on the information from 
   * vos_hdf_grid. Set default value for hit, ca_distance, ca_time, ca_date.
   *
   * REturn 1 for successful; -1, otherwise.
   */
  float  frac_area;

  if (vos_hdf_grid == NULL || date == NULL || vos_info == NULL) return -1;
  
  /* Compute the percent of the raining area. */
  frac_area = compute_frac_area(vos_hdf_grid);
  /* Rain percent is in percentage */
  vos_info->rain_percent = (int) (frac_area * 100);
  /*
  if (verbose) 
	fprintf(stderr, "rain_percent = %d\n", rain_percent);
	*/
  memcpy(&(vos_info->vos_date), date, sizeof(DATE_STR));
  memcpy(&(vos_info->vos_time), &(vos_hdf_grid->tktime), sizeof(TIME_STR));
  vos_info->hit = 0;
  sprintf(vos_info->ca_date_str,  "   %s", DEFAULT_CA_DATE_STR);
  sprintf(vos_info->ca_time_str, " %s", DEFAULT_CA_TIME_STR);
  vos_info->ca_distance = DEFAULT_CA_DISTANCE;

  return 1;

} /* set_vos_info */

#if 0
int TKwriteDBRainInfo(int instrID, struct tm *sStart_time, struct tm *sEnd_time, 
	int rain_percent)
{
  int status = TK_SUCCESS;
  fprintf(stderr, "id: %d, rr: %d\n", instrID, rain_percent);
  fprintf(stderr, "stime: %s\n", asctime(sStart_time));
  fprintf(stderr, "etime: %s\n", asctime(sEnd_time));
  return status;
}
#endif
/****************************************************************************/
/*                                                                          */
/*                         write_vos_info_list_to_outfile                   */
/*                                                                          */
/****************************************************************************/
int write_vos_info_list_to_outfile(period_type_t period_type, 
									vos_info_t *vos_info_list, int site_id,
									char *outfile)
{
  /* If period_type = BEGIN or outfile doesnot exist, then:
   *     Open outfile for writing, write header info then VOS info list,
   *     and close file.
   * Otherwise, 
   *     Open outfile for appending, write VOS info list to it, and close file.
   * Return 1 for successful; -1, otherwise.
   */
  char   *open_type = "a";
  FILE   *outfp;
  DATE_STR *vos_date;
  TIME_STR *vos_time;
  time_t vos_stime_sec, vos_etime_sec;
  int i;
  int sec, msec;
  char vos_time_str[MAX_STR_LEN], vos_date_str[MAX_STR_LEN];
  struct tm tmp_time;

  if (outfile == NULL) return -1;
  if (vos_info_list == NULL) return 1;

  /* Create new output file if either BEGIN or file doesnot exist. */
  if (period_type == BEGIN || gv_utils_file_exist(outfile) == 0)
	open_type = "w";

  /* open output file */
  if ((outfp = fopen(outfile, open_type)) == NULL) {
	perror("fopen outfile");
	TKreportWarning(W_2A52_OPEN_OUTFILE_FAILED);
	return (-1);
  }
  /* Write header if it's the beginning of file */
  if (period_type == BEGIN) {
	fprintf(outfp, "Date of        Time of                Distance of Closest  Date of  Time of\n");
	fprintf(outfp, "  VOS            VOS     %c Rain  Hit     Approach (CA)       CA       CA\n", '%');
	fprintf(outfp, "-----------------------------------------------------------------------------\n");
  }


  /* write results to outfile.   
   */
  if (verbose)
	fprintf(stderr, "Writing VOS info to file...\n");

  for (i = 0; i < MAX_VOS; i++) {

	vos_date = &(vos_info_list+i)->vos_date;
	vos_time = &(vos_info_list+i)->vos_time;
	if (vos_date->tkyear < 1900) break;   /* No more vos entry */
	memset(vos_time_str, '\0', MAX_STR_LEN);
	memset(vos_date_str, '\0', MAX_STR_LEN);
	sec = (int) vos_time->tksecond;
	msec = (int) ((vos_time->tksecond - sec) * 100);
	sprintf(vos_time_str, "%-2.2d:%-2.2d:%-2.2d.%-3.3d", vos_time->tkhour, 
			vos_time->tkminute, sec, msec) ;
	sprintf(vos_date_str, "%-4.4d-%-2.2d-%-2.2d", vos_date->tkyear, 
			vos_date->tkmonth, vos_date->tkday);
	fprintf(outfp, "%s  %s    %-5d %-5d    %-10.3f    %-10.10s %s\n", 
			vos_date_str, vos_time_str, (vos_info_list+i)->rain_percent, 
			(vos_info_list+i)->hit,
			(vos_info_list+i)->ca_distance, (vos_info_list+i)->ca_date_str, 
			(vos_info_list+i)->ca_time_str);

	/* Write %rain to TSDIS' db */
	date_time2system_time(vos_date, vos_time, &vos_stime_sec);
	vos_etime_sec = vos_stime_sec  + VOS_MINS*60;
	memcpy(&tmp_time, localtime(&vos_stime_sec), sizeof(struct tm));
	if (TKwriteDBRainInfo(site_id, &tmp_time, 
						  localtime(&vos_etime_sec), 
						  (vos_info_list+i)->rain_percent) != TK_SUCCESS) {
	  if (verbose)
		fprintf(stderr, "Failed to write rain percent to TSDIS' DB.\n");
	  TKreportWarning(W_2A52_FAILED_TO_WRITE_RAIN_TO_DB);
	}
  }
  fclose(outfp);
  return 1;
} /* write_vos_info_list_to_outfile */


/****************************************************************************/
/*                                                                          */
/*                         get_info_from_infile                             */
/*                                                                          */
/****************************************************************************/
int get_info_from_infile(IO_HANDLE *hdf_fd, 
						 DATE_STR *granule_sdate, TIME_STR *granule_stime, 
						 DATE_STR *granule_edate, TIME_STR *granule_etime, 
						 int *site_id, int *num_vos)
{

  /* Get start/end date/time, num_vos, and site_id (converted from string) 
   * from infile. 
   * Return 1 for successful; -1, otherwise.
   */
  char   site_name_str[MAX_NAME_LEN];

  if (hdf_fd == NULL || granule_sdate == NULL || granule_stime == NULL ||
	  granule_edate == NULL || granule_etime == NULL || site_id == NULL ||
	  num_vos == NULL) 
	return -1;
  memset(granule_sdate, '\0', sizeof(DATE_STR));
  memset(granule_edate, '\0', sizeof(DATE_STR));
  memset(granule_stime, '\0', sizeof(TIME_STR));
  memset(granule_etime, '\0', sizeof(TIME_STR));

  if (TKreadMetadataInt(hdf_fd, TK_BEGIN_DATE, granule_sdate) == TK_FAIL) {
	if (verbose)
	  fprintf(stderr, "Failed to read start date from infile.\n");
	TKreportWarning(W_2A52_READ_2A53_FILE_FAILED);
	return (-1);
  }       
  if (TKreadMetadataInt(hdf_fd, TK_END_DATE, granule_edate) == TK_FAIL) {
	if (verbose)
	  fprintf(stderr, "Failed to read end date from infile.\n");
	TKreportWarning(W_2A52_READ_2A53_FILE_FAILED);
	return (-1);
  }       
  if (TKreadMetadataInt(hdf_fd, TK_BEGIN_TIME, granule_stime) == TK_FAIL) {
	if (verbose)
	  fprintf(stderr, "Failed to read start time from infile.\n");
	TKreportWarning(W_2A52_READ_2A53_FILE_FAILED);
	return (-1);
  }       

  if (TKreadMetadataInt(hdf_fd, TK_END_TIME, granule_etime) == TK_FAIL) {

	if (verbose)
	  fprintf(stderr, "Failed to read end time from infile.\n");
	TKreportWarning(W_2A52_READ_2A53_FILE_FAILED);
	return (-1);
  }       

  if (granule_sdate->tkmonth < 1 || granule_sdate->tkmonth > 12 ||
	  granule_edate->tkmonth < 1 || granule_edate->tkmonth > 12 ||
	  granule_stime->tkhour < 0 || granule_stime->tkhour > 24 ||
	  granule_etime->tkhour < 0 || granule_etime->tkhour > 24 ) {
	if (verbose)
	  fprintf(stderr, "Date or time from infile is invalid.\n");
	return -1;
  }

  memset(site_name_str, '\0', MAX_NAME_LEN);
  if (TKreadMetadataChar(hdf_fd, TK_RADAR_NAME, site_name_str) == TK_FAIL) {

	if (verbose)
	  fprintf(stderr, "Failed to read date from infile.\n");
	TKreportWarning(W_2A52_READ_2A53_FILE_FAILED);

	return (-1);
  }
  if (strcmp(site_name_str, "NULL") == 0) {
	return -1;
  }
  *site_id = site_name2id(site_name_str);
  if (*site_id == -1) {
	if (verbose) 
	  fprintf(stderr, "Unable to recognize radar site name <%s>\n", site_name_str);
	return -1;
  }

  if (TKreadMetadataInt(hdf_fd, TK_NUM_VOS, num_vos) == TK_FAIL) 
	*num_vos = 0;

  return 1;
} /* get_info_from_infile */



/****************************************************************************/
/*                                                                          */
/*                    find_closest_vos_in_current_granule                   */
/*                                                                          */
/****************************************************************************/
int find_closest_vos_in_current_granule(DATE_STR *ca_date, TIME_STR *ca_time, 
								  vos_info_t *vos_info_list, 
								  int *vos_index)
{
  /* Find the vos from vos_info_list having the time closest to 
   * ca_date/ca_time within 30 mins-- this vos is closest to the satellite. 
   * Set vos_index to reflect the index for that vos.
   * Will pick the first vos if there are two vos'es having the same 
   * time difference.
   * Set vos_index to -1 if there is no closest vos.
   * return 1 for successful; -1, otherwise.
   */
  int i;
  time_t vos_time_sec, ca_time_sec;
  int closest_vos_index = -1;
  time_t closest_vos_diff_time = -1, diff_time;

  *vos_index = -1;
  if (ca_date == NULL || ca_time == NULL || vos_index == NULL) 
	return -1;
  if (vos_info_list == NULL) 
	return 1;

  date_time2system_time(ca_date, ca_time, &ca_time_sec);
  
  for (i = 0; i < MAX_VOS; i++) {

	if ((vos_info_list+i)->vos_date.tkyear <= 0) break;
	date_time2system_time(&(vos_info_list+i)->vos_date, 
						  &(vos_info_list+i)->vos_time, &vos_time_sec);
	diff_time = difftime(vos_time_sec, ca_time_sec);
	if ((abs(diff_time) < closest_vos_diff_time && abs(diff_time) <= 30*60)
		|| closest_vos_diff_time < 0) {
	  closest_vos_diff_time = abs(diff_time);
	  closest_vos_index = i;
	}
  }
  *vos_index = closest_vos_index;
  return 1;

} /* find_closest_vos_in_current_granule */

/****************************************************************************/
/*                                                                          */
/*                      find_and_set_closest_vos_in_previous_granule        */
/*                                                                          */
/****************************************************************************/
int	find_and_set_closest_vos_in_previous_granule(char *outfile, 
												 vos_info_t *curr_closest_vos,
												 char *ca_date_str, 
												 char *ca_time_str,
												 float ca_distance)
{
  /* Find a vos from the previous granule in the end of outfile that has
   * the time closer to the ca_date/ca_time than curr_closest_vos does. 
   * Update that vos' satellite coincidence information if it exists.
   * For non closest VOSes within 30 mins of the CA's time, set their hit 
   * and CA' fields to the default value (no hit).
   *
   * This routine calls a script to do all that.
   * Return 1 if the closest vos was found from the previous granule; 
   * 0, didn't find the closest vos from the previous granule; -1, otherwise.
   *
   * Note: Assume that the file is already in 
   *       date/time order. Thus, need to check the last few VOS'es having 
   *       the time +/- 30 mins the the CA' time at the bottom of the file only.
   *
   */

  time_t ca_time_sec, curr_vos_time_sec;
  char cmd[MAX_CMD_LEN];
  DATE_STR date, *vos_date;
  TIME_STR time, *vos_time;
  int rc;

  if (outfile == NULL) return 1;
  if (ca_date_str == NULL || ca_time_str == NULL) return -1;

  if (curr_closest_vos == NULL) {
	memset(&date, '\0', sizeof(DATE_STR));
	vos_date = &date;
	memset(&time, '\0', sizeof(TIME_STR));
	vos_time = &time;
  }
  else {
	vos_date = &(curr_closest_vos->vos_date);
	vos_time = &(curr_closest_vos->vos_time);
  }
  /* cmd: process_new_closest_vos_in_the_previous_granule ca_date
   *            ca_time ca_distance curr_vos_date curr_vos_time inoutfile
   */
  date_time_strs2seconds2(ca_date_str, ca_time_str, &ca_time_sec);
  date_time2system_time(&(curr_closest_vos->vos_date), 
						&(curr_closest_vos->vos_time), &curr_vos_time_sec);

  memset(cmd, '\0', MAX_CMD_LEN);
  sprintf(cmd, "process_new_closest_vos_in_the_previous_granule %s %s %f %d-%d-%d %d:%d:%d  %s",
		  ca_date_str, ca_time_str, ca_distance,
		  vos_date->tkyear, vos_date->tkmonth, curr_closest_vos->vos_date.tkday,
		  vos_time->tkhour, vos_time->tkminute, vos_time->tksecond, outfile);
  rc = system(cmd);
  rc = rc >> 8;

  if (rc == 127 || rc < 0) {
	if (verbose)
	  fprintf(stderr, "Failed executing process_new_closest_vos_in_the_previous_granule\n");
	return -1;
  }
  if (rc == 1) return 1;  /* Closest vos from the previous granule was found */
  return 0;
  
} /* find_and_set_closest_vos_in_previous_granule */


/****************************************************************************/
/*                                                                          */
/*                      process_closest_vos                                 */
/*                                                                          */
/****************************************************************************/
int process_closest_vos(DATE_STR *granule_sdate, TIME_STR *granule_stime, 
						DATE_STR *granule_edate, TIME_STR *granule_etime, 
						int site_id, char *sat_coinc_dir,
						vos_info_t *vos_info_list, period_type_t period_type,
						char *outfile)
					   
{
  /* Determine and set sat coinc data for VOS that is closest to the
   * satellite coincident data occurring +/- 30 mins of granule_sdate/stime
   * and granule_edate/etime.  The closest vos may be in this granule
   * or in the previous granule already in the outfile.
   * Return 1 for successful; -1, otherwise.
   *
   */
  char   ca_date_str[MAX_STR_LEN], ca_time_str[MAX_STR_LEN];
  float ca_distance;
  DATE_STR ca_date;
  TIME_STR ca_time;
  int vos_index;
  vos_info_t *curr_closest_vos = NULL;
  int rc;

  if (granule_sdate == NULL || granule_stime == NULL ||
	  granule_edate == NULL || granule_etime == NULL) return -1;
  if (vos_info_list == NULL) return 1;

  /* Query for satellite coincidence occurring during this time interval:
   * [granule start time -30 mins, granule end time + 30 mins].
   */
  if ((rc = get_sat_coinc_info2(site_id, *granule_sdate, *granule_stime,
								*granule_edate, *granule_etime, verbose, 
								sat_coinc_dir, ca_date_str, ca_time_str, 
								&ca_distance)) < 0) {
	if (verbose)
	  fprintf(stderr, "Failed calling do_query_coinc_info()\n");
	TKreportWarning(W_2A52_GET_COIN_INFO_FAILED);
	/* Abort without writing the result to file. */
	return -1;
  }
  if (rc == 1) {
	/* There is sat coinc data during this granule +/- 30 mins.
	 */
	/*    1. Find the VOS that has the time 
	 *       closest to the time of the satellite coinc. data.
	 *
	 *    2. Since there is a time limit of +/-30 minutes in seeking out the 
	 *       'closest' VOS, we need to check for VOSs from the previous
	 *       granule (that is from the product file).
	 *       Therefore, if the time's minute of the satellite coinc data
	 *       is less than 30, need to look up from the product file if
	 *       it exists for a vos that has the time closer to the time of 
	 *       the satellite coinc data than the VOS found in step (a). 
	 *       If such VOS exists, modify its hit, CA_distance, CA date, 
	 *       and CA time.
	 */

	/* Convert date/time strings to DATE_STR/TIME_STR
	 */
	if (verbose) 
	  fprintf(stderr, "Calling find_closest_vos_in_current_granule()...\n");
	date_time_strs2date_and_time2(ca_date_str, ca_time_str, &ca_date,
								  &ca_time);
	find_closest_vos_in_current_granule(&ca_date, &ca_time, vos_info_list, 
										&vos_index);
	if (vos_index < 0 || vos_index >= MAX_VOS ||
		vos_info_list[vos_index].vos_date.tkyear < 1900) 
	  curr_closest_vos = NULL; /* No closest vos in this granule. */
	else
	  curr_closest_vos = &vos_info_list[vos_index];
	
	if (ca_time.tkminute < 30 && period_type != BEGIN) {
	  /* Need to check the previous granule from outfile for vos having the
	   * time closest to the ca_date/ca_time and update its satellite 
	   * coincidence information if it exists. Reset CA and hit fields to
	   * default for VOS that was considered closest in the previous granule
	   * but not the closest any longer.
	   */
	  if (verbose) 
		fprintf(stderr, "Calling find_and_set_closest_vos_in_previous_granule()...\n");	  
	  if (find_and_set_closest_vos_in_previous_granule(outfile, 
													   curr_closest_vos,
													   ca_date_str, ca_time_str,
													   ca_distance) == 1) 
		curr_closest_vos = NULL;   /* Found the closest VOS in the previous
					    * granule.
					    */
	}
	/* Set satellite coinc data for the 'hit' vos in vos_info_list.*/
	if (curr_closest_vos) {
	  curr_closest_vos->hit = 1;
	  strcpy(curr_closest_vos->ca_date_str, ca_date_str);
	  strcpy(curr_closest_vos->ca_time_str, ca_time_str);
	  curr_closest_vos->ca_distance = ca_distance;
	}
	
  }
  else {
	/* There is no sat coinc data during this granule +/- 30 mins.
	 */
  }
  return 1;
} /* process_closest_vos */
