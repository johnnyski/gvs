/*
 * create_level_3_rrmap_hdf.c:
 *    Create 3A-53 or 3A-54 product in HDF format based on the intermediate
 *    accumulation map, one 2A-53 product in HDF format, and admin. info. file.
 *    Note: 
 *        * 2A-53 and the admin. info. file provide the metadata only.
 *        * The admin info file and the accumm mmap file are not used
 *        * to create empty granule product.
 *         
 *    Program's exit code:
 *      -2 Program was aborted by SIGINT (^C).
 *      -1 Failed.
 *       0 successful.
 *
 *    Requires '../../scripts/grep_runtime_parm.pl'
 *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * By: Ngoc-Thuy Nguyen
 *     Science Systems and Applications, Inc. (SSAI)
 *     NASA/TRMM Office
 *     nguyen@trmm.gsfc.nasa.gov
 *     August 25, 1997
 *
 ***********************************************************************/

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <IO.h>
#include <IO_GV.h>
#include "gvs_metadata.h"
#include <TKerrHandle.h>
#include <TS_ACC_RRMAP_63.h>
#include <gv_utils.h>


typedef enum {
  PENTAD, MONTHLY
} period_type_t;

#define MAX_LINE_LEN        2000      /* Large to be able to read each line
									   * from the acc. map. file.
									   */
#define MAX_FILENAME_LEN    256
#define MAX_CMD_LEN         500
#define MAX_COLS            151
#define MAX_ROWS            151


static char *this_prog = "";
static int verbose = 0;

/********************** Function Prototypes *****************************/
void process_argvs(int argc, char **argv, char **metadata_infile,
				   product_type_t *product, int *create_empty_granule, 
				   char **anomaly_flag_msg, char **acc_map_infile,
				   char **level2_rrmap_hdf_infile, char **admin_info_infile, 
				   char **vos_time_outfile,
				   char **hdf_outfile);
int create_vos_times_file(char *admin_info_infile, char *vos_times_outfile);
void doexit_handler(int sig);
/************************************************************************/
/*                                                                      */
/*                                main                                  */
/*                                                                      */
/************************************************************************/
main(int argc, char **argv)
{
  product_type_t product;
  char *metadata_infile = NULL, *acc_map_infile = NULL,
	*level2_rrmap_hdf_infile = NULL, *admin_info_infile = NULL,
	*hdf_outfile = NULL, *vos_times_outfile = NULL, *anomaly_flag_msg = NULL;
  gv_metadata_t metadata;
  period_type_t period_flag;
  int create_empty_granule = 0;

  this_prog = argv[0];
  signal(SIGINT, doexit_handler);
  signal(SIGSTOP, doexit_handler);
  signal(SIGKILL, doexit_handler);
  signal(SIGFPE, doexit_handler);
  signal(SIGILL, doexit_handler);
  signal(SIGSEGV, doexit_handler);

  process_argvs(argc, argv, &metadata_infile,  &product, &create_empty_granule,
				&anomaly_flag_msg,
				&acc_map_infile, &level2_rrmap_hdf_infile, &admin_info_infile, 
				&vos_times_outfile, &hdf_outfile);
  
  if (verbose) {
	fprintf(stderr, "metadata_infile: %s\n", metadata_infile);
	fprintf(stderr, "product:         %d\n", product);

	fprintf(stderr, "level2_rrmap_hdf_infile: %s\n", level2_rrmap_hdf_infile);
	fprintf(stderr, "hdf_outfile:             %s\n", hdf_outfile);
	fprintf(stderr, "vos_times_outfile:       %s\n", vos_times_outfile);
	if (create_empty_granule == 0) {
	  fprintf(stderr, "acc_map_infile:  %s\n", acc_map_infile);
	  fprintf(stderr, "admin_info_infile:       %s\n", admin_info_infile);

	}

  }
  memset(&metadata, '\0', sizeof(gv_metadata_t));
  if (verbose) 
	fprintf(stderr, "%s: Getting metadata for level 3...\n", this_prog);

  if (product == P3A53) 
	period_flag = PENTAD;
  else 
	period_flag = MONTHLY;

  if (get_metadata_for_level3(create_empty_granule, anomaly_flag_msg,
							  metadata_infile, level2_rrmap_hdf_infile, 
							  admin_info_infile, hdf_outfile, 
							  period_flag, product, &metadata) <0) {
	fprintf(stderr, "%s: Error: Failed to get metadata.\n", this_prog);
	TKreportWarning(W_ACC_RRMAP_GET_METADATA_FAILED);
	exit(-1);
  }

  if (verbose)
	fprintf(stderr, "%s: Constructing HDF product...\n", this_prog);
  if (construct_hdf_product(create_empty_granule,
							acc_map_infile, &metadata, product, 
							hdf_outfile) < 0) {
	fprintf(stderr, "%s: Error: Failed to construct hdf product.\n", this_prog);
	TKreportWarning(W_ACC_RRMAP_CREATE_HDF_FAILED);
	exit(-1);
  }

  /* Create the time file even if it's an empty granule product. */
  if (verbose) 
	fprintf(stderr, "%s: Creating vos times file...\n", this_prog);
  if (create_vos_times_file(admin_info_infile, vos_times_outfile) < 0) {
	fprintf(stderr, "%s: Error: Failed to create vos times file.\n", this_prog);
	/* Error. Remove the HDF file. */
	remove (hdf_outfile);
	TKreportWarning(W_ACC_RRMAP_CREATE_TIME_FILE_FAILED);
	exit(-1);
  }

  exit(0);
} /* main */

/************************************************************************/
/*                                                                      */
/*                                 usage                                */
/*                                                                      */
/************************************************************************/
void usage()
{
  fprintf(stderr, "Create Either 3A-53 or 3A-54 Product in HDF Format Using its accu_map.\n");
  fprintf(stderr, "Usage (%s):\n", PROG_VERSION);
  fprintf(stderr, " %s [-v] [-m metadata_file] [-e] [-a anomaly_flag_msg] \n", this_prog);
  fprintf(stderr, "      {3A-53 | 3A-54}\n"
		          "      [acc_map_infile] 2A-53_hdf_infile [admin_info_infile] \n"
                  "      vos_times_outfile hdf_outfile\n");
  fprintf(stderr, "     where,\n");
  fprintf(stderr, "       -v:  Verbose diagnostics during execution.\n");
  fprintf(stderr, "       -m:  Specify filename for metadata. \n");
  fprintf(stderr, "            Default:\"\"\n");
  fprintf(stderr, "       -e:  Create empty granule product.\n");
  fprintf(stderr, "       -a:  Specify the reason why a granule is/isnot empty. The possible values are:\n"
                  "            'EMPTY: GENERATED AFTER SOFTWARE ERROR'\n"
			      "            'EMPTY: NO DATA DUE TO NO RAIN.'\n"
                  "            'EMPTY: NO DATA  RECORDED.'\n"
                  "            'EMPTY: DATA RECORED BUT STILL MISSING'\n"
                  "            'EMPTY: REASON UNKNOWN'\n"
                  "            'NOT EMPTY: POSSIBLE PROBLEM'\n"
                  "            'NOT EMPTY'\n"
			      "            Default: Obtain from 2A-53_hdf_infile.\n");
  fprintf(stderr, "   acc_map_infile    ASCII accumulated map for either 3A-53 or 3A-54.\n");
  fprintf(stderr, "   2A-53_hdf_infile  2A-53 product in HDF format.  Compressed file is allowed.\n");
  fprintf(stderr, "   admin_info_infile ASCII intermediate file for either '3A-53' or '3A-54'.\n");
  fprintf(stderr, "   vos_times_outfile Specify the filename for dates and times of all\n"
                  "                     VOSes included in this product.\n");
  fprintf(stderr, "   hdf_outfile       Specify the filename for output file.\n");
  fprintf(stderr, "\n   Note: Do not specify acc_map_infile and admin_info_infile\n"
                  "         if '-e' is specified.\n");
  fprintf(stderr, "\n");
  TKreportWarning(W_ACC_RRMAP_BAD_USAGE);
  exit(-1);
} /* usage */


/************************************************************************/
/*                                                                      */
/*                               process_argvs                          */
/*                                                                      */
/************************************************************************/
void process_argvs(int argc, char **argv, char **metadata_infile,
				   product_type_t *product, int *create_empty_granule, 
				   char **anomaly_flag_msg, char **acc_map_infile,
				   char **level2_rrmap_hdf_infile, char **admin_info_infile, 
				   char **vos_times_outfile,
				   char **hdf_outfile)
{
  extern char *optarg;
  extern int optind, optopt;

  int c;

  if (argc < 4) 
	usage();


  while ((c = getopt(argc, argv, ":m:a:ve")) != -1) {
	switch (c) {
	case 'v': verbose = 1; break;
	case 'e': *create_empty_granule = 1; break;
	case 'm':
	  *metadata_infile= (char *) strdup(optarg); 
	  if (*metadata_infile[0] == '-') usage();
	  break;
	case 'a':
	  if (optarg[0] == '-') usage();
	  *anomaly_flag_msg = (char *) strdup(optarg);
	  break;
    case '?': fprintf(stderr, "option -%c is undefined\n", optopt);
	  usage();
    case ':': fprintf(stderr, "option -%c requires an argument\n",optopt);
	  usage();
    default: break;
    }
  }

  /* must have 6 params listed if '-e' is not specified; 4, otherwise. */
  if (((argc - optind != 6) && *create_empty_granule == 0) ||
	  ((argc - optind != 4) && *create_empty_granule == 1))
	usage();

  if (strcmp(argv[optind], "3A-54") == 0) 
	*product = P3A54;
  else if (strcmp(argv[optind], "3A-53") == 0) 
	*product = P3A53;
  else {
	fprintf(stderr, "%s: Error: Program only recognizes 3A-53 and 3A-54.\n", this_prog);
	TKreportWarning(W_ACC_RRMAP_INVALID_PROG);
	exit(-1);
  }
  optind++;
  if (*create_empty_granule) {
	*level2_rrmap_hdf_infile = (char *) strdup(argv[optind++]);
	*vos_times_outfile = (char *) strdup(argv[optind++]);
	*hdf_outfile = (char *) strdup(argv[optind++]);
  }
  else {
	*acc_map_infile = (char *) strdup(argv[optind++]);
	*level2_rrmap_hdf_infile = (char *) strdup(argv[optind++]);
	*admin_info_infile = (char *) strdup(argv[optind++]);
	*vos_times_outfile = (char *) strdup(argv[optind++]);
	*hdf_outfile = (char *) strdup(argv[optind++]);
  }
} /* process_argvs */



/************************************************************************/
/*                                                                      */
/*                            get_metadata_for_level3                   */
/*                                                                      */
/************************************************************************/
int get_metadata_for_level3(int create_empty_granule, 
							char *anomaly_flag_msg,
							char *metadata_file, char *level2_rrmap_hdf_file,
							char *admin_info_file, char *hdf_outfile,
							period_type_t period_flag, product_type_t product,
							gv_metadata_t *metadata) 
{
  /* Get general metadata fields for product from level2 rrmap hdf file,
   * and metadata_file. 
   * The memory for metadata must be allocated by the caller.
   * Get begin/end date and time from admin_info_file if 
   * create_empty_granule == 0; else construct date/time based on 
   * level2 rrmap file.
   *
   * admin_info_file is the intermediate file for '3A-53' or '3A-54'.
   * 
   * The format of inter_admin_info_file:
   *  <begin of file>
   #  last_processed_vos_time   << Last VOS -- it may not be included in the
   #                            << accumulation.
   *  vos_time1 vos_time2 ...   << list of times of
   *  ...                       << all VOSes included in the  
   *  selected_vos_time_list    << accumulation.  Time format: mm/dd/yyyy hh:mm:ss
   *  <end of file>
   *
   *
   * level2 rrmap hdf file can be compressed.
   * Return 1 for successful; -1, otherwise.
   */
  FILE *fp;
  char line[MAX_LINE_LEN];
  IO_HANDLE hdf_fd;
  int rc = 1;
  DATE_STR begin_vos_date, end_vos_date;
  TIME_STR begin_vos_time, end_vos_time;
  char new_level2_rrmap_hdf_file[MAX_FILENAME_LEN];
  char date_str[MAX_NAME_LEN];
  char *granule_id = NULL, anomaly_flag_msg2[MAX_NAME_LEN];
  char *alg_id,	*data_id, *contact_name, *alg_version;

  if (level2_rrmap_hdf_file == NULL || hdf_outfile == NULL ||
	  (create_empty_granule == 0 && admin_info_file == NULL) ||
	  metadata == NULL) return -1;

  memset(&begin_vos_date, '\0', sizeof(DATE_STR));
  memset(&begin_vos_time, '\0', sizeof(TIME_STR));

  /* Open and get general metadata from level2 rrmap hdf file. */
  create_tmp_filename(level2_rrmap_hdf_file, "", new_level2_rrmap_hdf_file);
  uncompress_file(level2_rrmap_hdf_file, new_level2_rrmap_hdf_file);
  if (verbose) 
	fprintf(stderr, "%s: Getting metadata from file: %s\n", this_prog, new_level2_rrmap_hdf_file);

  memset(&hdf_fd, '\0', sizeof(IO_HANDLE));
  TKopen(new_level2_rrmap_hdf_file, TK_L2A_53S, TK_READ_ONLY, &hdf_fd);
  if (read_l3hdf_metadata_from_l2granule(&hdf_fd, metadata, 0) < 0) {
	fprintf(stderr, "%s: Error: Failed to get metadata from file: %s\n",
			this_prog, new_level2_rrmap_hdf_file);
	TKclose(&hdf_fd);	
	remove(new_level2_rrmap_hdf_file);
	return -1;
  }
  
  if (TKreadMetadataInt(&hdf_fd, TK_BEGIN_DATE, &begin_vos_date) == TK_FAIL) {
	TKclose(&hdf_fd);
	remove(new_level2_rrmap_hdf_file);
	return -1;
  }
  if (anomaly_flag_msg == NULL) {
	if (TKreadMetadataChar(&hdf_fd, TK_ANOMALY_FLAG, anomaly_flag_msg2) == 
		TK_FAIL) {
	  TKclose(&hdf_fd);
	  remove(new_level2_rrmap_hdf_file);
	  return -1;
	}
  }
	
  TKclose(&hdf_fd);
  remove(new_level2_rrmap_hdf_file);

  if (create_empty_granule == 0) {
	/* Read date/time from admin info file */
	if (verbose)
	  fprintf(stderr, "%s: Get metadata from file <%s>\n", this_prog, admin_info_file);
	/* Open and get the first and last vos times from the admin info file. */
	if ((fp = fopen(admin_info_file, "r")) == NULL) {
	  perror("open admin_info_file");
	  return -1;
	}
	/* Skip the first line. Read first vos date from file. */
	memset(line, '\0', MAX_LINE_LEN);
	fgets(line, MAX_LINE_LEN, fp);
	memset(line, '\0', MAX_LINE_LEN);
	fgets(line, MAX_LINE_LEN, fp);
	if (sscanf(line, "%s %*s", date_str) != 1) {
	  fprintf(stderr, "%s:Error: File <%s> is obsolete.  It should starts with mm/dd/yyyy .\n", this_prog, admin_info_file);
	  fclose(fp);
	  return -1;
	}
	
	fclose(fp);
	date_time_strs2date_and_time(date_str, NULL,
								 &begin_vos_date, NULL);
  }
  else {
	/* Empty granule */
	if (anomaly_flag_msg != NULL)
	  /* Set anomaly flag */
	  strcpy(metadata->anomaly_flag, anomaly_flag_msg);
	else
	  strcpy(metadata->anomaly_flag, anomaly_flag_msg2);
  }

  /* begin date/time: 
   *      yyyy/mm/dd 00:00:00 (date of the first day in the period).
   * end date/time:  
   *      yyyy/mm/dd 00:00:00 (date of the first day of the next period).
   */

  switch (period_flag) {
  case PENTAD:
	 get_dates_times_for_pentad_period(&begin_vos_date, &begin_vos_time,
									   &begin_vos_date, &begin_vos_time,
									   &end_vos_date, &end_vos_time);
	break;
  case MONTHLY:
	begin_vos_date.tkday = 1; /* Set to the first day of that month. */
	/* Set end date to the first day of the next month at 00:00:00. */
	get_date_for_end_of_monthly_period(&begin_vos_date, &begin_vos_time,
									   &end_vos_date, &end_vos_time);
	break;
  default:
	  break;
  }
  memcpy(&(metadata->begin_date), &begin_vos_date, sizeof(DATE_STR));
  memcpy(&(metadata->end_date), &end_vos_date, sizeof(DATE_STR));
  memcpy(&(metadata->begin_time), &begin_vos_time, sizeof(TIME_STR));
  memcpy(&(metadata->end_time), &end_vos_time, sizeof(TIME_STR));


  /* Set fields specific to level 3 only. */
  if (strrchr(hdf_outfile, '/') == NULL) 
	granule_id = hdf_outfile;                   /* Filename has no path */
  else
	granule_id = strrchr(hdf_outfile, '/') + 1; /* Skip to filename */
  strcpy(metadata->granule_id, granule_id);

  if (verbose)
	fprintf(stderr, "%s: Getting metadata from file: %s\n", this_prog, metadata_file);
  if (get_metadata_from_metadata_file(metadata_file, &begin_vos_date,
									  &end_vos_date, metadata) < 0) 
	return -1;

  switch (product) {
  case P3A53:
	alg_id = P3A53_ALG_ID;
	data_id = P3A53_DATA_ID;
	contact_name = P3A53_CONTACT_NAME;
	alg_version = P3A53_ALG_VERSION;
	break;
  case P3A54:
	alg_id = P3A54_ALG_ID;
	data_id = P3A54_DATA_ID;
	contact_name = P3A54_CONTACT_NAME;
	alg_version = P3A54_ALG_VERSION;
	break;
  default:
	return -1;
  }

  strcpy(metadata->alg_id, alg_id);
  strcpy(metadata->data_id, data_id);
  strcpy(metadata->contact, contact_name);
  strcpy(metadata->alg_version, alg_version);

  return rc;
} /* get_metadata_for_level3 */





/************************************************************************/
/*                                                                      */
/*                            construct_hdf_product                     */
/*                                                                      */
/************************************************************************/
int construct_hdf_product(int create_empty_granule,
						  char *acc_map_file, gv_metadata_t *metadata, 
						  product_type_t product, char *hdf_outfile)
{
  /* Construct hdf file for product.
   * Return 1 for successful; -1, otherwise.
   */
  IO_HANDLE hdf_fd;
  FILE *fp;
  int data_type;
  void *pgrid;
  int r, c;
  char line[MAX_LINE_LEN];
  float rr;
  float32 new_rr;
  int status;
  char  *token;

  union _grid{
	L3A_53_SINGLE_RADARGRID g3A53;
	L3A_54_SINGLE_RADARGRID g3A54;
  } grid;
  
  if ((create_empty_granule == 0 && acc_map_file == NULL) ||
	  metadata == NULL || hdf_outfile == NULL)
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

  if (create_empty_granule == 0) {
	/* Open the acc_map file and read in the map. */
	if ((fp = fopen(acc_map_file, "r")) == NULL) {
	  perror("open acc_map_file");
	  return -1;
	}

	/* Fill grid's map. */
	for (r = 0; r < MAX_ROWS; r++) {
	  memset(line, '\0', MAX_LINE_LEN);
	  if (fgets(line, MAX_LINE_LEN, fp) == NULL) {
		fprintf(stderr, "%s:Error: acc. map has fewer than %d rows.\n", this_prog, MAX_ROWS);
		fclose(fp);
		return -1;
	  }
	  token = strtok(line, " ");
	  for (c = 0; c < MAX_COLS; c++) {
		if (token == NULL) {
		  fprintf(stderr, "%s:Error: acc. map has fewer than %d cols.\n", this_prog, MAX_COLS);
		  fclose(fp);
		  return -1;
		}
		memset(&rr, '\0', sizeof(float));
		sscanf(token, "%f", &rr);

		if (product == P3A53) {
		  if (rr == -99.0) 
			new_rr = TK_DEFAULT;
		  else
			new_rr = rr;
		  ((L3A_53_SINGLE_RADARGRID *) pgrid)->pentadRainfall[r][c] = new_rr;
		  
		}
		else if (product == P3A54) {
		  if (rr == -99.0) 
			new_rr = TK_DEFAULT;
		  else
			new_rr = rr;
		  
		  ((L3A_54_SINGLE_RADARGRID *) pgrid)->monthlyRainfall[r][c] = new_rr;
		}
		token = strtok(NULL, " ");
	  }
	}
	fclose(fp);
  } /* Non empty granule */

  /* Open the hdf file for write. */
  memset(&hdf_fd, '\0', sizeof(IO_HANDLE));
  status = TKopen(hdf_outfile, data_type, TK_NEW_FILE, &hdf_fd);
  if (status != TK_SUCCESS) {
	fprintf(stderr, "%s:Error: Failed to open %s\n", this_prog, hdf_outfile);
	return -1;
  }
  
  if (TKwriteGrid(&hdf_fd, pgrid) != TK_SUCCESS) {
	fprintf(stderr, "%s:Error: Failed to write grid to hdf file.\n", this_prog);
	goto ERROR;
  }

  if (write_hdf_metadata_for_l3granule(&hdf_fd, metadata, 0) < 0) {
	fprintf(stderr, "%s:Error: Failed to write metadata to hdf file.\n", this_prog);
	goto ERROR;
  }

  if (TKclose(&hdf_fd) != TK_SUCCESS) {
	fprintf(stderr, "%s:Error:Failed to close hdf file.\n", this_prog);
	return -1;
  }
  return 1;

  ERROR:
  	TKclose(&hdf_fd);
	return -1;
} /* construct_hdf_product */



/************************************************************************/
/*                                                                      */
/*                          create_vos_times_file                       */
/*                                                                      */
/************************************************************************/
int create_vos_times_file(char *admin_info_infile, char *vos_times_outfile)
{
  /* Create vos times oufile that contains a list of date and time of all
   * VOSes included in this product.
   *
   * The format of inter_admin_info_file:
   *  <begin of file>
   #  last_processed_vos_time   << Last VOS -- it may not be included in the
   #                            << accumulation.
   *  vos_time1 vos_time2 ...   << list of times of
   *  ...                       << all VOSes included in the  
   *  selected_vos_time_list    << accumulation.  Time format: mm/dd/yyyy hh:mm:ss
   *  <end of file>
   *
   * It may create an empty time file.

   * Return 1 for successful; -1, otherwise.
   */
  char cmd[MAX_CMD_LEN];
  int rc;

  if (vos_times_outfile == NULL) return -1;
  memset(cmd, '\0', MAX_CMD_LEN);
  if (admin_info_infile == NULL)
	sprintf(cmd, "touch %s", vos_times_outfile); /* Create an empty file */
  else
	sprintf(cmd, "tail +2 %s >%s", admin_info_infile, vos_times_outfile);
  rc = system(cmd);
  if (rc != 0) 
	return -1;
  return 1;
} /* create_vos_times_file */


/************************************************************************/
/*                                                                      */
/*                         doexit_handler                               */
/*                                                                      */
/************************************************************************/
void doexit_handler(int sig)
{
  fprintf(stderr, "%s: Got signal <%d>. Bye.\n", this_prog, sig);
  fflush(0);
  if (sig == SIGINT || sig == SIGKILL || sig == SIGSTOP) {
	TKreportWarning(W_ACC_RRMAP_ABORT_EXEC);
	exit(-2);
  }
  TKreportWarning(W_ACC_RRMAP_STOP_ON_ERROR);
  exit(-1);
}
