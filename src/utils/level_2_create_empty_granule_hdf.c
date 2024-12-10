/*
 * level_2_create_empty_granule_hdf.c
 *        - Create empty granule hdf file for 2A-53, 2A-54, or 2A-55.
 *
 * Requires:
 *
 *  $GVS_DATA_PATH/gv_radar_site_info.data
 *
 *----------------------------------------------------------------
 *
 * By: Ngoc-Thuy Nguyen
 *     Science Systems and Applications, Inc. (SSAI)
 *     NASA/TRMM Office
 *     nguyen@trmm.gsfc.nasa.gov
 *     December 30, 1997
 *
 ***********************************************************************/

/* Note:
 *
 * This routine will copy the generic metadata fields from
 * the qc hdf file.
 */

#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

/* TSDIS toolkit include files*/
#include <IO.h>
#include <IO_GV.h>
#include <TKerrHandle.h>
#include "gvs_metadata.h"
#include <gv_utils.h>

int verbose = 0;

/***************************************************************************/
/*                                                                         */
/*                             open_hdf_file                               */
/*                                                                         */
/***************************************************************************/
int open_hdf_file(char *hdf_fname, char mode, char pcode, IO_HANDLE *hdf_fd)
{
  /* Copied from 'level_2_singlevos_hdf_to__ghdf.c'.
   * Return 1 upon successful; -1, otherwise.
   */
  int status;

  if (hdf_fname == NULL || strlen(hdf_fname) < 1|| hdf_fd == NULL) return -1;
  memset(hdf_fd, '\0', sizeof(IO_HANDLE));

  /* open HDF file */
  switch (pcode) {
	case '3':  /* 2A-53 */
	
	  /* open hdf file */
	  status = TKopen(hdf_fname, TK_L2A_53S, mode, hdf_fd);
	  break;
	
	case '4':  /* 2A-54 */
	  status = TKopen(hdf_fname, TK_L2A_54S, mode, hdf_fd);
	  break;
	case '5':  /* 2A-55 */
	  status = TKopen(hdf_fname, TK_L2A_55S, mode, hdf_fd);
	  break;

  default:
	fprintf(stderr, "product code <%c> is unknown\n", pcode);
	return -1;
  }
  /* Check the Error Status */
  if (status != TK_SUCCESS) {
	return -1;
  }
  return 1;

} /* open_hdf_file */

/***************************************************************************/
/*                                                                         */
/*                           read_prod_specific_metadata_from_file         */
/*                                                                         */
/***************************************************************************/
int read_prod_specific_metadata_from_file(char *fname, gv_metadata_t *metadata)
{
  /* Read some of metadata fields from file.
   * File should have this format:
   *    line 1: input files separated by space
   *    line 2: input files' generated date separated by space
   *    line 3: program's runtime options.
   *
   * Return 1 upon successful; -1, otherwise.
   * Copied from 'level_2_singlevos_hdf_to_ghdf.c'.
   */

  FILE *fp;
  long_name_t input_files;
  name_t      input_files_gen_dates;
  very_long_name_t      runtime_opts;
  char *rc;
  int len;

  if (fname == NULL || metadata == NULL) return -1;
  
  memset(runtime_opts, '\0', sizeof(very_long_name_t));
  memset(input_files_gen_dates, '\0', sizeof(name_t));
  memset(input_files, '\0', sizeof(long_name_t));

  if ((fp = fopen(fname, "r")) == NULL) return -1;
  rc = fgets(input_files, sizeof(long_name_t), fp);
  if (rc == NULL) goto FAILURE;
  rc = fgets(input_files_gen_dates, sizeof(name_t), fp);
  if (rc == NULL) goto FAILURE;
  rc = fgets(runtime_opts, sizeof(very_long_name_t), fp);
  if (rc == NULL) goto FAILURE;
  fclose(fp);

  /* Remove \n from strings */
  len = strlen(input_files);
  if (len < 1) return -1;
  if (input_files[len-1] == '\n') 
	input_files[len-1] = '\0';

  len = strlen(input_files_gen_dates);
  if (len < 1) return -1;
  if (input_files_gen_dates[len-1] == '\n') 
	input_files_gen_dates[len-1] = '\0';

  len = strlen(runtime_opts);
  if (len < 1) return -1;
  if (runtime_opts[len-1] == '\n') 
	runtime_opts[len-1] = '\0';

  strcpy(metadata->input_files, input_files);
  strcpy(metadata->gen_date_input_files, input_files_gen_dates);
  strcpy(metadata->runtime_opt, runtime_opts);

  return 1;

FAILURE:
  if (fp) fclose(fp);
  return -1;
} /* read_prod_specific_metadata_from_file */

/***************************************************************************/
/*                                                                         */
/*                                 write_metadata                          */
/*                                                                         */
/***************************************************************************/
int write_metadata(char *qc_filename, char *outfile, char *metadata_file,
				   char *param_file, char *anomaly_flag,
				   float xdim, float ydim, float zdim,
				   float xres, float yres, float zres, float min_val,
				   float max_val, float alt,
				   char *alg_id, char *data_id, char *alg_version, 
                   char *software_version, char *contact,
				   IO_HANDLE *outfile_fd)
{
  gv_metadata_t metadata;
  IO_HANDLE qc_fd;
  float lat, lon;
  char site[MAX_NAME_LEN];
  DATE_STR date;
  TIME_STR time;

  if (qc_filename == NULL || outfile_fd == NULL || metadata_file == NULL) 
	return -1;

  memset(&metadata, '\0', sizeof(gv_metadata_t));

  /* Read generic metadata from the qc hdf file */
  if (verbose)
	fprintf(stderr, "Reading generic metadata from qchdf... %s\n", qc_filename);
  memset(&qc_fd, '\0', sizeof(IO_HANDLE));

  if (TKopen(qc_filename, TK_L1C_GV, TK_READ_ONLY, &qc_fd) != TK_SUCCESS) {
	if (verbose) fprintf(stderr, "Failed to open qc file <%s>\n", qc_filename);
	return -1;
  }

  if (TKreadMetadataInt(&qc_fd, TK_BEGIN_DATE, &date) ==
	  TK_FAIL) {
	TKclose(&qc_fd);
	return -1;
  }

  if (TKreadMetadataInt(&qc_fd, TK_BEGIN_TIME, &time) ==
	  TK_FAIL) {
	TKclose(&qc_fd);
	return -1;
  }

  if (read_hdf_metadata_from_qc(&qc_fd, &metadata, verbose) < 0) {
	fprintf(stderr, "Warning: Failed reading generic metadata from qc hdf file <%s>\n",  qc_filename);
  }

  if (anomaly_flag == NULL) {
	if (TKreadMetadataChar(&qc_fd, TK_ANOMALY_FLAG, metadata.anomaly_flag) ==
		TK_FAIL) {
	  TKclose(&qc_fd);
	  return -1;
	}
  }
  else 
	strcpy(metadata.anomaly_flag, anomaly_flag);

  TKclose(&qc_fd);

  /* Read lat,lon from file. */
  if (gv_utils_get_grid_origin_lat_lon(metadata.radar_name, 
										GV_UTILS_SINGLE_RADAR, &lat, &lon) < 0) {
	fprintf(stderr, "Failed to read data from lat lon file.\n");
	return -1;
  }


  if (set_l2_specific_metadata(&date, &time, 
							   xdim, ydim, zdim,
							   xres, yres,  zres , lat, lon, alt,
							   metadata_file, min_val, max_val,
							   alg_id, alg_version, software_version,
							   contact, data_id,&metadata) < 0) {
	  fprintf(stderr, "Failed to set l2 specific metadata.\n");
	  return(-1);
  }

  /* The begin date/time is when the granule (hour) begins.
   * The end date/time is when the granule (hour) ends.
   */
 
  memcpy(&(metadata.begin_time), &time, sizeof(TIME_STR));
  memcpy(&(metadata.begin_date), &date, sizeof(DATE_STR));

  /* Dates/Times  will be modified. */
  set_granule_dateNtime(&(metadata.begin_date), &(metadata.begin_time),
						&(metadata.end_date), &(metadata.end_time));

  /* These fields are unknown so use default */
  metadata.num_data_gap = 0;
  metadata.data_gap = 0;
  strcpy(metadata.percent_bad_miss_pixel, "");
  metadata.leap_secs_flag = 0;       

  if (param_file != NULL) {
	if (verbose)
	  fprintf(stderr, "Reading prod specific metadata from file... %s\n", param_file);
	if (read_prod_specific_metadata_from_file(param_file, &metadata) < 0) {
	fprintf(stderr, "Warning: Failed reading product specific metadata from metadata file<%s>\n",  param_file);
	}
  }
  if (strrchr(outfile, '/') != NULL)
	strcpy(metadata.granule_id, strrchr(outfile, '/') + 1);
  else
	strcpy(metadata.granule_id, outfile);

  metadata.num_vos = 0;

  /* These fields are unknown so use default */
  metadata.num_data_gap = 0;
  metadata.data_gap = 0;
  strcpy(metadata.percent_bad_miss_pixel, "");
  metadata.leap_secs_flag = 0;       

  if (verbose)
	fprintf(stderr, "calling write_hdf_metadata_for_l2granule...\n");
  /* Write metadata to granuled hdf file */
  if (write_hdf_metadata_for_l2granule(outfile_fd, &metadata, verbose) < 0) {
	fprintf(stderr, "Warning: Failed writing metadata to file.\n");
  }
  return 1;
} /* write_metadata */

/***************************************************************************/
/*                                                                         */
/*                              process_argvs                              */
/*                                                                         */
/***************************************************************************/
void process_argvs(int argc, char **argv, char **outfile, char **qcfile,
				   char **anomaly_flag,
				  char **metadata_file, char **param_file, char *product_code)
{
  
  char *tmp_str;
  int c, i;

  if (product_code == NULL || outfile == NULL ||qcfile == NULL) exit(-1);
  if (argc  < 4) {
 USAGE:
	fprintf(stderr, "Usage (%s):\n", PROG_VERSION);
	fprintf(stderr, "  %s [-v] [-a anomaly_flag_msg]\n",argv[0]);
	fprintf(stderr, "\t[-p parameter_file] [-m metadata_file] {3 | 4 | 5} \n");
	fprintf(stderr, "\tqc_hdf_file outfile_hdf\n");
	fprintf(stderr, "\n where:\n");
	fprintf(stderr, "\t-a: Specify the reason why a granule is empty. The possible values are:\n"
                    "\t    'EMPTY: GENERATED AFTER SOFTWARE ERROR'\n"
			        "\t    'EMPTY: NO DATA DUE TO NO RAIN.'\n"
                    "\t    'EMPTY: NO DATA  RECORDED.'\n"
                    "\t    'EMPTY: DATA RECORED BUT STILL MISSING'\n"
                    "\t    'EMPTY: REASON UNKNOWN'\n"
			        "\t    Default: Obtain from qc_hdf_file.\n");
	fprintf(stderr, "\t-p: Specify filename that contains input file names,\n"
                    "\t    generated dates for input files, and product's runtime \n"
			        "\t    options on each line.  Each item on a line is separated by \n"
	                "\t    space(s).  Default: \"\".\n");
    fprintf(stderr, "\t-m: Specify filename for metadata file. Default:$GVS_DATA_PATH/<prod>_metadata.txt\n"
                    "\t    where <prod> = 2A53|2A54|2A55\n");
	fprintf(stderr, "\t3 = 2A-53; 4 = 2A=54; 5 = 2A-55\n");
	exit(-1);
  }

  while ((c = getopt(argc, argv, "p:m:a:v")) != -1) {
	switch(c) {
	case 'v':
	  verbose = 1;  break;
	case 'a':
	  if (optarg[0] == '-') goto USAGE;
	  *anomaly_flag = (char *) strdup(optarg);
	  break;
	case 'p':
	  if (optarg[0] == '-') goto USAGE;
	  *param_file = (char *) strdup(optarg);
	  break;
	case 'm':
	  if (optarg[0] == '-') goto USAGE;
	  *metadata_file = (char *) strdup(optarg);
	  break;
	case '?': fprintf(stderr, "option -%c is undefined\n", optopt);
	  goto USAGE;
    case ':': fprintf(stderr, "option -%c requires an argument\n",optopt);
	  goto USAGE;
    default: break;
    }
  }
  /* must have at least 3 items listed */
  if (argc - optind < 3) goto USAGE;
  *product_code =  argv[optind++][0];
  switch(*product_code) {
  case '3':
  case '4':
  case '5':
	break;
  default:
	goto USAGE;
  }
  *qcfile = (char *) strdup(argv[optind++]);
  *outfile = (char *) strdup(argv[optind++]);
} /* process_argvs */ 


/***************************************************************************/
/*                                                                         */
/*                                   main                                  */
/*                                                                         */
/***************************************************************************/
int main(int argc, char **argv)
{
  char pcode;
  IO_HANDLE outfile_fd;
  int mode;
  char *outfile = NULL, *qcfile=NULL, *param_file = NULL, 
	*metadata_file = NULL, *anomaly_flag = NULL;
  float xres, yres, zres, min_val, max_val;
  int xdim, ydim, zdim;
  char *path = NULL;
  char tmp_metadata_file[MAX_FILENAME_LEN];
  float alt;
  char *alg_id=NULL, *alg_version=NULL, *data_id=NULL, *contact = NULL;
  char *software_version=NULL;
  set_signal_handlers();

  process_argvs(argc, argv, &outfile, &qcfile, &anomaly_flag, &metadata_file, &param_file, 
				&pcode);


  path = getenv("GVS_DATA_PATH");
  if (path == NULL) 
	path = "/usr/local/trmm/GVBOX/data";

  switch (pcode) {
	case '3':  /* 2A-53 */
	  xdim = P2A53_XDIM;
	  ydim = P2A53_YDIM;
	  zdim = P2A53_ZDIM;
	  xres = P2A53_XRES;
	  yres = P2A53_YRES;
	  zres = P2A53_ZRES;
	  alt = P2A53_ALT;
	  min_val = P2A53_MIN_UNIT;
	  max_val = P2A53_MAX_UNIT;
	  alg_id = P2A53_ALG_ID;
	  data_id = P2A53_DATA_ID;
	  alg_version = P2A53_ALG_VERSION;
	  software_version = P2A53_SOFTWARE_VERSION;
	  contact = P2A53_CONTACT_NAME;

	  if (metadata_file == NULL) {
		sprintf(tmp_metadata_file, "%s/2A53_metadata.txt", path);
		metadata_file = tmp_metadata_file;
	  }
	  break;
	
	case '4':  /* 2A-54 */
	  xdim = P2A54_XYDIM;
	  ydim = P2A54_XYDIM;
	  zdim = P2A54_ZDIM;
	  xres = P2A54_XYRES;
	  yres = P2A54_XYRES;
	  zres = P2A54_ZRES;
	  alt = P2A54_ALT;
	  min_val = P2A54_MIN_UNIT;
	  max_val = P2A54_MAX_UNIT;
	  alg_id = P2A54_ALG_ID;
	  data_id = P2A54_DATA_ID;
	  alg_version = P2A54_ALG_VERSION;
	  software_version = P2A54_SOFTWARE_VERSION;
	  contact = P2A54_CONTACT_NAME;

	  if (metadata_file == NULL) {
		sprintf(tmp_metadata_file, "%s/2A54_metadata.txt", path);
		metadata_file = tmp_metadata_file;
	  }
	  break;
	case '5':  /* 2A-55 */
	  xdim = P2A55_XYDIM;
	  ydim = P2A55_XYDIM;
	  zdim = P2A55_ZDIM;
	  xres = P2A55_XYRES;
	  yres = P2A55_XYRES;
	  zres = P2A55_ZRES;
	  alt = P2A55_ALT;
	  min_val = P2A55_MIN_UNIT;
	  max_val = P2A55_MAX_UNIT;
	  alg_id = P2A55_ALG_ID;
	  data_id = P2A55_DATA_ID;
	  alg_version = P2A55_ALG_VERSION;
	  software_version = P2A55_SOFTWARE_VERSION;
	  contact = P2A55_CONTACT_NAME;
	  

	  if (metadata_file == NULL) {
		sprintf(tmp_metadata_file, "%s/2A55_metadata.txt", path);
		metadata_file = tmp_metadata_file;
	  }
	  break;
  default:
	exit(-1);
  }
  if (open_hdf_file(outfile, TK_NEW_FILE, pcode, &outfile_fd) < 0) {
	if (verbose) 
	  fprintf(stderr, "Failed to create hdf file <%s>.\n", outfile);
	exit(-1);
  }

  if (write_metadata(qcfile, outfile, metadata_file, param_file, anomaly_flag,
					 xdim, ydim, 
					 zdim, xres, yres, zres, min_val,
					 max_val, alt, alg_id, data_id, alg_version,
                     software_version, contact,
					 &outfile_fd) < 0) {
	fprintf(stderr, "Failed to write metadata.\n");
	TKclose(&outfile_fd);
	remove (outfile); 
	exit(-1);
  }
  if (TKclose(&outfile_fd) != TK_SUCCESS)
	exit(-1);

  if (verbose)
	fprintf(stderr, "Successful.\n");
  exit(0);
} /* main */

static void handler(int sig)
{
  fprintf(stderr, "Got signal %d. Abort.\n", sig);
  if (sig == SIGINT || sig == SIGKILL || sig == SIGSTOP) {
	exit(-2);
  }
  exit(-1);
}
