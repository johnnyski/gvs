#include <ctype.h>
#include <signal.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>                       /* standard library functions      */
#include <volume.h>                       /* general header for volume       */

#define MAX_FILENAME_LEN 256
#define MAX_SITENAME_LEN 30
#define MAX_PATH_LEN 100
#define MAX_XYDIM P2A54_XYDIM 

#define INNER_GRID_CAPPI 0
#define OUTER_GRID_CAPPI 1
#define THRESHOLD_RADIUS 50

// TN 051396--added.
extern "C"
{
  #include <IO.h>
  #include <IO_GV.h>
  #include <gv_utils.h>
  #include "gvs_metadata.h"
  #include "2A54.h"
  #include "exitCodes.h"
  int write_csmap2hdf(int csmap[MAX_XYDIM][MAX_XYDIM],  int year, int month, int day,
					  int hour, int min, int sec, 
					  int xdim, int ydim,
					  float xres, float yres,
					  float lat, float lon, float alt,
					  char *metadata_fname,
					  char *hdf_filename, int verbose);
}

/*
 * Dual-cappi 2A-54 Algorithm
 *   Modified from Matteus Steiner's 2A-54 algorithm.
 * 
 * Input is 2A-55 hdf. No longer uses 1C-51 as input.  No call to sprint.
 *
 * 2A-54 uses as input the first cappi (1.5km) from 2A-55 for the
 * inner disk and the second cappi (3 km) for the outer disk.  The
 * outer disk is every pixel beyond THRESHOLD_RADIUS pixels from the
 * center.
 *
 * By:
 *  Galen J. Wilkerson
 *  Science Systems and Applications, Inc.
 *  Lanham, MD
 *  Galen.Wilkerson@gsfc.nasa.gov
 *  6/28/2000 
 * 
 */

int verbose = 0;

/***********************************************************************/
/*                                                                     */
/*                            doCS                                     */
/*                                                                     */
/***********************************************************************/

int doCS(char* log_fname, const float g[MAX_XYDIM][MAX_XYDIM], int *csmap, int xmax, int ymax, float xy_res)
{
  // Create the volume and set some parameters.
  Volume v;
  v.params.xydim = MAX_XYDIM;
  v.params.xyres = xy_res;
  v.params.csAlgorithm = 0;
  sprintf(v.params.outputConvStraMapFn, "csmap");
  FILE *fp;
  if(log_fname != NULL) 
    fp = fopen(log_fname, "a");
  else 
    fp = fopen("/dev/null", "a");
  if (!fp) return(1);
  v.params.fpout1 = fp;
    
  // Create the linear map.
  v.params.xydim = MAX_XYDIM;
  {
    for (int i = 0; i < v.params.xydim; i++)
      {
	for (int j = 0; j < v.params.xydim; j++)
	  {
	    v.grids.range_mask[i][j] = 1;
	    v.grids.reflmap[i][j] = g[i][j];
	    v.grids.reflinmap[i][j] = pow(10, 0.1 * g[i][j]);
	  }
      }
  }
  //v.params.min_range = 15.0;
  v.params.min_range = 13.0;
  v.params.max_range = 150.0;

  v.PREPvalid_range();
  v.ANALconv_stra();

#ifdef CCC
  if(log_fname != NULL) 
    fclose(fp);
#endif

  // create csmap 
  {

	for (int i = 0; i < v.params.xydim && i < xmax; i++)
	  {
		for (int j = 0; j < v.params.xydim && j < ymax; j++)
		  {
			csmap[j + xmax * i] = v.grids.rain_type[i][j];
		  }
	  }
  }

  return(0);
}



/***********************************************************************/
/*                                                                     */
/*                 read_data_from_2A_55_hdf_file                       */
/*                                                                     */
/***********************************************************************/

int read_data_from_2A55_hdf_file(char *hdffile, L2A_55_SINGLE_RADARGRID *grid)
{
  int status;
  IO_HANDLE hdf_fd;

  if (grid == NULL || hdffile == NULL) return -1;

  memset(&hdf_fd, '\0', sizeof(IO_HANDLE));
  /* open hdf file */
  status = TKopen(hdffile, TK_L2A_55S, TK_READ_ONLY, &hdf_fd);
  
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
  return 0;
}/* read_data_from_2A55_hdf_file */

/***********************************************************************/
/*                                                                     */
/*                    get_info_from_2A_55_hdf_file                     */
/*                                                                     */
/***********************************************************************/

int get_info_from_2A_55_hdf_file(char *hdf_fname, char *site_name,
						  int *mm, int *dd, int *yy)
{
  int status;
  DATE_STR date;
  int data_type = TK_L2A_55S;
  IO_HANDLE fh;

  memset(&fh, '\0', sizeof(IO_HANDLE));
  
  status = TKopen(hdf_fname, data_type, TK_READ_ONLY, &fh);
  if (status != TK_SUCCESS) {
    TKclose(&fh);
    return -1;
  }

  TKreadMetadataChar(&fh, TK_RADAR_NAME, site_name);

  TKreadMetadataInt(&fh, TK_END_DATE, &date);

  *mm = date.tkmonth;
  *dd = date.tkday;
  *yy = date.tkyear;

  TKclose(&fh);

  return 0;
} /*get_info_from_2A_55_hdf_file */


/***********************************************************************/
/*                                                                     */
/*                    read_gridded_reflectivity                        */
/*                                                                     */
/***********************************************************************/

/* 
 *Read the gridded reflectivity -
 *      The inner disk - radius 100 km is from 2A-55 cappi at 1.5 km
 *      the outer disk - 100 to 150 km is from 2A-55 cappi at 3 km
 */
int read_gridded_reflectivity(char *l2A_55_filename, float grid[MAX_XYDIM][MAX_XYDIM], int *HH, int *MM, int *SS) {

  L2A_55_SINGLE_RADARGRID l2A55Grid;
  TIME_STR tktime;

  int i, j;
  float temp;

  if (l2A_55_filename == NULL) 
    return (-1);

  memset(&l2A55Grid, '\0', sizeof(L2A_55_SINGLE_RADARGRID));

  if (read_data_from_2A55_hdf_file(l2A_55_filename,&l2A55Grid) < 0) 
    {
    fprintf(stderr, "Failed to read data from file <%s>\n", l2A_55_filename);

    /*
     * TK Error
     */

    return -1;
  }

  /* get the time from the radar grid */  
  tktime = l2A55Grid.tktime;
  *HH = tktime.tkhour;
  *MM = tktime.tkminute;
  *SS = tktime.tksecond;

  if (verbose) {
    fprintf(stderr,"hour: %d, min: %d, sec: %d\n",&HH,&MM,&SS);
  }

  /* 
   * use cappi 0 for the inner THRESHOLD_RADIUS pixels
   * use cappi 1 for the outer THRESHOLD_RADIUS to MAX_XYDIM pixels
   */

  if(verbose) {
    fprintf(stderr, "center: %d , thresh: %d\n", MAX_XYDIM/2, THRESHOLD_RADIUS);
    fprintf(stderr, "inner CAPPI: %d, outer CAPPI: %d\n", INNER_GRID_CAPPI, OUTER_GRID_CAPPI);
  }

  for (i = 0; i < MAX_XYDIM; i++) {
    for ( j = 0; j < MAX_XYDIM; j++) {

      if ((pow(i - MAX_XYDIM/2, 2) + pow(j - MAX_XYDIM/2, 2)) 
	  < THRESHOLD_RADIUS * THRESHOLD_RADIUS) {
	temp = l2A55Grid.threeDreflect[INNER_GRID_CAPPI][i][j];
      }
      else {
	temp = l2A55Grid.threeDreflect[OUTER_GRID_CAPPI][i][j];
      }
	if (temp > TK_DEFAULT) 
	  grid[i][j] = temp;
	else 
	  grid[i][j] = MISSINGV;
    }
  }
  return 0;
} /* read_gridded_reflectivity */

#ifdef SGI53
static void handler(...)
#else
static void handler(int sig)
#endif

{
#ifdef SGI53
  fprintf(stderr, "Got signal. Abort.\n");
#else
  fprintf(stderr, "Got signal %d. Abort.\n", sig);
#endif
  kill(0, SIGUSR1);
  TKreportWarning(W_2A54_STOP_ON_ERROR);
  exit(FAILED_CODE4);
}

#ifdef SGI53
void abort_handler(...)
#else
void abort_handler(int sig)
#endif

{
#ifdef SGI53
  fprintf(stderr, "Interrupted. Abort.\n");
#else
  fprintf(stderr, "Got signal %d. Abort.\n", sig);
#endif
  kill(0, SIGINT);
  TKreportWarning(W_2A54_ABORT_EXEC);
  exit(INTER_CODE);
}


main(int argc, char **argv)
{
  // variable declarations
  char site[MAX_SITENAME_LEN];
  float clat, clon;
  char *output_filename;    // TN 041096
  extern int optind;
  extern char *optarg;
  extern int optopt;
  char *metadata_fname = NULL;
  char *path = NULL;
  char tmpname[MAX_FILENAME_LEN];
  char tmpname2[MAX_FILENAME_LEN];

  float xy_res = P2A54_XYRES;        // Horizontal resolution in KM.

  float grid[MAX_XYDIM][MAX_XYDIM];

  // Create the convective/stratiform map.
  int csmap[MAX_XYDIM][MAX_XYDIM];
  char csmapfn[MAX_FILENAME_LEN];

  char *log_fname = NULL;

  char *output_path = NULL;

  int year;
  int month;
  int day;
  int hour;
  int minute;
  int second;

  if (argc < 3) {
  USAGE:
     fprintf(stdout, "Usage (%s): \n\t%s [-v] [-H horizontal_resolution]\n"
	     "               [-m metadata_fname] [-l result_log_filename] [-D output_dir] \n"
	     "               2A-55_shdf_filename csmap_shdf_file\n",
	PROG_VERSION, argv[0]);
	 fprintf(stderr, "       -v  Verbose diagnostics during execution.\n");
	 fprintf(stderr, "       -H  Specify horizontal resolution in km (2.0 or 4.0) . Default: 2.0.\n");
	 fprintf(stderr, "       -m  Specify filename for metadata file.\n");
	 fprintf(stderr, "            Default:$GVS_DATA_PATH/2A54_metadata.txt\n");
	 fprintf(stderr, "       -l  Specify result log filename to be saved.\n");
	 fprintf(stderr, "       -D  Specify output path.\n");

	 TKreportWarning(W_2A54_BAD_USAGE);
     exit(FAILED_CODE4);
  }
  int c;

  signal(SIGINT, abort_handler);
  signal(SIGFPE, handler);
  signal(SIGKILL, abort_handler);
  signal(SIGSTOP, abort_handler);
  signal(SIGILL, handler);
  signal(SIGSEGV, handler);

  while ((c = getopt(argc, argv, ":m:l:D:H:v")) != -1) {
	switch(c) {
	case 'v':
	  verbose = 1;
	  break;
	case 'H':
	  if (!isdigit(optarg[0])) {
		fprintf(stderr, "Error: Resolution must be a number.\n");
		TKreportWarning(W_2A54_BAD_USAGE);
		exit(FAILED_CODE4);
	  }
	  sscanf(optarg, "%f", &xy_res);
	  break;
	case 'm':
	  metadata_fname = (char *) strdup(optarg); 
	  break;
	case 'l':
	  log_fname = (char *) strdup(optarg);
	  break;
	case 'D':
	  output_path = (char *) strdup(optarg);
	  break;
	case '?': fprintf(stderr, "option -%c is undefined\n", optopt);
	  goto USAGE;
    case ':': fprintf(stderr, "option -%c requires an argument\n",optopt);
	  goto USAGE;
    default: break;
    }
  }
  // must have 2 files listed 
  if (argc - optind != 2) goto USAGE;
  char *l2A_55_filename =  argv[optind++];

  // fix output path
  if (output_path == NULL) 
    output_path = (char *) strdup("./");


  // get output filename   --  TN 041096
  output_filename = argv[optind++];

  // Set default
  if (metadata_fname == NULL) {
	path = getenv("GVS_DATA_PATH");
	if (path == NULL)
	  path = "/usr/local/trmm/GVBOX";
	memset(tmpname, '\0', MAX_FILENAME_LEN);
	sprintf(tmpname, "%s/2A54_metadata.txt", path);
	metadata_fname = tmpname;
  }

  // get the sitename and date
  if (get_info_from_2A_55_hdf_file(l2A_55_filename,site,&month,&day,&year) < 0) {
    fprintf(stderr, "Failed to read sitename, month, day, year from <%s>\n",l2A_55_filename);

     // TK warning here.

    exit(-1);
  }

  // read the 2A-55 radar grid
  if (read_gridded_reflectivity(l2A_55_filename, grid, &hour, &minute, &second) < 0) {
    fprintf(stderr, "Failed to read grid from file <%s>\n",l2A_55_filename);

    // TK warning here.

    exit(-1);
  }

  // Read lat/lon from $GVS_DATA_PATH/gv_radar_site_info.data
  if (gv_utils_get_grid_origin_lat_lon(site, GV_UTILS_SINGLE_RADAR, &clat, &clon) < 0) {
    fprintf(stderr, "Failed to read lat/lon from file with site = <%s>\n", site);
    TKreportWarning(W_2A54_INVALID_SITE);
    exit(FAILED_CODE4);
  }


  // TN 041096
  if (strchr(output_filename, '/') == NULL) 
	// Apply output path if the specified output filename doesnot include path.
	sprintf(csmapfn, "%s/%s", output_path, output_filename);
  else
	strcpy(csmapfn, output_filename);

  if (doCS(log_fname,grid, (int *) csmap, MAX_XYDIM, MAX_XYDIM, xy_res) > 0) {
	TKreportWarning(W_2A54_DOCS_FAILED);
	exit (FAILED_CODE4);
  }

  if (verbose)
	fprintf(stderr, "lat/lon: %f/%f\n", clat, clon);


  if (write_csmap2hdf(csmap, year, month, day, hour, minute, second, 
                      MAX_XYDIM, MAX_XYDIM,
					  xy_res, xy_res,
					  clat, clon, P2A54_ALT,
					  metadata_fname,
					  csmapfn, verbose) < 0) {
	fprintf(stderr, "Failed to write csmap to hdf file <%s>\n", csmapfn);
	TKreportWarning(W_2A54_CSMAP2HDF_FAILED);
	exit(FAILED_CODE4);
  }

  exit(0);
}
