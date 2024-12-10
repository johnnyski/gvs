
#include <ctype.h>
#include <signal.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>                       /* standard library functions      */
#include <volume.h>                       /* general header for volume       */


// TN 051396--added.
extern "C"
{
  #include <gv_utils.h>
  #include "gvs_metadata.h"
  #include "2A54.h"
  #include "exitCodes.h"
  int write_csmap2hdf(int csmap[151][151],  int year, int month, int day,
					  int hour, int min, int sec, 
					  int xdim, int ydim,
					  float xres, float yres,
					  float lat, float lon, float alt,
					  char *metadata_fname,
					  char *hdf_filename, int verbose);
}

#define MAX_XYDIM P2A54_XYDIM 
#define MAX_FILENAME_LEN 256


int doCS(const float g[151][151], int *csmap, int xmax, int ymax, float xy_res)
{
  // Create the volume and set some parameters.
  Volume v;
  v.params.xydim = MAX_XYDIM;
  v.params.xyres = xy_res;
  v.params.csAlgorithm = 0;
  sprintf(v.params.outputConvStraMapFn, "csmap");
  FILE *fp = fopen("logfile", "a");
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
  char site[30];
  char output_path[100];
  float clat, clon;
  char *output_filename;    // TN 041096
  extern int optind;
  extern char *optarg;
  extern int optopt;
  char *metadata_fname = NULL;
  char *path = NULL;
  char tmpname[MAX_FILENAME_LEN];

  float xy_res = P2A54_XYRES;        // Horizontal resolution in KM.

  if (argc < 5) {
  USAGE:
     fprintf(stdout, "Usage (%s): %s [-v] [-H horizontal_resolution]\n"
		             "               [-m metadata_fname] \n"
                     "               working_map_filename site output_path output_filename\n",
	PROG_VERSION, argv[0]);
	 fprintf(stderr, "       -v             Verbose diagnostics during execution.\n");
	 fprintf(stderr, "       -H             Specify horizontal resolution in km (2.0 or 4.0) . Default: 2.0.\n");
	 fprintf(stderr, "       -m             Specify filename for metadata file.\n");
     fprintf(stderr, "                      Default:$GVS_DATA_PATH/2A54_metadata.txt\n");

	 TKreportWarning(W_2A54_BAD_USAGE);
     exit(FAILED_CODE4);
  }
  int c;
  int verbose = 0;

  signal(SIGINT, abort_handler);
  signal(SIGFPE, handler);
  signal(SIGKILL, abort_handler);
  signal(SIGSTOP, abort_handler);
  signal(SIGILL, handler);
  signal(SIGSEGV, handler);

  while ((c = getopt(argc, argv, ":m:H:v")) != -1) {
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
	case '?': fprintf(stderr, "option -%c is undefined\n", optopt);
	  goto USAGE;
    case ':': fprintf(stderr, "option -%c requires an argument\n",optopt);
	  goto USAGE;
    default: break;
    }
  }
  /* must have 4 files listed */
  if (argc - optind != 4) goto USAGE;
  char *fn =  argv[optind++];
  // read in site name 
  strcpy(site, argv[optind++]);

  // read in output path
  strcpy(output_path,argv[optind++]);

  // get output filename   --  TN 041096
  output_filename = argv[optind++];

  /* Set default */
  if (metadata_fname == NULL) {
	path = getenv("GVS_DATA_PATH");
	if (path == NULL)
	  path = "/usr/local/trmm/GVBOX";
	memset(tmpname, '\0', MAX_FILENAME_LEN);
	sprintf(tmpname, "%s/2A54_metadata.txt", path);
	metadata_fname = tmpname;
  }

//  int xdim = 151;
//  int ydim = 151;
  // Read lat/lon from $GVS_DATA_PATH/gv_radar_site_info.data
  if (gv_utils_get_grid_origin_lat_lon(site, GV_UTILS_SINGLE_RADAR, &clat, &clon) < 0) {
	fprintf(stderr, "Failed to read lat/lon from file with site = <%s>\n", site);
	TKreportWarning(W_2A54_INVALID_SITE);
	exit(FAILED_CODE4);
  }
  // Open input mudras working map
  FILE *fp = fopen(fn, "r");
  if (!fp)
    {
      fprintf(stderr, "Can't open file %s.\n", fn);
	  TKreportWarning(W_2A54_OPEN_MUDRAS_FILE_FAILED);
      exit(FAILED_CODE4);
    }
  
  float grid[151][151];

  //Read the single-level volume
  MudFile m;
  if (m.readPureCedricVolume(fp) <= 0) exit(FAILED_CODE4);
  if (m.getField(0, (float *)grid, 151, 151, 1, -99.0) <= 0)
	exit(FAILED_CODE4);
  fclose(fp);

  int year = m.header.VolStart.Year;
  int month = m.header.VolStart.Month;
  int day = m.header.VolStart.Day;
  int hour = m.header.VolStart.Hour;
  int minute = m.header.VolStart.Minute;
  int second = m.header.VolStart.Second;

  // Create the convective/stratiform map.
  int csmap[151][151];
  char csmapfn[256];
  
  // TN 041096
  if (strchr(output_filename, '/') == NULL) 
	// Apply output path if the specified output filename doesnot include path.
	sprintf(csmapfn, "%s/%s", output_path, output_filename);
  else
	strcpy(csmapfn, output_filename);

  if (doCS(grid, (int *) csmap, MAX_XYDIM, MAX_XYDIM, xy_res) > 0) {
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

