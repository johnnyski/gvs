

#include <signal.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>                       /* standard library functions      */
#include <volume.h>                       /* general header for volume       */
extern "C" {
  #include "exitCodes.h"
  #include <gv_utils.h>

  extern int strncasecmp(const char *s1, const char *s2, size_t n);

}

#define MAX_FILENAME_LEN 256

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
  TKreportWarning(W_2A55_STOP_ON_ERROR);
  exit(FAILED_CODE4);
}

#ifdef SGI53
void abort_handler(...)
#else
void abort_handler(int sig)
#endif
{
#ifdef SGI53
  fprintf(stderr, "Got signal. Abort.\n");
#else
  fprintf(stderr, "Got signal %d. Abort.\n", sig);
#endif
  kill(0, SIGINT);
  TKreportWarning(W_2A55_ABORT_EXEC);
  exit(INTER_CODE);
}

int main(int argc, char **argv)
{
  // variable declarations
  Volume v;
  int i, j;
  char fn_csmap[100];
  char *metadata_fname = NULL;
  char tmpname[MAX_FILENAME_LEN];
  int c;
  char *path = NULL;
  int verbose = 0;
  int no_csmap = 0;

  signal(SIGINT, abort_handler);
  signal(SIGFPE, handler);
  signal(SIGKILL, abort_handler);
  signal(SIGSTOP, abort_handler);
  signal(SIGILL, handler);
  signal(SIGSEGV, handler);


  // check for correct number of arguments
  if (argc < 5) {
  USAGE:
     fprintf(stderr, "Usage (%s): \n"
			 "%s [-v] [-n] \n"
			 "   interp_volume_filename  [csmap_filename] \n", PROG_VERSION, argv[0]);
     fprintf(stderr, "\tgeographical_mask_filename site output_hdf_file\n");
     fprintf(stderr, "\nNote: if [csmap_filename] is not passed as a command-line argument\n");
     fprintf(stderr, "a 2A55_shdf file is created containing only 3d reflectivity \n");
     fprintf(stderr, "and no CFAD or VPROF\n");
	 TKreportWarning(W_2A55_BAD_USAGE);
     exit(FAILED_CODE4);
  }
 while ((c = getopt(argc, argv, ":m:vn")) != -1) {
	switch(c) {
	case 'v':
	  verbose = 1;
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
  /* must have 4 or 5 files listed */
  if ((argc - optind != 4) && (argc - optind != 5)) goto USAGE;
    ;
  if (argc - optind == 4) 
    no_csmap = 1;

  // read in input filenames
  strcpy(v.params.inputVolumeFn,argv[optind++]);     // interp volume filename
  if(! no_csmap) 
    strcpy(fn_csmap, argv[optind++]);                   // csmap filename
  strcpy(v.params.inputGeoMapFn, argv[optind++]);     // geographical mask filename
 
  // read in site name 
  strcpy(v.params.siteName, argv[optind++]);

  // read in output HDF filename.
  strcpy(v.params.outputPath, argv[optind]); // Compatability,
  strcpy(v.params.outputHDFfile, argv[optind++]); // Used this instead.

  /* Set default */
  if (metadata_fname == NULL) {
	path = getenv("GVS_DATA_PATH");
	if (path == NULL)
	  path = "/usr/local/trmm/GVBOX";
	memset(tmpname, '\0', MAX_FILENAME_LEN);
	sprintf(tmpname, "%s/2A55_metadata.txt", path);
	metadata_fname = tmpname;
  } 
  if (verbose) {
	fprintf(stdout, "Check inputs:\n");
	fprintf(stdout, "   Interp volume = %s.\n", v.params.inputVolumeFn);
	fprintf(stdout, "   Threshold fn  = %s.\n", v.params.inputThresholdFn);
	if(! no_csmap) 
	  fprintf(stdout, "   csmap         = %s.\n", fn_csmap);
	fprintf(stdout, "   geo mask      = %s.\n", v.params.inputGeoMapFn);
	fprintf(stdout, "   site name     = %s.\n", v.params.siteName);
	fprintf(stdout, "   output file   = %s.\n", v.params.outputHDFfile);
  }
  // initialize some items

  strcpy(v.params.inputThresholdFn,"-");  // only variable for aiw1

  v.params.radarFormat = 3;          // radar input file format
				     //    0 = CSU Mudras
				     //    1 = CEDRIC tape (DM)
				     //    2 = CEDRIC disk (NM)
				     //    3 = Pure CEDRIC format 
  v.params.nVolume = 0;              // ???
  v.params.ASCIIformat = 1;          // format to use for writing results
                                     //    0 = Bell format
                                     //    1 = ASCII format
  v.params.level = 1;                // need to get rid of anvil check


  FILE *fp = fopen("logfile", "a");
  if (!fp) {
	TKreportWarning(W_2A55_OPEN_LOGFILE_FAILED);
	exit(FAILED_CODE4);
  }
  v.params.fpout1 = fp;

  // Read radar volume
  if (v.readRadarVolume(0) != 0) {
     fprintf(stderr, "Sorry, can't read input file %s\n", 
          v.params.inputVolumeFn);
	 TKreportWarning(W_2A55_READ_RADAR_VOL_FAILED);
     exit(FAILED_CODE4);
  }
  /* Want to set lat/lon, etc.. after calling readRadarVolume() since
   * that function read lat/lon from the mudras file which doesnot
   * contain lat/lon info.
   */
  // Read lat/lon from $GVS_DATA_PATH/gv_radar_site_info.data
  if (gv_utils_get_grid_origin_lat_lon(v.params.siteName, 
									   GV_UTILS_SINGLE_RADAR, &(v.params.lat), 
									   &(v.params.lon)) < 0) {
	fprintf(stderr, "Failed to read lat/lon from file with site = <%s>\n", v.params.siteName);
	TKreportWarning(W_2A55_INVALID_SITE);
	exit(FAILED_CODE4);
  }

  v.params.min_range = 17.0;
  v.params.max_range = 150.0;
	
  if (!strncasecmp(v.params.siteName,"DAR", 3))  {
     v.params.min_range = 13.0;
     v.params.max_range = 150.0;
  }

  // Create the date and time strings
  sprintf(v.params.dateString, "%02d%02d%02d", v.params.year,
          v.params.month, v.params.day);
  sprintf(v.params.timeString, "%02d%02d%02d", v.params.hour,
	  v.params.minute, v.params.second);

  //  fprintf(stderr, "v.params.timeString = <%s>.\n", v.params.timeString);

  // Read the geographical map (same resolution as radar data)
  if (v.READgeogrNew() != 0)  {
     fprintf(stderr, "Can't read geo map file %s.\n", v.params.inputGeoMapFn);
	 TKreportWarning(W_2A55_READ_GEOGRNEW_FAILED);
     exit(FAILED_CODE4);
  }

  // Determine mask to blend out ranges where no valid data recorded by radar
  if (v.PREPvalid_range() != 0)  {
     fprintf(stderr, "Can't determine range mask.\n");
	 TKreportWarning(W_2A55_INVALID_RANGE_MASK);
     exit(FAILED_CODE4);
  }

  //DEBUG
  //FILE *fpmsg = fopen("message.sb", "w");
  //for (j = 0; j < v.params.xydim; j++)  {
  //   for (i = 0; i < v.params.xydim; i++)  {
  //       fprintf(fpmsg, "%d ", v.grids.range_mask[j][i]);
  //   }
  //   fprintf(fpmsg, "\n");
  //}
  //DEBUG

  // Open and read the csmap
  if(! no_csmap) {
    FILE *fp_csmap = fopen(fn_csmap, "r");
    if (!fp_csmap)  {
      fprintf(stderr, "Can't open file %s.\n", fn_csmap);
      TKreportWarning(W_2A55_OPEN_CSMAP_FAILED);
      exit(FAILED_CODE4);
    }
    for (j = 0; j < v.params.xydim; j++)  {
      for (i = 0; i < v.params.xydim; i++)  {
        fscanf(fp_csmap, "%d", &v.grids.rain_type[j][i]);
      }
    }

    // Create CFADs and mean vertical profiles
    v.ANALhist_cfad();
    v.ANALmean_prof();
    
    fclose(fp);
  }

  // Write out the ASCII products
  /*  v.writeASCII2a55();*/
  v.output_hdf(metadata_fname, verbose);

  return(0);
}
