/*
 * 9/24/96
 *
 * Input: Single vos (hdf, uf, ... any rsl ingest format)
 *
 * Implement Simple Simon algorithm:
 *
 *           Z = 300R^1.4
 * Where:
 *    dBZ = reflectivity
 *    dBK = 10log(K)
 *      Z = KR^a
 *
 *            (.1/a)(dBZ-dBK)
 *      R = 10    
 *
 *      a = 1.4
 *      K = 300
 *
 * Requires RSL v1.3 or later, due to a minor bug in the z_to_r routines.
 *
 * By: John H. Merritt
 *     Applied Research Corporation
 *     John.Merritt@gsfc.nasa.gov
 *     Copyright 1996
 */


#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

#include "rsl.h"

void write_rrmap2hdf(float rrmap[151][151], 
					 int hour, int min, int sec, float lat, float lon, 
					 int verbose, char *hdf_filename);

#include "write_rrmap2hdf.c"
#if defined(__sgi) || defined(__linux)
#include <getopt.h>
#endif
void doexit_handler(int sig);

/*************************************************************/
/*                                                           */
/*                  usage and process_args                   */
/*                                                           */
/*************************************************************/
void usage(char *cmd)
{
  /* The interface:
   *    program @options input1c51 input2a54 output2a53
   * Note: -v is the only option.
   */
  fprintf(stderr, "Usage: %s [-v] [-s firstfile_or_siteid] single_vos_1C-51_file 2A-54_csmap out.hdf\n", cmd);
  fprintf(stderr, "Where:\n");
  fprintf(stderr, "    'firstfile_or_siteid' is the header file (file.000)\n    or 4 letter callid, eg. KMLB\n");
  exit(-1);
}

void process_args(int argc, char **argv,
				  char **in1c51, char **site_file, char **in2a54, char **outhdf,
				  int *verbose)
{
  extern int optind;
  extern char *optarg;
  extern int optopt;

  int c;
  
  signal(SIGINT, doexit_handler);
  signal(SIGSTOP, doexit_handler);
  signal(SIGKILL, doexit_handler);
  signal(SIGFPE, doexit_handler);
  signal(SIGILL, doexit_handler);
  signal(SIGSEGV, doexit_handler);

  while ((c = getopt(argc, argv, "vs:")) != -1)
	switch (c) {
	case 'v': *verbose  = 1;  break;
	case 's': *site_file  = strdup(optarg);  break;
	case '?': usage(argv[0]); break;
	default:  break;
	}

/* must have 3 files listed */
  if (argc - optind != 3) {
	usage(argv[0]);
  }

  *in1c51 = strdup(argv[optind++]);
  *in2a54 = strdup(argv[optind++]);
  *outhdf = strdup(argv[optind]);
}
 
int main (int argc, char **argv)
{
  Radar *radar;
  Volume *dz_vol, *cz_vol;  /* Use CZ, fallback on DZ if CZ missing. */
  Sweep  *rr_sweep; /* Rain-rate volume. */
  Cappi  *cappi;
  Ray    *ray;
  unsigned char *image;
#define IMAGEDIM 151
  float rrmap[IMAGEDIM][IMAGEDIM];
  char *infile = NULL;
  char *site_file = NULL;
  char *in2a54 = NULL;
  char *hdf_filename = NULL;
  int i, j;
  int verbose;
  float lat, lon;
  
  verbose = 0;
  process_args(argc, argv, &infile, &site_file, &in2a54, &hdf_filename, &verbose);

  /* 2A-54 ignored. */
  
  if (verbose) RSL_radar_verbose_on();
  radar = RSL_anyformat_to_radar(infile, site_file);
  if (radar == NULL) {
	fprintf(stderr, "No radar in file <%s>.\n", infile);
	exit(-1);
  }

  cz_vol = radar->v[CZ_INDEX];
  if (cz_vol == NULL) {
	fprintf(stderr, "Warning: No CZ volume in file <%s>.\n", infile);
	dz_vol = radar->v[DZ_INDEX];
	if (dz_vol == NULL) {
	  fprintf(stderr, "  Error: No DZ volume in file <%s>.\n", infile);
	  exit(-1);
	}
	fprintf(stderr, "         Using DZ volume.\n");
	cz_vol = dz_vol;
  }

  

  /*
   * Redefine conversion functions for the rain-rate volume?
   */

  cappi = RSL_cappi_at_h(cz_vol, 1.5, (float)IMAGEDIM);

  rr_sweep = RSL_sweep_z_to_r(cappi->sweep, 300.0, 1.4);

  /* Wow! That was easy. */

  /* Now, convert to 2km x 2km grid, 151x151 image, and output in HDF. */
  /* Image generation assumes each grid point is 1km. */
  image = RSL_sweep_to_cart(rr_sweep, IMAGEDIM*2, IMAGEDIM*2, (float)IMAGEDIM);
  ray   = RSL_get_first_ray_of_volume(cz_vol);
  if (ray == NULL) {
	fprintf(stderr, "No ray in CZ volume, in file <%s>.\n", infile);
	exit(-1);
  }
  
  for (i=0; i<IMAGEDIM; i++) {
	for (j=0; j<IMAGEDIM; j++) {
	  rrmap[i][j] = (*(image+0)   + *(image+1) + 
						*(image+IMAGEDIM*2) + *(image+IMAGEDIM*2+1))/4;
	  image+=2;
	}
	image+=IMAGEDIM*2;
  }

  lat = radar->h.latd + radar->h.latm/60.0 + radar->h.lats/3600.0;
  lon = radar->h.lond + radar->h.lonm/60.0 + radar->h.lons/3600.0;  
  write_rrmap2hdf(rrmap, 
				  ray->h.hour, ray->h.minute, ray->h.sec+.5,
				  lat, lon, 
				  verbose, hdf_filename);

  /*
  RSL_load_refl_color_table();
  RSL_sweep_to_gif(cappi->sweep, "cappi_1.5.gif", IMAGDIM*2, IMAGEDIM*2, (float)IMAGDIM);
  */

  exit(0);
}


void doexit_handler(int sig)
{

  /* Restore STDERR and STDOUT.  This is required since both stderr and stdout
   * were redirected in the calling program, all_tape_level_1n2.
   */
  close(1);
  close(2);
  fopen("/dev/tty", "w");
  fopen("/dev/tty", "w");

  fprintf(stderr, "Got signal <%d>. Bye.\n", sig);
  if (sig == SIGINT || sig == SIGKILL || sig == SIGSTOP)
	exit(-2);

  exit(-1);
}
