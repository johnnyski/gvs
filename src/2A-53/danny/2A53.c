/*
 * Mainline driver for Danny Rosenfeld's rain estimate algorithm.
 *
 * Initiated: 3/7/96
 * By: John Merritt
 *
 * 1. Construct a 3D array, Z(nbins, nrays, nsweeps), for the appropriate
 *    volume: DZ or CZ.
 *
 * 2. Construct 2D array of azimuths, AZIM(nrays, nsweeps)
 *
 * 3. Construct 1D array of tilt angles, TILTS(nsweeps)
 *
 * 4. Call VOSWIN.  This is Danny's algorithm.
 *
 * 5. Output the resultant rainmap.
 *
 *
 *
 *              
 */

#include <stdio.h>
#include <signal.h>

#include "rsl.h"

#ifdef __sgi
#include <getopt.h>
#endif
#include <stdlib.h>
#include <string.h>
int julian(int mo, int day, int year);
/*************************************************************/
/*                                                           */
/*                  usage and process_args                   */
/*                                                           */
/*************************************************************/
void usage(char *cmd)
{
  fprintf(stderr, "Usage: %s [-v] [-s {firstfile|site}]\n\
      [-b HBASE] [-f HFREEZE] [-n DBZNOISE]\n\
      [-p PRADAR] [-t TRADAR] [-d DEWPOINT]\n\
      [-P DATAPATH] [-z ZRFILE] [-g GAGFILE] [-w GWINFILE] \n\
      [-k XKMP] [-a ALPHA] [-m MPONLY]\n\
      uffile csmap_ascii_file rrmap_hdf_file\n",cmd);
  fprintf(stderr, "\nWhere:\n\
      HBASE     =  Altitude for reflectivity map, to be used for rainfall.\n\
                   (Default: 2.0)\n\
      HFREEZE   =  Altitude of 0 C isotherm.\n\
                   (Default: 5.0)\n\
      DBZNOISE  =  Minimum reflectivity to process [dBZ].\n\
                   (Default: 0.0)\n\
      PRADAR    =  Surface pressure at the radar [mb].\n\
                   (Default: 1000.0)\n\
      TRADAR    =  Surface temperature at the radar [degrees kelvin].\n\
                   (Default: 303.0)\n\
      DEWPOINT  =  Surface dew point at the radar [degrees kelvin].\n\
                   (Default: 295.0)\n\
      DATAPATH  =  Path name where ZRFILE AND GAGFILE are located.\n\
                   (Default: $GVS_DATA_PATH)\n\
      ZRFILE    =  String with the Ze-R input file name.\n\
                   (Default: zr_mlb.out (Melbourne) or zr_drw.out(Darwine))\n\
      GAGFILE   =  String with the Gauge coordinates file name.\n\
                   (Default: sitelist_cscale)\n\
      GWINFILE  =  String with the gauge windows output file name.\n\
                   (Default: gwin.out)\n\
      XKMP      =  Optional K for a power law Z-R.\n\
                   (Default: 0.0)\n\
      ALPHA     =  Optional ALPHA for a power law Z-R.\n\
                   (Default: 0.0)\n\
      MPONLY    =  Flag for WPMM (0), or a power-law Z-R (1).\n\
                   (Default: 0.0)\n");
  exit(-1);
}

void process_args(int argc, char **argv,
				  char **infile, char **outfile,
				  char **firstfile,
				  int *verbose,
				  char *data_path,
				  float *HBASE, float *HFREEZE, 
				  float *DBZNOISE, 
				  float *PRADAR, float *TRADAR, float *DEWPOINT,
				  char **ZRFILE, char **GAGFILE, char **GWINFILE,
				  char **csmapfile,
				  float *XKMP, float *ALPHA, float *MPONLY)

{
  extern int optind;
  extern char *optarg;
  extern int optopt;

  int c;
  
  while ((c = getopt(argc, argv, "vs:c:b:f:n:p:t:d:z:g:w:k:a:m:P:")) != -1)
	switch (c) {
	case 's': *firstfile = strdup(optarg);  break;
	case 'v': *verbose  = 1;  break;
	case 'b': *HBASE    = atof(optarg); break;
	case 'f': *HFREEZE  = atof(optarg); break;
	case 'n': *DBZNOISE = atof(optarg); break;
	case 'p': *PRADAR   = atof(optarg); break;
	case 't': *TRADAR   = atof(optarg); break;
	case 'd': *DEWPOINT = atof(optarg); break;
	case 'z': *ZRFILE   = strdup(optarg); break;
	case 'g': *GAGFILE  = strdup(optarg); break;
	case 'w': *GWINFILE = strdup(optarg); break;
	case 'k': *XKMP     = atof(optarg); break;
	case 'a': *ALPHA    = atof(optarg); break;
	case 'm': *MPONLY    = atof(optarg); break;
	case 'P': strcpy(data_path,optarg); break;
	case '?': usage(argv[0]); break;
	default:  break;
	}

/* must have 3 files listed */
  if (argc - optind != 3) {
	usage(argv[0]);
  }

  *infile = strdup(argv[optind++]);
  *csmapfile = strdup(argv[optind++]);
  *outfile = strdup(argv[optind]);
}


/*************************************************************/
/*                                                           */
/*                      max_nbins                            */
/*                                                           */
/*************************************************************/
int max_nbins(Volume *v)
{
  int max = 0;
  Ray *ray;
  Sweep *sweep;
  int i, j;

  if (v == NULL) return 0;

  for (i=0; i<v->h.nsweeps; i++) {
	if (v->sweep == NULL) continue;
	sweep = v->sweep[i];
	if (sweep) {
	  for (j=0; j<sweep->h.nrays; j++) {
		ray = sweep->ray[i];
		if (ray)
		  if (ray->h.nbins > max) max = ray->h.nbins;
	  }
	}
  }
  return max;
}

/*************************************************************/
/*                                                           */
/*                       max_nrays                           */
/*                                                           */
/*************************************************************/
int max_nrays(Volume *v)
{
  int max = 0;
  Sweep *sweep;
  int i;

  if (v == NULL) return 0;

  for (i=0; i<v->h.nsweeps; i++) {
	if (v->sweep == NULL) continue;
	sweep = v->sweep[i];
	if (sweep)
	  if (sweep->h.nrays > max) max = sweep->h.nrays;
  }
  return max;
}

/*************************************************************/
/*                                                           */
/*                    construct_Z                            */
/*                                                           */
/*************************************************************/
short *construct_Z(Volume *v)
{
  short *Z;
  Sweep *sweep;
  Ray *ray;
  int nbins, nrays, nsteps;
  int index;
  int i, j, k;

  if (v == NULL) return NULL;
  /* Z's dimensions are nbins X nrays X nsweeps. */

  nbins  = max_nbins(v);
  nrays  = max_nrays(v);
  nsteps = v->h.nsweeps;

  Z = (short *)calloc(nbins*nrays*nsteps, sizeof(short));
  if (Z == NULL) perror("construct_Z");

/* Treat the indexing as if we have a 3D array: Z(nsteps, nrays, nbins).
 * Note, that notation is for C.  When passed to Fortran, nbins will be
 * incrimented the fastest; to avoid paging.
 */
  for (i=0; i<v->h.nsweeps; i++) {
	sweep = v->sweep[i];
	if (sweep) {
	  for (j=0; j<sweep->h.nrays; j++) {
		ray = sweep->ray[j];
		if (ray) {
		  for (k=0; k<ray->h.nbins; k++) {
			/* Necessary math because we could have skip structures and
			 * we want them to be 0's in Z.
			 */
			index = i*(nbins*nrays) + j*(nbins) + k;
			Z[index] = ray->h.f(ray->range[k]) * 10; /* Scaled by 10. */
		  }
		}
	  }
	}
  }
  return Z;
}
/*************************************************************/
/*                                                           */
/*                    construct_AZIM                         */
/*                                                           */
/*************************************************************/
float *construct_AZIM(Volume *v)
{
  float *AZIM;
  Sweep *sweep;
  Ray *ray;
  int nrays, nsteps;
  int index;
  int i, j;

  if (v == NULL) return NULL;
  nrays  = max_nrays(v);
  nsteps = v->h.nsweeps;

  AZIM = (float *)calloc(nrays*nsteps, sizeof(float));
  if (AZIM == NULL) perror("construct_AZIM");

/* Treat the indexing as if we have a 3D array: AZIM(nsteps, nrays).
 * Note, that notation is for C.  When passed to Fortran, nrays will be
 * incrimented the fastest; to avoid paging.
 */
  for (i=0; i<v->h.nsweeps; i++) {
	sweep = v->sweep[i];
	if (sweep) {
	  for (j=0; j<sweep->h.nrays; j++) {
		ray = sweep->ray[j];
		if (ray) {
		  index = i*(nrays) + j;
		  AZIM[index] = ray->h.azimuth;
		}
	  }
	}
  }

  return AZIM;
}
/*************************************************************/
/*                                                           */
/*                    construct_TILTS                        */
/*                                                           */
/*************************************************************/
float *construct_TILTS(Volume *v)
{
  float *TILTS;
  Sweep *sweep;
  int nsteps;
  int index;
  int i;

  if (v == NULL) return NULL;
  nsteps = v->h.nsweeps;

  TILTS = (float *)calloc(nsteps, sizeof(float));
  if (TILTS == NULL) perror("construct_AZIM");

/* Treat the indexing as if we have a 1D array: TILTS(nsteps).
 * Note, that notation is for C.  When passed to Fortran, nsteps
 * incrimented the fastest; to avoid paging.
 */
  for (i=0; i<v->h.nsweeps; i++) {
	sweep = v->sweep[i];
	if (sweep) {
	  index = i;
	  TILTS[index] = sweep->h.elev;
	}
  }

  return TILTS;
}

/*************************************************************/
/*                                                           */
/*                        loadcsmap                          */
/*                                                           */
/*************************************************************/
short *loadcsmap(char *csmapfile)
{
  short *csmap;
  FILE *fp;
  int i, num;

  csmap = (short *)calloc(151*151, sizeof(short));
  if (csmap == NULL) {
	perror("load csmap");
	return NULL;
  }
  if ((fp = fopen(csmapfile, "r")) == NULL) {
	perror(csmapfile);
  return csmap;
/*	return NULL;*/
  }

  for (i=0; i<151*151; i++) {
	fscanf(fp, "%d", &num);
	csmap[i] = num;
  }
  fclose(fp);
  return csmap;
}

/*************************************************************/
/*                                                           */
/*                       output_rainmap                      */
/*                                                           */
/*************************************************************/
void output_rainmap(float *rainmap, char *outfile)
{
  FILE *fp;
  int i, j;

  if ((fp = fopen(outfile, "w")) == NULL) {
	perror(outfile);
	return;
  }

  for (i=0; i<151; i++) {
	for (j=0; j<151; j++)
	  fprintf(fp, " %.2f", *rainmap++);
	fprintf(fp, "\n");
  }

  fclose(fp);
}

void handler(int sig)
{
  fprintf(stderr, "Got signal %d. Abort.\n", sig);
  kill(0, sig);
  if (sig == SIGINT || sig == SIGKILL || sig == SIGSTOP) 
	exit (-2);
  exit(-1);
}

/*************************************************************/
/*                                                           */
/*                        main                               */
/*                                                           */
/*************************************************************/
int main(int argc, char **argv)
{
extern void write_rrmap2hdf(float rrmap[151][151], 
					 int hour, int min, int sec, float lat, float lon, 
					 int verbose, char *hdf_filename);

  char *infile, *outfile, *firstfile;
  int verbose;
  Radar *radar;
  Volume *qcvol;

  short *Z;
  float *AZIM;
  float *TILTS;
  int NBINS, NRAYS, NSTEPS;
  Ray *ray;
  Sweep *sweep;
  float RANGE1,  BINLENGTH,  RADELEV,  HBASE,  HFREEZE,  DBZNOISE;
  float PRADAR,  TRADAR,  DEWPOINT;
  char ZRFILE[200],  GAGFILE[200],  *GWINFILE;

  char *CSMAPFILE;
  char *tmp_ZRFILE,  *tmp_GAGFILE;
  float XKMP,  ALPHA,  MPONLY;
  float RAYWIDTH,  RADLAT,  RADLON;
  int YEAR, JDAY, HOUR, MINUTE, SECOND;
  short *CSMAP;

  char data_path[100];
  float RAINMAP[151][151];

  signal(SIGINT, handler);
  signal(SIGFPE, handler);
  signal(SIGKILL, handler);
  signal(SIGILL, handler);
  signal(SIGSTOP, handler);
  signal(SIGSEGV, handler);

  verbose = 0;
  memset(ZRFILE,'\0', 200);
  memset(GAGFILE,'\0',200);
  memset(data_path,'\0', 100);

  if (getenv("GVS_DATA_PATH") != NULL) 
	strcpy(data_path, getenv("GVS_DATA_PATH"));
  else 
	strcpy(data_path, "");

/* From parameters list or defaults. Overridden via arguments on the
 * command line.
 */
  HBASE    = 2.0; /* Altitude for reflectivity map, to be used for rainfall. */
  HFREEZE  = 5.0; /* Altitude of 0 C isotherm */
  DBZNOISE = 0.0; /* Minimum reflectivity to process [dBZ] */
  PRADAR   = 1000.0; /* Surface pressure at the radar [mb] */
  TRADAR   = 303.0;  /* Surface temperature at the radar [degrees kelvin] */
  DEWPOINT = 295.0;  /* Surface dew point at the radar [degrees kelvin] */
  tmp_ZRFILE   = ""; /* String with the Ze-R input file name. */
  tmp_GAGFILE  = "sitelist_cscale"; /* String with the Gauge coordinates file name. */
  GWINFILE = "gwin.out";    /* String with the gauge window output file name.*/
  CSMAPFILE = "";  /* Convective/Stratiform file. This is 2A-54 (ASCII). */
  XKMP     = 0.0; /* Optional K for a power law Z-R. */
  ALPHA    = 0.0; /* Optional ALPHA for a power law Z-R. */
  MPONLY   = 0.0; /* Flag for WPMM (0), or a power-law Z-R (1). */
  
  process_args(argc, argv, &infile, &outfile, &firstfile, &verbose, data_path,
			   &HBASE, &HFREEZE, 
			   &DBZNOISE, 
			   &PRADAR, &TRADAR, &DEWPOINT,
			   &tmp_ZRFILE, &tmp_GAGFILE, &GWINFILE, 
			   &CSMAPFILE,
			   &XKMP, &ALPHA, &MPONLY);

  if (strlen(data_path) == 0) 
	fprintf(stderr, "Data path is unknown. Set $GVS_DATA_PATH or specify -p data_path\n");
  if (verbose) 	RSL_radar_verbose_on();
  radar = RSL_anyformat_to_radar(infile, firstfile);
  if (radar == NULL) {
	perror("RSL_anyformat_to_radar");
	exit(-1);
  }

  if (strlen(tmp_ZRFILE) == 0) {
	/* get default zr file if it was not specified */
	if (strcmp(radar->h.radar_name, "KMLB") == 0) 
	  tmp_ZRFILE = "zr_mlb.out";
	else
	  tmp_ZRFILE = "zr_drw.out";
  }
	
  sprintf(ZRFILE, "%s/%s", data_path, tmp_ZRFILE);
  sprintf(GAGFILE, "%s/%s", data_path, tmp_GAGFILE);
  if (verbose) {
	fprintf(stderr, "ZRFILE = %s\n", ZRFILE);
	fprintf(stderr, "GAGFILE = %s\n", GAGFILE);
	/* TN 051496 -- changed name */
	fprintf(stderr, "CSMAPFILE = %s\n", CSMAPFILE);
	fprintf(stderr, "UFFILE = %s\n", infile);

  }


  qcvol = radar->v[CZ_INDEX];
  if (qcvol == NULL) {
	fprintf(stderr, "No CZ volume.\n");
	exit(-1);
  }

  /* Construct Z array.  This is a basic repackaging of the Volume
   * structure suitable for passing to a Fortran subroutine.
   *
   * Optimization note: combine the construction of Z, AZIM and TILTS.
   */
  Z = construct_Z(qcvol);

  /* Z's dimensions are nbins X nrays X nsweeps. */

  NBINS  = max_nbins(qcvol);
  NRAYS  = max_nrays(qcvol);
  NSTEPS = qcvol->h.nsweeps;
  if (verbose) printf("NBINS %d, NRAYS %d, NSTEPS %d\n", NBINS, NRAYS, NSTEPS);
  AZIM = construct_AZIM(qcvol);

  TILTS = construct_TILTS(qcvol);

#ifdef TEST
  {
	int i;
	for (i=0; i<NSTEPS; i++)
	  printf("TILTS[%d] = %f\n", i, TILTS[i]);
  }

  {
	int i;
	for (i=0; i<NSTEPS*NRAYS; i++)
	  printf("AZIM[%d] = %f\n", i, AZIM[i]);
  }
#endif
/*
 * DEFINE ADDITIONAL PARAMETERS.
 */
  ray       = RSL_get_first_ray_of_volume(qcvol);
  sweep     = RSL_get_first_sweep_of_volume(qcvol);
  RANGE1    = ray->h.range_bin1 / 1000.0;  /* KM */
  BINLENGTH = ray->h.gate_size  / 1000.0;  /* KM */
  RADELEV   = radar->h.height   / 1000.0;  /* KM */
  RAYWIDTH  = sweep->h.beam_width;
  RADLAT    = radar->h.latd + radar->h.latm/60.0 + radar->h.lats/3600.0;
  RADLON    = radar->h.lond + radar->h.lonm/60.0 + radar->h.lons/3600.0;  
  YEAR      = ray->h.year;
  JDAY      = julian(ray->h.month, ray->h.day, YEAR);
  HOUR      = ray->h.hour;
  MINUTE    = ray->h.minute;
  SECOND    = ray->h.sec + .5; /* nearest int. */

	if (verbose) printf("LAT/LON %f/%f,\
 YEAR %d, JDAY %d, HOUR %d, MIN %d, SEC %d\n",
						RADLAT, RADLON, YEAR, JDAY, HOUR, MINUTE, SECOND);

  RSL_free_radar(radar); /* Free up as much memory as possible before invoking
						  * the rain estimate algorithm.
						  */


  CSMAP = loadcsmap(CSMAPFILE); 

  /*
   * Now, call the Fortran rain estimate program.
   */
/*
 * To access elements of Z such that it matches the fortran syntax,
 *    Z(istep, iray, ibin), use the following index calculation:
 *        index = i*(nbins*nrays) + j*(nbins) + k;
 *    Where, i = istep, j = iray, and k = ibin.
 *
 * To access elements of AZIM such that it matches the fortran syntax,
 *    AZIM(istep, iray), use the following index calculation:
 * 		  index = i*(nrays) + j;
 *    Where, i = istep, and j = iray.
 */
  
#if defined(__linux) || defined(__sgi) || defined(__sun)
  voswin_
#else
  voswin
#endif
(&HBASE, &HFREEZE, &RAYWIDTH, &RANGE1, &BINLENGTH,
		  &DBZNOISE, &RADLAT, &RADLON, &RADELEV, &NBINS, &NRAYS, &NSTEPS,
		  &PRADAR, &TRADAR, &DEWPOINT, Z,
		  ZRFILE, GAGFILE, GWINFILE, CSMAP,
		  &XKMP, &ALPHA, &MPONLY, AZIM, TILTS, &verbose,
		  &YEAR, &JDAY, &HOUR, &MINUTE, &SECOND,
/* RETURN */
		  RAINMAP,

/* Special arguments.  Fortran string lengths. These *are* constants.
 * List string lengths in order of appearance. These *must* be last
 * in the parameters list.
 */
		  strlen(ZRFILE), strlen(GAGFILE), strlen(GWINFILE));

/*
 * Now output the rain map.
 */ 

/*  TN 051396: write rainmap to hdf file */
  if  (verbose)
     fprintf(stderr, "Writing rrmap to hdf file\n");

  write_rrmap2hdf(RAINMAP, HOUR, MINUTE, SECOND, RADLAT, RADLON, verbose, 
				  outfile);

  return 0;
}
	 
int __main(int ac, char **av)
{
  /* Do nothing. */

  return 0;
}
