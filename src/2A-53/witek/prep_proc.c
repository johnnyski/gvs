#define  VERSION 2.0
#include <unistd.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "rsl.h"
#include "aiw.h"                   /* for the HML's AIW library */
#define BUFFER_SIZE 16384          /* size of file IO buffers   */

long julday(int mm, int id, int iyyy);
void prepCompress(int *value, int nvals, int *prep, int *nprep);
void getHeaderDefaults(sweep_header_t *sweep,Radar *radar,int j,int i,int k);
void clearHeader(sweep_header_t * sweep);
void compareHeaders(sweep_header_t header1,sweep_header_t header2,
                    long *warning_number, long *error_number);
void checkHeader(sweep_header_t header,long *warning_number, long *error_number);

void getArgs(int argc, char **argv,int *rid, int * dtype,
          char **infile,FILE **fp1,char ** outfile, FILE **fp2, int *verbose);

unsigned char aiw_invf(float x);
FILE *uncompress_pipe(FILE *f);


void main(int argc, char **argv)
{
  extern void trap_signals(void (*handler)(int));
  extern void handler(int sig);

  FILE    *fp1,*fp2;
  int     i,j,k,l,n,ns;
  int     nsweeps;
  int     v,value[2000];
  int     nprep,prep[2000];
  int     allzero, ray_nbins;
  long    warning_number,error_number;
  float   max,min;
  float   ray_time,start_time;
  float   stop_time,delta;
  float   xval;
  char    *infile,*outfile;
  Radar   *radar;
  int     rid;                  /* radar ID, i.e., MELBOURNE = 20 */
  int     dtype;                /* type of radar data: CZ or DZ */
  sweep_header_t sweep;         /* declared in "aiw.h" */
  sweep_header_t ray;           /* declared in "aiw.h" */
  int     verbose = 0;

  /* Function pointer, specific for volume. I use these for
   * syntax simplification.
   */
  static float (*F)(Range x);

  /* trap signals to exit with the right code */
  trap_signals(handler);

  /* Next two lines shut up some compilers */

  fp1 = fp2 = NULL;
  infile = outfile = "NULL";

  /* Process command-line arguments */

  getArgs(argc,argv,&rid,&dtype,&infile,&fp1,&outfile,&fp2, &verbose);

  /* Read the UF file */
  /*  RSL_radar_verbose_on(); */
  fp1 = uncompress_pipe(fp1); /* From rsl library. */
  radar = RSL_uf_to_radar_fp(fp1);
  j = dtype;

  /*
  ** The variable: radar->v[j]->h.nsweeps is a misnomer. It seems
  ** to refer to maximum possible number of sweeps for this file.
  ** To determine the actual number of sweeps, use the code below.
  */

  nsweeps = 0;
  for (i=0;i<=radar->v[j]->h.nsweeps-1;i++){
    if (radar->v[j]->sweep[i] != 0)
      nsweeps++;
  }
  if (nsweeps == 0){
    fprintf(stderr,"FATAL ERROR: no CZ data in UF file.\n");
    exit(-1);
  }


  if (verbose) {
	fprintf(stderr, "nvolumes: %d\n",radar->h.nvolumes);
	fprintf(stderr, "nsweeps: %d %d\n",radar->v[j]->h.nsweeps,nsweeps);
	fprintf(stderr, "Latitude: %d:%d:%d\n",radar->h.latd,radar->h.latm,radar->h.lats);
	fprintf(stderr, "Longitude: %d:%d:%d\n",radar->h.lond,radar->h.lonm,radar->h.lons);
	fprintf(stderr, "Height: %d\n",radar->h.height);
	fprintf(stderr, "short pulse: %d\n",radar->h.spulse);
	fprintf(stderr, "long pulse: %d\n",radar->h.lpulse);
	fprintf(stderr, "City: %s\n",radar->h.city);
  }

  ns = -1;  
  for (i=0;i<=radar->v[j]->h.nsweeps-1;i++){
    if (radar->v[j]->sweep[i] == 0) 
      continue;

    ns++;

    /* 
    ** Create sweep header by getting values from first ray in sweep (k=0).
    ** Since this header will be the default for the sweep, check to see if 
    ** it is ok. If not exit.
    **
    */

    k = 0;
    getHeaderDefaults(&sweep,radar,j,i,k);
    warning_number = HEAD_NO_WARN;
    error_number = HEAD_NO_ERR;

    checkHeader(sweep,&warning_number,&error_number);
    if ((warning_number != HEAD_NO_WARN) || (error_number != HEAD_NO_ERR)) {
      fprintf(stderr,"FATAL ERROR: first ray of sweep has bad header:\n");
      printWarning(stderr,warning_number);
      printError(stderr,error_number);
	  fprintf(stderr, " ---< HOWEVER, ATTEMPTING TO CONTINUE. (at check #1) >---\n");
	  
	  /*      exit(-1); */
    }
    
    sweep.version = _AIW_VERSION;
    sweep.warning_number = HEAD_NO_WARN;
    sweep.error_number = HEAD_NO_ERR;
    sweep.nsweeps = nsweeps;
    sweep.number= ns;
    sweep.nheader_lines = 4;
    sweep.header_type = 1;
    sweep.type_of_data = dtype;
    sweep.radar_id = rid;
    sweep.smallest_value = 0;
    sweep.largest_value = 255;
    sweep.julian_day = julday(sweep.month,sweep.day,sweep.year);
    strcpy(sweep.time_units,"UT");
    strcpy(sweep.angle_units,"DEG");
	sweep.latd = radar->h.latd;
	sweep.latm = radar->h.latm;
	sweep.lats = radar->h.lats; 
	sweep.lond = radar->h.lond; 
	sweep.lonm = radar->h.lonm; 
	sweep.lons = radar->h.lons; 

    /*  Determine the number of rays per second */
  
    k = 0;                      /* first ray */
    start_time = radar->v[j]->sweep[i]->ray[k]->h.hour*3600.0 +
      60.0*radar->v[j]->sweep[i]->ray[k]->h.minute +
        radar->v[j]->sweep[i]->ray[k]->h.sec;
    
    k = sweep.nrays-1;          /* last ray */
    stop_time = radar->v[j]->sweep[i]->ray[k]->h.hour*3600.0 +
      60.0*radar->v[j]->sweep[i]->ray[k]->h.minute +
        radar->v[j]->sweep[i]->ray[k]->h.sec;

    if (stop_time == start_time) /* i.e., partial sweep */
      sweep.rays_per_second = UNKNOWN;
    else
      sweep.rays_per_second =  (float)sweep.nrays/(stop_time-start_time);

    /* 
    ** Count the number of non-zero rays in this sweep. Also determine
    ** sweep.nbins, which is the maximum number of bins any ray in this
    ** sweep will have. (Normally all the rays have the same number of
    ** bins, but there are cases when on or two rays in a sweep
    ** have fewer.)
    */
    
    sweep.nonzero = 0;
    sweep.nbins = 0;
    for (k=0;k<=sweep.nrays-1;k++){
      ray_nbins = radar->v[j]->sweep[i]->ray[k]->h.nbins;
      if (ray_nbins > sweep.nbins) 
        sweep.nbins = ray_nbins;
      for (l=0;l<=ray_nbins-1;l++){
        v = (int) radar->v[j]->sweep[i]->ray[k]->range[l];  /* No conversion
															 * is necessary.
															 * Just 0 count.
															 */
        if (v != 0){
          sweep.nonzero++;
          break;
        }
      }
    }

    /* Write sweep header */

    writeSweepHeader(fp2,sweep);

    for (k=0;k<=sweep.nrays-1;k++){

      /* Generate the ray header */

      getHeaderDefaults(&ray,radar,j,i,k);
      ray.version = _AIW_VERSION;
      ray.number = k;           /* not the same as ray.ray_number */
      ray.azimuth = radar->v[j]->sweep[i]->ray[k]->h.azimuth;
      ray.nbins =  radar->v[j]->sweep[i]->ray[k]->h.nbins;
      ray.nsweeps = nsweeps;
      ray.nheader_lines = 4;
      ray.header_type = 1;
      ray.type_of_data = dtype;
      ray.radar_id = rid;
      ray.smallest_value = 0;
      ray.largest_value = 255;
      ray.julian_day = julday(ray.month,ray.day,ray.year);
      strcpy(ray.time_units,"UT");
      strcpy(ray.angle_units,"DEG");

      /* Perform some checks */

      max = -100000.0;
      min = +100000.0;
	  F = radar->v[j]->sweep[i]->ray[k]->h.f;
      for (l=0;l<=ray.nbins-1;l++){
		/* No round off error for wsr88d,
		 * minor round off for lassen.
		 */
		/* Use RSL internal storage to floating point conversion function. */
        xval = F(radar->v[j]->sweep[i]->ray[k]->range[l]);
		/* Convert floating point to HMLF interal storage. */
		v = aiw_invf(xval);
		if (v > max) max = v;
		if (v < min) min = v;
		assert((max <= 255));
		assert((min >= 0));
        value[l] = v;
      }

      allzero = TRUE;
      for (n=0;n<=ray.nbins-1;n++)
        if (value[n] != 0){
          allzero = FALSE;
          break;
        }
      if (allzero) continue;
      
      /* Compress */

      ray_time = radar->v[j]->sweep[i]->ray[k]->h.hour*3600.0 +
        60.0*radar->v[j]->sweep[i]->ray[k]->h.minute +
          radar->v[j]->sweep[i]->ray[k]->h.sec;
      delta = ray_time - start_time;

      /* 
      ** Check the ray header for inconsistencies/error (i.e., invalid
      ** values, such as second > 59, and so on) with a call to function
      ** checkHeader.
      ** 
      ** We assume that several of the headers fields follow a certain
      ** rule. Check to see if this is true with a call to the function
      ** compareHeaders. 
      **
      ** If both the flags warning_number and error_number are not ok, then
      ** generate a custom header.
      */
   
      warning_number = HEAD_NO_WARN;
      error_number = HEAD_NO_ERR;
      checkHeader(ray,&warning_number,&error_number);
      compareHeaders(sweep,ray,&warning_number,&error_number);

      if (error_number != HEAD_NO_ERR) {
        printError(stderr,error_number);
        fprintf(stderr,"(I have encountered invalid input data and can not continue.)\n");
		fprintf(stderr," ---< HOWEVER, ATTEMPTING TO CONTINUE. (at check #2) >---\n");

		/*        exit(-1); */
      }
      prepCompress(value,ray.nbins,prep,&nprep);
      fprintf(fp2,"%d %d %d %g\n",ray.number,(int)(ray.azimuth*64.0),nprep,delta);
      asciiEncode(fp2,prep,nprep);
    }
  }
  fclose(fp2);
  exit(0);
}


#include <string.h>
void getArgs(int argc, char **argv,int *rid, int * dtype,
             char **infile,FILE **fp1,char **outfile, FILE **fp2, int *verbose)
{
  int    n,c,errflg;
  extern char *optarg;
  extern int optind,optopt;

  errflg = 0;
  if (argc < 4) errflg++;

  while((c=getopt(argc,argv,"vt:i:"))!=-1){

    switch (c) {
	case 'v': *verbose = 1; break;
    case 't':
      if (strcmp(optarg,"DZ") == 0)
        *dtype = DZ_INDEX;
      else if (strcmp(optarg,"CZ") == 0)
        *dtype = CZ_INDEX;
      else {
        fprintf(stderr,
                "ERROR: argument to -t option must be \"DZ\" or \"CZ\"\n");
        exit(-1);
      }
      break;

    case 'i':
      *rid = atoi(optarg);
      break;

    case '?':
      fprintf(stderr,"Unrecognized option: -%c\n",optopt);
      errflg++;
    }
  }

  n = argc - optind;            /* number of arguments left */
  if ((errflg > 0) || (n < 1) || (n > 2) ){
    fprintf(stderr,"Usage: prep_proc [-v] -t type -i id uf_file prep_file\n");
    exit(-1);    
  }

  switch (n){
  case 1: 
    *infile = "stdin";
    *fp1 = stdin;
    if ((*fp2=fopen(argv[optind],"w"))==NULL){
      fprintf(stderr,"ERROR: could not open file: %s\n",argv[optind]);
      exit(-1);
    }
    setvbuf(*fp2,NULL,_IOFBF,(size_t)BUFFER_SIZE);
    *outfile = strdup(argv[optind]);
    break;
  case 2:
    if ((*fp1=fopen(argv[optind],"r"))==NULL){
      fprintf(stderr,"ERROR: could not open file: %s\n",argv[optind]);
      exit(-1);
    }
    setvbuf(*fp1,NULL,_IOFBF,(size_t)BUFFER_SIZE);
    *infile = strdup(argv[optind]);
    if ((*fp2=fopen(argv[optind+1],"w"))==NULL){
      fprintf(stderr,"ERROR: could not open file: %s\n",argv[optind++]);
      exit(-1);
    }
    setvbuf(*fp2,NULL,_IOFBF,(size_t)BUFFER_SIZE);
    *outfile = strdup(argv[optind+1]);
    break;
  }
}

void getHeaderDefaults(sweep_header_t *sweep,Radar *radar,int j,int i,int k)
{
  clearHeader(sweep);           /* clear fields */
  strcpy(sweep->radar_name,radar->h.radar_name);
  sweep->year = radar->h.year;
  sweep->month = radar->v[j]->sweep[i]->ray[k]->h.month;
  sweep->day = radar->v[j]->sweep[i]->ray[k]->h.day;
  sweep->hour = radar->v[j]->sweep[i]->ray[k]->h.hour;
  sweep->minute = radar->v[j]->sweep[i]->ray[k]->h.minute;
  sweep->second = radar->v[j]->sweep[i]->ray[k]->h.sec;
  sweep->nrays = radar->v[j]->sweep[i]->h.nrays;
  sweep->elevation = radar->v[j]->sweep[i]->h.elev;
  sweep->beam_width = radar->v[j]->sweep[i]->h.beam_width;
  sweep->range_bin1 = radar->v[j]->sweep[i]->ray[k]->h.range_bin1;
  sweep->gate_size = radar->v[j]->sweep[i]->ray[k]->h.gate_size;
  sweep->sweep_rate = radar->v[j]->sweep[i]->ray[k]->h.sweep_rate;
  sweep->prf = radar->v[j]->sweep[i]->ray[k]->h.prf;
  sweep->pulse_count = radar->v[j]->sweep[i]->ray[k]->h.pulse_count;
  sweep->pulse_width = radar->v[j]->sweep[i]->ray[k]->h.pulse_width;
  sweep->frequency = radar->v[j]->sweep[i]->ray[k]->h.frequency;
  sweep->wavelength = radar->v[j]->sweep[i]->ray[k]->h.wavelength;
  sweep->nyquist_velocity = radar->v[j]->sweep[i]->ray[k]->h.nyq_vel;
  sweep->azimuth = radar->v[j]->sweep[i]->ray[k]->h.azimuth;
  sweep->ray_number = radar->v[j]->sweep[i]->ray[k]->h.ray_num;
}

void clearHeader(sweep_header_t * sweep)
{
  strcpy(sweep->radar_name,"UNKNOWN");
  strcpy(sweep->time_units,"UNKNOWN");
  strcpy(sweep->angle_units,"UNKNOWN");
  sweep->nheader_lines = (int)UNKNOWN;
  sweep->header_type = (int)UNKNOWN;
  sweep->radar_id = (int)UNKNOWN;
  sweep->type_of_data = (int)UNKNOWN;
  sweep->nsweeps = (int)UNKNOWN;
  sweep->nrays = (int)UNKNOWN;
  sweep->elevation_number = (int)UNKNOWN;
  sweep->range_bin1 = (int)UNKNOWN;
  sweep->gate_size = (int)UNKNOWN;
  sweep->sector_number = (int)UNKNOWN;
  sweep->prf = (int)UNKNOWN;
  sweep->rays_per_second = (float)UNKNOWN;
  sweep->unambiguous_range = (float)UNKNOWN;
  sweep->beam_width = (float)UNKNOWN;
  sweep->elevation = (float)UNKNOWN;
  sweep->system_calibration = (float)UNKNOWN;
  sweep->velocity_resolution = (float)UNKNOWN;
  sweep->sweep_rate = (float)UNKNOWN;
  sweep->azimuth_rate = (float)UNKNOWN;
  sweep->fix_angle = (float)UNKNOWN;
  sweep->pulse_count = (float)UNKNOWN;
  sweep->pulse_width = (float)UNKNOWN;
  sweep->frequency = (float)UNKNOWN;
  sweep->wavelength = (float)UNKNOWN;
  sweep->nyquist_velocity = (float)UNKNOWN;
  sweep->attenuation = (float)UNKNOWN;
  sweep->minimum_delta = (float)UNKNOWN;
  sweep->julian_day = (long)UNKNOWN;
  sweep->year = (int)UNKNOWN;
  sweep->month = (int)UNKNOWN;
  sweep->day = (int)UNKNOWN;
  sweep->hour = (int)UNKNOWN;
  sweep->minute = (int)UNKNOWN;
  sweep->second = (int)UNKNOWN;
  sweep->status = (int)UNKNOWN;
  sweep->number = (int)UNKNOWN;
  sweep->ray_number = (int)UNKNOWN;
  sweep->nbins = (int)UNKNOWN;
  sweep->nonzero = (int)UNKNOWN;
  sweep->smallest_value = (int)UNKNOWN;
  sweep->largest_value = (int)UNKNOWN;
  sweep->azimuth = (float)UNKNOWN;
}  

void checkHeader(sweep_header_t header,long *warning_number, long *error_number)
{
  /* 
  ** This routines performs several consistency checks on the header.
  ** For example, the value in the second field should be < 59, the
  ** hour field < 23, and so on. If the fields are consistent, return 0,
  ** else return a value > 0;
  */ 

  if ((header.month < 1) || (header.month > 12))
    *error_number = *error_number + HEAD_INVALID_MONTH_ERR;
    
  if ((header.day < 1) || (header.day > 31))
    *error_number = *error_number + HEAD_INVALID_DAY_ERR;

  if ((header.hour < 0) || (header.hour > 23))
    *error_number = *error_number + HEAD_INVALID_HOUR_ERR;

  if ((header.minute < 0) || (header.minute > 59))
    *error_number = *error_number + HEAD_INVALID_MINUTE_ERR;

  if ((header.second < 0) || (header.second > 59))
    *error_number = *error_number + HEAD_INVALID_SECOND_ERR;
}

void compareHeaders(sweep_header_t header1,sweep_header_t header2,
                    long *warning_number, long *error_number)
{

  /* These parameters are expected to change */

  if (header1.year != header2.year)
    *warning_number = *warning_number + HEAD_YEAR_WARN;

  if (header1.month != header2.month)
    *warning_number = *warning_number + HEAD_MONTH_WARN;

  if (header1.day != header2.day)
    *warning_number = *warning_number + HEAD_DAY_WARN;

  if (header1.julian_day != header2.julian_day)
    *warning_number = *warning_number + HEAD_JULIAN_DAY_WARN;

  if (header1.sector_number != header2.sector_number)
    *warning_number = *warning_number + HEAD_SECTOR_NUMBER_WARN;


  /* These parameters may change, but are unlikely */

  if (header1.range_bin1 != header2.range_bin1)
    *warning_number = *warning_number + HEAD_RANGE_BIN_WARN;

  if (header1.gate_size != header2.gate_size)
    *warning_number = *warning_number + HEAD_GATE_SIZE_WARN;

  if (header1.prf != header2.prf)
    *warning_number = *warning_number + HEAD_PRF_WARN;

  if (header1.unambiguous_range != header2.unambiguous_range)
    *warning_number = *warning_number + HEAD_UNAMBIGUOUS_RANGE_WARN;

  if (header1.velocity_resolution != header2.velocity_resolution)
    *warning_number = *warning_number + HEAD_VELOCITY_RESOLUTION_WARN;

  if (header1.sweep_rate != header2.sweep_rate)
    *warning_number = *warning_number + HEAD_SWEEP_RATE_WARN;

  if (header1.azimuth_rate != header2.azimuth_rate)
    *warning_number = *warning_number + HEAD_SWEEP_AZIMUTH_RATE_WARN;

  if (header1.fix_angle != header2.fix_angle)
    *warning_number = *warning_number + HEAD_FIX_ANGLE_WARN;

  if (header1.pulse_count != header2.pulse_count)
    *warning_number = *warning_number + HEAD_PULSE_COUNT_WARN;

  if (header1.pulse_width != header2.pulse_width)
    *warning_number = *warning_number + HEAD_PULSE_WIDTH_WARN;

  if (header1.minimum_delta != header2.minimum_delta)
    *warning_number = *warning_number + HEAD_MINIMUM_DELTA_WARN;


  /* These parameters should not change */

  if (header1.frequency != header2.frequency)
    *error_number = *error_number + HEAD_FREQUENCY_ERR;

  if (header1.wavelength != header2.wavelength)
    *error_number = *error_number + HEAD_WAVELENGTH_ERR;

  if (header1.nyquist_velocity != header2.nyquist_velocity)
    *error_number = *error_number + HEAD_NYQUIST_ERR;

  if (header1.attenuation != header2.attenuation)
    *error_number = *error_number + HEAD_ATTENUATION_ERR;

  if (header1.system_calibration != header2.system_calibration)
    *error_number = *error_number + HEAD_SYSTEM_CALIBRATION_ERR;

  if (header1.beam_width != header2.beam_width)
    *error_number = *error_number + HEAD_BEAM_WIDTH_ERR;

  if (header1.elevation != header2.elevation)
    *error_number = *error_number + HEAD_ELEVATION_ERR;

  if (header1.nheader_lines != header2.nheader_lines)
    *error_number = *error_number + HEAD_NHEADER_LINES_ERR;

  if (header1.header_type != header2.header_type)
    *error_number = *error_number + HEAD_HEADER_TYPE_ERR;

  if (header1.radar_id != header2.radar_id)
    *error_number = *error_number + HEAD_RADAR_ID_ERR;

  if (strcmp(header1.radar_name,header2.radar_name) != 0)
    *error_number = *error_number + HEAD_RADAR_NAME_ERR;

  if (header1.type_of_data != header2.type_of_data)
    *error_number = *error_number + HEAD_TYPE_OF_DATA_ERR;

  if (header1.nsweeps != header2.nsweeps)
    *error_number = *error_number + HEAD_NSWEEPS_ERR;

  if (header1.nrays != header2.nrays)
    *error_number = *error_number + HEAD_NRAYS_ERR;

  if (header1.elevation_number != header2.elevation_number)
    *error_number = *error_number + HEAD_ELEVATION_NUMBER_ERR;

  if (header1.smallest_value != header2.smallest_value)
    *error_number = *error_number + HEAD_SMALLEST_VALUE_ERR;

  if (header1.largest_value != header2.largest_value)
    *error_number = *error_number + HEAD_LARGEST_VALUE_ERR;

  if (header1.status != header2.status)
    *error_number = *error_number + HEAD_STATUS_ERR;

  if (strcmp(header1.time_units,header2.time_units) != 0)
    *error_number = *error_number + HEAD_TIME_ERR;

  if (strcmp(header1.angle_units,header2.angle_units) != 0)
    *error_number = *error_number + HEAD_ANGLE_ERR;
}  
