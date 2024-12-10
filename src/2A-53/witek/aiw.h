#ifndef _AIW_INCLUDED
#define _AIW_VERSION    2.0
#include <stdio.h>
#include "error.h"

#define TRUE            1
#define FALSE           0
#define UNKNOWN        -1         /* unknown field in sweep header */
#ifndef BADVAL
#define BADVAL          320       /* data below SNR threshold */
#define RFVAL          (BADVAL-1) /* meaning unclear. A.K. 10/18/94 */
#define APFLAG         (BADVAL-2)
#define ISDATA(x)      (((x) >= (float)(BADVAL-2)) ? FALSE : TRUE) /* test for data */
#endif

            /* Status flags for rays and sweeps follow */

#define NEW_ELEVATION   0       /* ray only */
#define INTERMEDIATE    1       /* ray and sweep */
#define END_ELEVATION   2       /* ray only */
#define NEW_VOLUME      3       /* ray and sweep */
#define END_VOLUME      4       /* ray and sweep */

typedef struct {
  int     nheader_lines;        /* # of lines in header */
  float   version;              /* ASCII-RLE version */
  int     header_type;          /* 0 => RLE binary */
                                /* 1 => RLE ASCII-coded */
  int     radar_id;             /* unique integer ID for radar */
  char    radar_name[80];       /* name of radar */
  int     type_of_data;         /* 0 => RAW reflectivity */
                                /* 3 => QC reflectivity */
  int     nsweeps;              /* # of sweeps in this volume */
  int     latd;                  /* latitude: degree */
  int     latm;                  /* latitude: minute */
  int     lats;                  /* latitude: second */
  int     lond;                  /* longitude: degree */
  int     lonm;                  /* longitude: minute */
  int     lons;                  /* longitude: second */

  /* Variables that (may) change every sweep follow */

  int     nrays;                /* # of rays in this sweep */
  int     elevation_number;     /* elevation # within volume scan */
  int     range_bin1;           /* range to first gate (m) */
  int     gate_size;            /* data gate size (or bin) size (m) */
  int     sector_number;        /* sector # within cut */
  int     prf;                  /* pulse repitition frequency (Hz) */
  float   rays_per_second;      /* average # of rays per second in sweep */
  float   unambiguous_range;    /* unambiguous range (km) */
  float   beam_width;           /* see "angle_units" */
  float   elevation;            /* see "angle_units" */
  float   system_calibration;   /* gain calibration constant */
  float   velocity_resolution;  /* Doppler velocity resolution */
  float   sweep_rate;           /* sweep rate (full sweeps/min) */
  float   azimuth_rate;         /* meaning unclear. A.K. 9/8/94 */
  float   fix_angle;            /* meaning unclear. A.K. 9/8/94 */
  float   pulse_count;          /* meaning unclear. A.K. 9/8/94 */
  float   pulse_width;          /* pulse width (micro-sec) */
  float   frequency;            /* bandwidth (MHz) */
  float   wavelength;           /* wavelength (m) */
  float   nyquist_velocity;     /* Nyquist velocity (m/s) */
  float   attenuation;          /* atmospheric attenuation factor */
  float   minimum_delta;        /* threshold parameter for minimum */
                                /* difference in echo power between two */
                                /* resolution volumes for them not to be */
                                /* labeled range ambiguous (W) */  
  char    time_units[50];       /* "GMT" or "LOCAL" */
  char    angle_units[50];      /* "RAD" or "DEG" */

          /* Variables that (may) change every ray follow */
  
  long    julian_day;           /* julian day when data was collected */
  int     year;                 /* e.g. 1994 */
  int     month;                /* January = 1, December = 12 */
  int     day;                  /* range: 1-31 */
  int     hour;                 /* range: 0-23 */
  int     minute;               /* range: 0-59 */
  float   second;               /* range: 0-59 */
  int     number;               /* # of this sweep/ray */
  int     ray_number;           /* full # of ray */
  int     nbins;                /* sweep: max, ray: actual */
  float   azimuth;              /* sweep: azimuth of first ray, ray: azimuth */
  int     nonzero;              /* # of the rays or bins that contain data */
  int     smallest_value;       /* smallest valid data value */
  int     largest_value;        /* largest valid data value */
  int     status;               /* see #defines above */
  long    warning_number;       /* warning flag */
  long    error_number;         /* error flag */

} sweep_header_t, ray_header_t;


                   /* Function prototypes follow */

void writeSweepHeader(FILE *fp,sweep_header_t sweep);
void readSweepHeader(FILE *fp,sweep_header_t *sweep);
void dumpAzimuth(FILE *fp,ray_header_t ray,int *value);
void readAzimuth(FILE *fp,sweep_header_t sweep,ray_header_t *ray,float *value);
void asciiEncode(FILE *fp,int *value,int ncodes);
int asciiDecode(FILE *fp,int * value, int ncodes);
float HMLF(int x);

#define _AIW_INCLUDED

#endif









