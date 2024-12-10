/*
 * gvs_metadata.h
 *
 *-----------------------------------------------------------------------------
 *
 * By: Thuy Nguyen
 *     NASA/TRMM Office
 *     Thuy.Nguyen@gsfc.nasa.gov
 *     5/13/97
 */

#ifndef __GVS_METADATA_H__
#define __GVS_METADATA_H__ 1

/* tsdis toolkit */
#include <IO.h>


#define MAX_NAME_LEN                  51
#define MAX_MID_NAME_LEN              101
#define MAX_LONG_NAME_LEN             301
#define MAX_VERY_LONG_NAME_LEN        TK_SZ_ELEMENT_1001

#define DATA_CENTER_SRC_DEFAULT    "TSDIS"
#define REPRO_STAT_DEFAULT         "YES"
#define QA_PARAM_VALUE_DEFAULT     "NOT BEING INVESTIGATED"
#define ANOMALY_FLAG_DEFAULT       "NOT EMPTY"


#define P2A53_SOFTWARE_VERSION  "gvs-v4.21"
#define P2A53_ALG_ID        "2A53"
#define P2A53_DATA_ID       "2A53"
#define P2A53_ALG_VERSION   "2.0"
#define P2A53_CONTACT_NAME  "Sandra Yuter, UW and John H. Merritt, NASA/TO"
#define P2A53_MIN_UNIT      0.0
#define P2A53_MAX_UNIT      100.0
#define P2A53_ALT           0.0
#define P2A53_XDIM          151
#define P2A53_YDIM          151
#define P2A53_ZDIM          0
#define P2A53_XRES          2.0
#define P2A53_YRES          2.0
#define P2A53_ZRES          0.0

/* 2A54 */
#define P2A54_ALG_ID        "2A54"
#define P2A54_DATA_ID       "2A54"
#define P2A54_ALG_VERSION   "1"
#define P2A54_SOFTWARE_VERSION  "gvs-v4.21"
#define P2A54_CONTACT_NAME  "Matthias Steiner et al., University of Washington"
#define P2A54_MIN_UNIT      -99.0
#define P2A54_MAX_UNIT      2.0
#define P2A54_ALT           0.0
#define P2A54_XYDIM         151
#define P2A54_ZDIM          0
#define P2A54_XYRES         2.0
#define P2A54_ZRES          0.0

/* 2A55 */
#define P2A55_ALG_ID        "2A55"
#define P2A55_DATA_ID       "2A55"
#define P2A55_ALG_VERSION   "1"
#define P2A55_SOFTWARE_VERSION  "gvs-v4.21"
#define P2A55_CONTACT_NAME  "Matthias Steiner et al., University of Washington"
#define P2A55_MIN_UNIT      -15.0
#define P2A55_MAX_UNIT      70.0
#define P2A55_ALT           1.5
#define P2A55_ZDIM          13
#define P2A55_ZRES          1.5
#define P2A55_XYDIM         151
#define P2A55_XYRES         2.0

/* 3A-53 */
#define P3A53_ALG_ID        "3A53"
#define P3A53_DATA_ID       "3A53"
#define P3A53_ALG_VERSION   "1"
#define P3A53_CONTACT_NAME  "Stacy Brodzik"

/* 3A-54  */
#define P3A54_ALG_ID        "3A54"
#define P3A54_DATA_ID       "3A54"
#define P3A54_ALG_VERSION   "1"
#define P3A54_CONTACT_NAME  "Stacy Brodzik"

typedef char name_t[MAX_NAME_LEN];
typedef char mid_name_t[MAX_MID_NAME_LEN];
typedef char long_name_t[MAX_LONG_NAME_LEN];
typedef char very_long_name_t[MAX_VERY_LONG_NAME_LEN];
typedef struct gv_metadata {

  DATE_STR begin_date;
  TIME_STR begin_time;
  DATE_STR end_date;
  TIME_STR end_time;
  name_t   granule_id;            /* Output filename  */
  name_t   data_id;
  name_t   contact;
  name_t   alg_version;           /* Version # of the algorithm starting at 1
								   * at launch time.
								   */
  name_t   software_version;      /* Hardcoded for each program. */
  int      product_version;       /* Specified as additional runtime params. */
  /* See /usr/local/trmm/GVBOX/data/2A54_metadata.txt */
  long int missing_data;          /* For level 2 products only. */  
  float    min_ref_threshold;     /* For level 2 products only. */
  name_t   alg_id;
  long_name_t   input_files; 
  name_t   gen_date_input_files;
  name_t   radar_name;     
  name_t   radar_city;     
  name_t   radar_state;     
  name_t   radar_country;
  float    radar_wavelength;      
  long int num_vos;               /* For level 2 products only. */
  float    lat;
  float    lon;
  float    alt;
  float    spacing_x;
  float    spacing_y;
  float    spacing_z;
  long int grid_size_x;
  long int grid_size_y;
  long int grid_size_z;
  very_long_name_t   runtime_opt;

  float    west_bound_coord;
  float    east_bound_coord;
  float    north_bound_coord;
  float    south_bound_coord;
  float    lat_res;
  float    lon_res;
  name_t   geo_coord_units;
  name_t   qa_param_value;
  name_t   repro_stat;   
  name_t   browse_name;            /* TK should set this in TSDIS env --
									*  Don't know about in DP env.
									*/
  float    num_data_gap;           /* For level 2 products only. */
  name_t   percent_bad_miss_pixel; /* For level 2 products only. */
  float    data_gap;               /* For level 2 products only. */
  int      leap_secs_flag;         /* 0=no; 1=yes */
  name_t   min_max_units;          /* For level 2 products only. */
  name_t   data_center_src;        /* Data center source of input files for l2 */
  mid_name_t anomaly_flag;   /* "EMPTY: GENERATED AFTER SOFTWARE ERROR"
							  * "EMPTY: NO DATA DUE TO NO RAIN"
							  * "EMPTY: NO DATA RECORDED"
							  * "EMPTY: DATA RECORDED BUT STILL MISSING"
							  * "EMPTY: REASON UNKNOWN"
							  * "NOT EMPTY: POSSIBLE PROBLEM"
							  * "NOT EMPTY"
							  */

} gv_metadata_t;


/* define macro */
#define WRITE_METADATA_CHAR(fh, field, value) \
          if (TKwriteMetadataChar(fh, field, (char *) value) == TK_FAIL) { \
			if (verbose) fprintf(stderr, "Failed to write metadata: %s.\n", field); \
			return -1; \
		  }

#define WRITE_METADATA_INT(fh, field, value) \
          if (TKwriteMetadataInt(fh, field, value) == TK_FAIL) { \
			if (verbose) fprintf(stderr, "Failed to write metadata: %s.\n", field); \
			return -1; \
		  }

#define WRITE_METADATA_FLOAT(fh, field, value) \
          if (TKwriteMetadataFloat(fh, field, (float *) value) == TK_FAIL) { \
			if (verbose) fprintf(stderr, "Failed to write metadata: %s.\n", field); \
			return -1; \
		  }

/* Read */
#define READ_METADATA_CHAR(fh, field, value) \
          if (TKreadMetadataChar(fh, field, (char *) value) == TK_FAIL) { \
			if (verbose) fprintf(stderr, "Failed to read metadata: %s.\n", field); \
			return -1; \
		  }

#define READ_METADATA_INT(fh, field, value) \
          if (TKreadMetadataInt(fh, field,  value) == TK_FAIL) { \
			if (verbose) fprintf(stderr, "Failed to read metadata: %s.\n", field); \
			return -1; \
		  }

#define READ_METADATA_FLOAT(fh, field, value) \
          if (TKreadMetadataFloat(fh, field, (float *) value) == TK_FAIL) { \
			if (verbose) fprintf(stderr, "Failed to read metadata: %s.\n", field); \
			return -1; \
		  }

int write_hdf_metadata_for_l2vos(IO_HANDLE *fh, gv_metadata_t *metadata, int verbose);
int read_hdf_metadata_from_l2vos(IO_HANDLE *fh, 
								gv_metadata_t *metadata, int verbose);
int read_hdf_metadata_from_qc(IO_HANDLE *fh, 
								gv_metadata_t *metadata, int verbose);
void get_today_date_str(char *date_str);
int set_l2_specific_metadata(DATE_STR *vos_date, TIME_STR *vos_time, 
							 int xdim, int ydim,	int zdim,
							 float xres, float yres, float zres,
							 float lat, float lon, float alt,
							 char *metadata_fname,
							 float min_unit, float max_unit,
							 char *alg_id, char *alg_version,
							 char *software_version,
							 char *contact_name, char *data_id,
							 gv_metadata_t *metadata);
int read_hdf_metadata_from_l2granule(IO_HANDLE *fh, 
										gv_metadata_t *metadata, int verbose);
int write_hdf_metadata_for_l2granule(IO_HANDLE *fh, gv_metadata_t *metadata, int verbose);
int write_hdf_metadata_for_l3granule(IO_HANDLE *fh, gv_metadata_t *metadata, int verbose);
int get_metadata_from_metadata_file(char *metadata_file, 
									DATE_STR *begin_date, DATE_STR *end_date,
									gv_metadata_t *metadata);
int read_l3hdf_metadata_from_l2granule(IO_HANDLE *fh, 
									   gv_metadata_t *metadata, int verbose);

int write_rrmap2hdf(float rrmap[151][151], 
					 int mm, int dd, int yy,
					 int hour, int min, int sec, 
					 int xdim, int ydim,
					 float xres, float yres,
					 float lat, float lon, float alt,
					 char *metadata_fname,
					 int verbose, int write_metadata,
					 char *hdf_filename);
#endif








