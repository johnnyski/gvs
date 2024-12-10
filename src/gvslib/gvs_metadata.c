/* 
 * gvs_metadata.c -
 *     Contains routines for filling HDF metadata fields for Level 2 software.
 *
 *
 *-----------------------------------------------------------------------------
 *
 * By: Thuy Nguyen
 *     Science Systems and Applications, Inc. (SSAI)
 *     NASA/TRMM Office
 *     nguyen@trmm.gsfc.nasa.gov
 *     5/13/97
 *
 *****************************************************************************/

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <math.h>

/* toolkit */
#include <IO.h>
#include "gvs_metadata.h"

#define MAX_CMD_LEN   256
#define MAX_LINE_LEN  300

/****************************************************************************/
/*                                                                          */
/*                              write_hdf_metadata_for_l2granule            */
/*                                                                          */
/****************************************************************************/
int write_hdf_metadata_for_l2granule(IO_HANDLE *fh, gv_metadata_t *metadata, int verbose)
{
  /* Write metadata fields to the opened HDF file. 
   * Return 1 upon successful; -1, otherwise.
   */
  if (fh == NULL || metadata == NULL) {
	if (verbose) fprintf(stderr, "Can't write metadata. fd or metadata is null\n");
	return -1;
  }
  /* specific to level 2 */

  /* Write fields that exists in the single VOS hdf file, to the granuled hdf 
   * via a call to write_hdf_metadata_for_l2vos().  Then
   * write additional metadata items (VIA MACRO's).
   */
  write_hdf_metadata_for_l2vos(fh, metadata, verbose);

  /* Write each metadata field to HDF file. */

  WRITE_METADATA_CHAR(fh, TK_GRANULE_ID, metadata->granule_id);
  WRITE_METADATA_CHAR(fh, TK_GV_RUNTIME_OPT, metadata->runtime_opt);
  WRITE_METADATA_INT(fh, TK_NUM_VOS, &(metadata->num_vos));
  WRITE_METADATA_CHAR(fh, TK_INPUT_FILES, metadata->input_files);
  WRITE_METADATA_CHAR(fh, TK_GEN_DATE_INPUT_FILES, metadata->gen_date_input_files);

  /* These were retrieved from qc file */

  WRITE_METADATA_INT(fh, TK_MISSING_DATA, &(metadata->missing_data));
  WRITE_METADATA_FLOAT(fh, TK_MIN_REF_THRESHOLD, &(metadata->min_ref_threshold));
  WRITE_METADATA_CHAR(fh, TK_RADAR_NAME, metadata->radar_name);
  WRITE_METADATA_CHAR(fh, TK_RADAR_CITY, metadata->radar_city);
  WRITE_METADATA_CHAR(fh, TK_RADAR_STATE, metadata->radar_state);
  WRITE_METADATA_CHAR(fh, TK_RADAR_COUNTRY, metadata->radar_country);
  WRITE_METADATA_FLOAT(fh, TK_RADAR_WAVELENGTH, &(metadata->radar_wavelength));
  WRITE_METADATA_FLOAT(fh, TK_NUM_DATA_GAP, &(metadata->num_data_gap));
  WRITE_METADATA_CHAR(fh, TK_PERCENT_BAD_MISS_PIXEL, metadata->percent_bad_miss_pixel);
  WRITE_METADATA_FLOAT(fh, TK_DATA_GAP, &(metadata->data_gap));
  WRITE_METADATA_INT(fh, TK_LEAP_SECS_FLAG, &(metadata->leap_secs_flag));

  if (strlen(metadata->anomaly_flag) == 0) {
	WRITE_METADATA_CHAR(fh, TK_ANOMALY_FLAG, ANOMALY_FLAG_DEFAULT);
  }
  else {
	WRITE_METADATA_CHAR(fh, TK_ANOMALY_FLAG, metadata->anomaly_flag);
  }
  if (verbose) fprintf(stderr, "Done writing metadata.\n");
  return 1;
} /* write_hdf_metadata_for_l2granule */

/****************************************************************************/
/*                                                                          */
/*                         read_hdf_metadata_from_l2granule                 */
/*                                                                          */
/****************************************************************************/ 
int read_hdf_metadata_from_l2granule(IO_HANDLE *fh, 
										gv_metadata_t *metadata, int verbose)
{
  
  if (fh == NULL || metadata == NULL) return -1;
  if (read_hdf_metadata_from_qc(fh, metadata, verbose) < 0) return -1;
  if (read_hdf_metadata_from_l2vos(fh, metadata, verbose) < 0) return -1;
  READ_METADATA_CHAR(fh, TK_GRANULE_ID, metadata->granule_id);
  READ_METADATA_CHAR(fh, TK_INPUT_FILES, metadata->input_files);
  READ_METADATA_CHAR(fh, TK_GEN_DATE_INPUT_FILES, metadata->gen_date_input_files);
  READ_METADATA_CHAR(fh, TK_GV_RUNTIME_OPT, metadata->runtime_opt);
  READ_METADATA_INT(fh, TK_NUM_VOS, &(metadata->num_vos));

  READ_METADATA_FLOAT(fh, TK_NUM_DATA_GAP, &(metadata->num_data_gap));
  READ_METADATA_CHAR(fh, TK_PERCENT_BAD_MISS_PIXEL, metadata->percent_bad_miss_pixel);
  READ_METADATA_FLOAT(fh, TK_DATA_GAP, &(metadata->data_gap));
  READ_METADATA_INT(fh, TK_LEAP_SECS_FLAG, &(metadata->leap_secs_flag));
  return 1;
} /* read_hdf_metadata_from_l2granule */


/****************************************************************************/
/*                                                                          */
/*                              write_hdf_metadata_for_l2vos                */
/*                                                                          */
/****************************************************************************/
int write_hdf_metadata_for_l2vos(IO_HANDLE *fh, gv_metadata_t *metadata, int verbose)
{
  /* Write metadata fields to the opened HDF file. 
   * Return 1 upon successful; -1, otherwise.
   */

  
  if (fh == NULL || metadata == NULL) {
	if (verbose) fprintf(stderr, "Can't write metadata. fd or metadata is null\n");
	return -1;
  }

  /* Write each metadata field to HDF file. */
  WRITE_METADATA_INT(fh, TK_BEGIN_DATE, &(metadata->begin_date));
  WRITE_METADATA_INT(fh, TK_BEGIN_TIME, &(metadata->begin_time));
  WRITE_METADATA_INT(fh, TK_END_DATE, &(metadata->end_date));
  WRITE_METADATA_INT(fh, TK_END_TIME, &(metadata->end_time));
  WRITE_METADATA_CHAR(fh, TK_DATA_ID, metadata->data_id);
  WRITE_METADATA_CHAR(fh, TK_CONTACT, metadata->contact);
  WRITE_METADATA_CHAR(fh, TK_ALGORITHM_VERSION, metadata->alg_version);
  WRITE_METADATA_CHAR(fh, TK_ALGORITHM_ID, metadata->alg_id);
  WRITE_METADATA_CHAR(fh, TK_SOFTWARE_VERSION, metadata->software_version);
  WRITE_METADATA_INT(fh, TK_PRODUCT_VERSION, &metadata->product_version);
  WRITE_METADATA_FLOAT(fh, TK_RADAR_SPACING_X, &(metadata->spacing_x));
  WRITE_METADATA_FLOAT(fh, TK_RADAR_SPACING_Y, &(metadata->spacing_y));
  WRITE_METADATA_FLOAT(fh, TK_RADAR_SPACING_Z, &(metadata->spacing_z));
  WRITE_METADATA_INT(fh, TK_RADAR_GRID_SIZE_X, &(metadata->grid_size_x));
  WRITE_METADATA_INT(fh, TK_RADAR_GRID_SIZE_Y, &(metadata->grid_size_y));
  WRITE_METADATA_INT(fh, TK_RADAR_GRID_SIZE_Z, &(metadata->grid_size_z));
  WRITE_METADATA_FLOAT(fh, TK_RADAR_ORIGIN_LAT, &(metadata->lat));
  WRITE_METADATA_FLOAT(fh, TK_RADAR_ORIGIN_LON, &(metadata->lon));
  WRITE_METADATA_FLOAT(fh, TK_RADAR_ORIGIN_ALT, &(metadata->alt));

  WRITE_METADATA_FLOAT(fh, TK_WEST_BOUND_COORD, &(metadata->west_bound_coord));
  WRITE_METADATA_FLOAT(fh, TK_EAST_BOUND_COORD, &(metadata->east_bound_coord));
  WRITE_METADATA_FLOAT(fh, TK_NORTH_BOUND_COORD, &(metadata->north_bound_coord));
  WRITE_METADATA_FLOAT(fh, TK_SOUTH_BOUND_COORD, &(metadata->south_bound_coord));
  WRITE_METADATA_FLOAT(fh, TK_LATITUDE_RES, &(metadata->lat_res));
  WRITE_METADATA_FLOAT(fh, TK_LONGITUDE_RES, &(metadata->lon_res));
  WRITE_METADATA_CHAR(fh, TK_GEO_COORD_UNITS, metadata->geo_coord_units);

  if (strlen(metadata->qa_param_value) != 0) {
	WRITE_METADATA_CHAR(fh, TK_QA_PARAM_VALUE, metadata->qa_param_value);
  }
  else {
	WRITE_METADATA_CHAR(fh, TK_QA_PARAM_VALUE, QA_PARAM_VALUE_DEFAULT);
  }
  if (strlen(metadata->repro_stat) != 0) {
	WRITE_METADATA_CHAR(fh, TK_REPRO_STAT, metadata->repro_stat);
  }
  else {
	WRITE_METADATA_CHAR(fh, TK_REPRO_STAT, REPRO_STAT_DEFAULT);
  }

  if (strlen(metadata->browse_name) != 0)
	WRITE_METADATA_CHAR(fh, TK_BROWSE_NAME, metadata->browse_name);

  if (strlen(metadata->data_center_src) != 0) {
	WRITE_METADATA_CHAR(fh, TK_DATA_CENTER_SRC, metadata->data_center_src);
  }
  else {
	WRITE_METADATA_CHAR(fh, TK_DATA_CENTER_SRC, DATA_CENTER_SRC_DEFAULT);
  }
  WRITE_METADATA_CHAR(fh, TK_MIN_MAX_UNITS, metadata->min_max_units);
  WRITE_METADATA_INT(fh, TK_NUM_VOS, &(metadata->num_vos));
  return 1;
} /* write_hdf_metadata_for_l2vos */

/****************************************************************************/
/*                                                                          */
/*                              read_hdf_metadata_from_l2vos                */
/*                                                                          */
/****************************************************************************/
int read_hdf_metadata_from_l2vos(IO_HANDLE *fh, 
						gv_metadata_t *metadata, int verbose)
{
  /* Read metadata fields from the opened HDF file. 
   * Return 1 upon successful; -1, otherwise.
   */

  
  if (fh == NULL || metadata == NULL) {
	if (verbose) fprintf(stderr, "Can't read metadata. fd or metadata is null\n");
	return -1;
  }
  /* Read each metadata field from HDF file. */
  READ_METADATA_INT(fh, TK_BEGIN_DATE, &(metadata->begin_date));
  READ_METADATA_INT(fh, TK_BEGIN_TIME, &(metadata->begin_time));
  READ_METADATA_INT(fh, TK_END_DATE, &(metadata->end_date));
  READ_METADATA_INT(fh, TK_END_TIME, &(metadata->end_time));

  READ_METADATA_CHAR(fh, TK_DATA_ID, metadata->data_id);
  READ_METADATA_CHAR(fh, TK_CONTACT, metadata->contact);
  READ_METADATA_CHAR(fh, TK_ALGORITHM_VERSION, metadata->alg_version);
  READ_METADATA_CHAR(fh, TK_ALGORITHM_ID, metadata->alg_id);
  READ_METADATA_CHAR(fh, TK_SOFTWARE_VERSION, metadata->software_version);
  READ_METADATA_INT(fh, TK_PRODUCT_VERSION, &metadata->product_version);
  READ_METADATA_FLOAT(fh, TK_RADAR_SPACING_X, &(metadata->spacing_x));
  READ_METADATA_FLOAT(fh, TK_RADAR_SPACING_Y, &(metadata->spacing_y));
  READ_METADATA_FLOAT(fh, TK_RADAR_SPACING_Z, &(metadata->spacing_z));
  READ_METADATA_INT(fh, TK_RADAR_GRID_SIZE_X, &(metadata->grid_size_x));
  READ_METADATA_INT(fh, TK_RADAR_GRID_SIZE_Y, &(metadata->grid_size_y));
  READ_METADATA_INT(fh, TK_RADAR_GRID_SIZE_Z, &(metadata->grid_size_z));
  READ_METADATA_FLOAT(fh,  TK_RADAR_ORIGIN_LON, &(metadata->lon));
  READ_METADATA_FLOAT(fh,  TK_RADAR_ORIGIN_LAT, &(metadata->lat));
  READ_METADATA_FLOAT(fh, TK_RADAR_ORIGIN_ALT, &(metadata->alt));

  READ_METADATA_FLOAT(fh, TK_WEST_BOUND_COORD, &(metadata->west_bound_coord));
  READ_METADATA_FLOAT(fh, TK_EAST_BOUND_COORD, &(metadata->east_bound_coord));
  READ_METADATA_FLOAT(fh, TK_NORTH_BOUND_COORD, &(metadata->north_bound_coord));
  READ_METADATA_FLOAT(fh, TK_SOUTH_BOUND_COORD, &(metadata->south_bound_coord));
  READ_METADATA_FLOAT(fh, TK_LATITUDE_RES, &(metadata->lat_res));
  READ_METADATA_FLOAT(fh, TK_LONGITUDE_RES, &(metadata->lon_res));
  READ_METADATA_CHAR(fh, TK_GEO_COORD_UNITS, metadata->geo_coord_units);

  READ_METADATA_CHAR(fh, TK_QA_PARAM_VALUE, metadata->qa_param_value);
  READ_METADATA_CHAR(fh, TK_REPRO_STAT, metadata->repro_stat);
  READ_METADATA_CHAR(fh, TK_BROWSE_NAME, metadata->browse_name);
  READ_METADATA_CHAR(fh, TK_DATA_CENTER_SRC, metadata->data_center_src);
  READ_METADATA_CHAR(fh, TK_MIN_MAX_UNITS, metadata->min_max_units);
  READ_METADATA_INT(fh, TK_NUM_VOS, &(metadata->num_vos));
  return 1;
} /* read_hdf_metadata_from_l2vos */


/****************************************************************************/
/*                                                                          */
/*                              read_hdf_metadata_from_qc                   */
/*                                                                          */
/****************************************************************************/
int read_hdf_metadata_from_qc(IO_HANDLE *fh, 
								gv_metadata_t *metadata, int verbose)
{
  /* Read metadata fields from the opened qc HDF file. 
   * Return 1 upon successful; -1, otherwise.
   */

  if (fh == NULL || metadata == NULL) {
	if (verbose) fprintf(stderr, "Can't read metadata. fd or metadata is null\n");
	return -1;
  }
  /* Read each metadata field to HDF file. */

  READ_METADATA_INT(fh,  TK_MISSING_DATA, &(metadata->missing_data));
  READ_METADATA_FLOAT(fh, TK_MIN_REF_THRESHOLD, &(metadata->min_ref_threshold));
  READ_METADATA_CHAR(fh, TK_RADAR_NAME, metadata->radar_name);
  READ_METADATA_CHAR(fh, TK_RADAR_CITY, metadata->radar_city);
  READ_METADATA_CHAR(fh, TK_RADAR_STATE, metadata->radar_state);
  READ_METADATA_CHAR(fh, TK_RADAR_COUNTRY, metadata->radar_country);
  READ_METADATA_FLOAT(fh, TK_RADAR_WAVELENGTH, &(metadata->radar_wavelength));
  return 1;
}  /* read_hdf_metadata_from_qc */

/****************************************************************************/
/*                                                                          */
/*                          set_l2_specific_metadata                        */
/*                                                                          */
/****************************************************************************/
int set_l2_specific_metadata(DATE_STR *vos_date, TIME_STR *vos_time, 
							 int xdim, int ydim, int zdim,
							 float xres, float yres, float zres,
							 float lat, float lon, float alt,
							 char *metadata_fname,
							 float min_unit, float max_unit,
							 char *alg_id, char *alg_version,
							 char *software_version,
							 char *contact_name, char *data_id,
							 gv_metadata_t *metadata)
{
  /* Clean metadata and fill metadata fields specific to level 2. 
   * Return 1 upon successful; -1, otherwise.
   */

  int km_lat_ratio = 111;        /* 111 km / 1 degree latitude */
  float xdegs_from_origin, ydegs_from_origin;
  int xbins_from_origin, ybins_from_origin;

  if (metadata == NULL) return -1;

  /* Use toolkit's default values if params are not set */
  if (vos_date != NULL) {
	/* Change 2 digit year to 4 digit year */
	if (vos_date->tkyear < 100) {
	  if (vos_date->tkyear > 60) 
		vos_date->tkyear += 1900;
	  else 
		vos_date->tkyear += 2000;
	}
	memcpy(&(metadata->begin_date), vos_date, sizeof(DATE_STR));
	memcpy(&(metadata->end_date), vos_date, sizeof(DATE_STR));
  }
  if (vos_time != NULL) {
	memcpy(&(metadata->begin_time), vos_time, sizeof(TIME_STR));
	memcpy(&(metadata->end_time), vos_time, sizeof(TIME_STR));
  }
  metadata->grid_size_x = xdim;
  metadata->grid_size_y = ydim;
  metadata->grid_size_z = zdim;
  metadata->spacing_x = (float) xres;                  /* km */
  metadata->spacing_y = (float) yres;                  /* km */
  metadata->spacing_z = (float) zres;                  /* km */
  metadata->lat = lat;
  metadata->lon = lon;
  metadata->alt = alt;

  /* The next two new lines are for correct bounding coordinates */
  metadata->lat_res = metadata->spacing_y / km_lat_ratio;
  metadata->lon_res = metadata->spacing_x / (km_lat_ratio*cos(lat*.0174533));
  metadata->lat_res = fabs(metadata->lat_res);
  metadata->lon_res = fabs(metadata->lon_res);

  strcpy(metadata->geo_coord_units, "Degrees/bin");
  /* Get x,y bins from the origin point.
   * Assuming the origin point is in the middle.
   */
  xbins_from_origin = (int) xdim / 2;
  ybins_from_origin = (int) ydim / 2; 
  /* Convert bins to degrees */
  xdegs_from_origin = xbins_from_origin * metadata->lon_res;
  ydegs_from_origin = ybins_from_origin * metadata->lat_res;

  /* bounding coordinates in degrees */
  metadata->east_bound_coord = lon + xdegs_from_origin;
  metadata->west_bound_coord = lon - xdegs_from_origin;
  metadata->north_bound_coord = lat + ydegs_from_origin;
  metadata->south_bound_coord = lat - ydegs_from_origin;

  sprintf(metadata->min_max_units, "(%f, %f)", (float)min_unit, (float)max_unit);
  
   strcpy(metadata->alg_id, alg_id);
   strcpy(metadata->data_id, data_id);
   strcpy(metadata->contact, contact_name);
   strcpy(metadata->alg_version, alg_version);
   strcpy(metadata->software_version, software_version);

  if (get_metadata_from_metadata_file(metadata_fname, vos_date, NULL,
									  metadata) < 0)
	return -1;
  return 1;
} /* set_l2_specific_metadata */


/****************************************************************************/
/*                                                                          */
/*                              write_hdf_metadata_for_l3granule            */
/*                                                                          */
/****************************************************************************/
int write_hdf_metadata_for_l3granule(IO_HANDLE *fh, gv_metadata_t *metadata, int verbose)
{
  /* Write metadata fields to the opened level 3 HDF file. 
   * Return 1 upon successful; -1, otherwise.
   */
  if (fh == NULL || metadata == NULL) {
	if (verbose) fprintf(stderr, "Can't write metadata. fd or metadata is null\n");
	return -1;
  }

  /* Write each metadata field to HDF file. */

  WRITE_METADATA_CHAR(fh, TK_GRANULE_ID, metadata->granule_id);
  WRITE_METADATA_INT(fh, TK_BEGIN_DATE, &(metadata->begin_date));
  WRITE_METADATA_INT(fh, TK_BEGIN_TIME, &(metadata->begin_time));
  WRITE_METADATA_INT(fh, TK_END_DATE, &(metadata->end_date));
  WRITE_METADATA_INT(fh, TK_END_TIME, &(metadata->end_time));

  WRITE_METADATA_CHAR(fh, TK_RADAR_NAME, metadata->radar_name);
  WRITE_METADATA_CHAR(fh, TK_RADAR_CITY, metadata->radar_city);
  WRITE_METADATA_CHAR(fh, TK_RADAR_STATE, metadata->radar_state);
  WRITE_METADATA_CHAR(fh, TK_RADAR_COUNTRY, metadata->radar_country);
  WRITE_METADATA_FLOAT(fh, TK_RADAR_WAVELENGTH, &(metadata->radar_wavelength));
  WRITE_METADATA_INT(fh, TK_LEAP_SECS_FLAG, &(metadata->leap_secs_flag));
  WRITE_METADATA_CHAR(fh, TK_DATA_ID, metadata->data_id);
  WRITE_METADATA_CHAR(fh, TK_CONTACT, metadata->contact);
  WRITE_METADATA_CHAR(fh, TK_ALGORITHM_VERSION, metadata->alg_version);
  WRITE_METADATA_CHAR(fh, TK_ALGORITHM_ID, metadata->alg_id);
  WRITE_METADATA_CHAR(fh, TK_SOFTWARE_VERSION, metadata->software_version);
  WRITE_METADATA_INT(fh, TK_PRODUCT_VERSION, &metadata->product_version);
  WRITE_METADATA_FLOAT(fh, TK_RADAR_SPACING_X, &(metadata->spacing_x));
  WRITE_METADATA_FLOAT(fh, TK_RADAR_SPACING_Y, &(metadata->spacing_y));
  WRITE_METADATA_FLOAT(fh, TK_RADAR_SPACING_Z, &(metadata->spacing_z));
  WRITE_METADATA_INT(fh, TK_RADAR_GRID_SIZE_X, &(metadata->grid_size_x));
  WRITE_METADATA_INT(fh, TK_RADAR_GRID_SIZE_Y, &(metadata->grid_size_y));
  WRITE_METADATA_INT(fh, TK_RADAR_GRID_SIZE_Z, &(metadata->grid_size_z));
  WRITE_METADATA_FLOAT(fh, TK_RADAR_ORIGIN_LAT, &(metadata->lat));
  WRITE_METADATA_FLOAT(fh, TK_RADAR_ORIGIN_LON, &(metadata->lon));
  WRITE_METADATA_FLOAT(fh, TK_RADAR_ORIGIN_ALT, &(metadata->alt));

  WRITE_METADATA_FLOAT(fh, TK_WEST_BOUND_COORD, &(metadata->west_bound_coord));
  WRITE_METADATA_FLOAT(fh, TK_EAST_BOUND_COORD, &(metadata->east_bound_coord));
  WRITE_METADATA_FLOAT(fh, TK_NORTH_BOUND_COORD, &(metadata->north_bound_coord));
  WRITE_METADATA_FLOAT(fh, TK_SOUTH_BOUND_COORD, &(metadata->south_bound_coord));
  WRITE_METADATA_FLOAT(fh, TK_LATITUDE_RES, &(metadata->lat_res));
  WRITE_METADATA_FLOAT(fh, TK_LONGITUDE_RES, &(metadata->lon_res));
  WRITE_METADATA_CHAR(fh, TK_GEO_COORD_UNITS, metadata->geo_coord_units);

  if (strlen(metadata->qa_param_value) != 0) {
	WRITE_METADATA_CHAR(fh, TK_QA_PARAM_VALUE, metadata->qa_param_value);
  }
  else {
	WRITE_METADATA_CHAR(fh, TK_QA_PARAM_VALUE, QA_PARAM_VALUE_DEFAULT);
  }
  if (strlen(metadata->repro_stat) != 0) {
	WRITE_METADATA_CHAR(fh, TK_REPRO_STAT, metadata->repro_stat);
  }
  else {
	WRITE_METADATA_CHAR(fh, TK_REPRO_STAT, REPRO_STAT_DEFAULT);
  }

  if (strlen(metadata->browse_name) != 0)
	WRITE_METADATA_CHAR(fh, TK_BROWSE_NAME, metadata->browse_name);

  if (strlen(metadata->anomaly_flag) == 0) {
	WRITE_METADATA_CHAR(fh, TK_ANOMALY_FLAG, ANOMALY_FLAG_DEFAULT);
  }
  else {
	WRITE_METADATA_CHAR(fh, TK_ANOMALY_FLAG, metadata->anomaly_flag);
  }
  if (verbose) fprintf(stderr, "Done writing metadata.\n");
  return 1;
} /* write_hdf_metadata_for_l3granule */
/****************************************************************************/
/*                                                                          */
/*                              read_hdf_metadata_for_l3granule             */
/*                                                                          */
/****************************************************************************/
int read_hdf_metadata_from_l3granule(IO_HANDLE *fh, gv_metadata_t *metadata, int verbose)
{
  /* Read metadata fields from the opened level 3 HDF file. 
   * Return 1 upon successful; -1, otherwise.
   */
  if (fh == NULL || metadata == NULL) {
	if (verbose) fprintf(stderr, "Can't write metadata. fd or metadata is null\n");
	return -1;
  }

  /* Read each metadata field from HDF file. */

  READ_METADATA_CHAR(fh, TK_GRANULE_ID, metadata->granule_id);
  READ_METADATA_INT(fh, TK_BEGIN_DATE, &(metadata->begin_date));
  READ_METADATA_INT(fh, TK_BEGIN_TIME, &(metadata->begin_time));
  READ_METADATA_INT(fh, TK_END_DATE, &(metadata->end_date));
  READ_METADATA_INT(fh, TK_END_TIME, &(metadata->end_time));

  READ_METADATA_CHAR(fh, TK_RADAR_NAME, metadata->radar_name);
  READ_METADATA_CHAR(fh, TK_RADAR_CITY, metadata->radar_city);
  READ_METADATA_CHAR(fh, TK_RADAR_STATE, metadata->radar_state);
  READ_METADATA_CHAR(fh, TK_RADAR_COUNTRY, metadata->radar_country);

  READ_METADATA_INT(fh, TK_LEAP_SECS_FLAG, &(metadata->leap_secs_flag));
  READ_METADATA_CHAR(fh, TK_DATA_ID, metadata->data_id);
  READ_METADATA_CHAR(fh, TK_CONTACT, metadata->contact);
  READ_METADATA_CHAR(fh, TK_ALGORITHM_VERSION, metadata->alg_version);
  READ_METADATA_CHAR(fh, TK_ALGORITHM_ID, metadata->alg_id);
  READ_METADATA_CHAR(fh, TK_SOFTWARE_VERSION, metadata->software_version);
  READ_METADATA_INT(fh, TK_PRODUCT_VERSION, &metadata->product_version);
  READ_METADATA_FLOAT(fh, TK_RADAR_SPACING_X, &(metadata->spacing_x));
  READ_METADATA_FLOAT(fh, TK_RADAR_SPACING_Y, &(metadata->spacing_y));
  READ_METADATA_FLOAT(fh, TK_RADAR_SPACING_Z, &(metadata->spacing_z));
  READ_METADATA_INT(fh, TK_RADAR_GRID_SIZE_X, &(metadata->grid_size_x));
  READ_METADATA_INT(fh, TK_RADAR_GRID_SIZE_Y, &(metadata->grid_size_y));
  READ_METADATA_INT(fh, TK_RADAR_GRID_SIZE_Z, &(metadata->grid_size_z));
  READ_METADATA_FLOAT(fh, TK_RADAR_ORIGIN_LAT, &(metadata->lat));
  READ_METADATA_FLOAT(fh, TK_RADAR_ORIGIN_LON, &(metadata->lon));
  READ_METADATA_FLOAT(fh, TK_RADAR_ORIGIN_ALT, &(metadata->alt));

  READ_METADATA_FLOAT(fh, TK_WEST_BOUND_COORD, &(metadata->west_bound_coord));
  READ_METADATA_FLOAT(fh, TK_EAST_BOUND_COORD, &(metadata->east_bound_coord));
  READ_METADATA_FLOAT(fh, TK_NORTH_BOUND_COORD, &(metadata->north_bound_coord));
  READ_METADATA_FLOAT(fh, TK_SOUTH_BOUND_COORD, &(metadata->south_bound_coord));
  READ_METADATA_FLOAT(fh, TK_LATITUDE_RES, &(metadata->lat_res));
  READ_METADATA_FLOAT(fh, TK_LONGITUDE_RES, &(metadata->lon_res));
  READ_METADATA_CHAR(fh, TK_GEO_COORD_UNITS, metadata->geo_coord_units);


  READ_METADATA_CHAR(fh, TK_QA_PARAM_VALUE, metadata->qa_param_value);
  READ_METADATA_CHAR(fh, TK_REPRO_STAT, metadata->repro_stat);
  READ_METADATA_CHAR(fh, TK_BROWSE_NAME, metadata->browse_name);
  READ_METADATA_CHAR(fh, TK_DATA_CENTER_SRC, metadata->data_center_src);



  if (verbose) fprintf(stderr, "Done reading metadata.\n");
  return 1;
} /* read_hdf_metadata_from_l3granule */

/************************************************************************/
/*                                                                      */
/*                          read_l3hdf_metadata_from_l2granule          */
/*                                                                      */
/************************************************************************/
int read_l3hdf_metadata_from_l2granule(IO_HANDLE *fh, 
									   gv_metadata_t *metadata, int verbose)
{
  /* Read some of level 3 metadata fields from level 2 granule HDF file.
   * Return 1 for successful; -1, otherwise.
   */
  if (fh == NULL || metadata == NULL) return -1;

  READ_METADATA_CHAR(fh, TK_RADAR_NAME, metadata->radar_name);
  READ_METADATA_CHAR(fh, TK_RADAR_CITY, metadata->radar_city);
  READ_METADATA_CHAR(fh, TK_RADAR_STATE, metadata->radar_state);
  READ_METADATA_CHAR(fh, TK_RADAR_COUNTRY, metadata->radar_country);
  READ_METADATA_FLOAT(fh, TK_RADAR_WAVELENGTH, &(metadata->radar_wavelength));
  READ_METADATA_INT(fh, TK_LEAP_SECS_FLAG, &(metadata->leap_secs_flag));

  READ_METADATA_FLOAT(fh, TK_RADAR_SPACING_X, &(metadata->spacing_x));
  READ_METADATA_FLOAT(fh, TK_RADAR_SPACING_Y, &(metadata->spacing_y));
  READ_METADATA_FLOAT(fh, TK_RADAR_SPACING_Z, &(metadata->spacing_z));
  READ_METADATA_INT(fh, TK_RADAR_GRID_SIZE_X, &(metadata->grid_size_x));
  READ_METADATA_INT(fh, TK_RADAR_GRID_SIZE_Y, &(metadata->grid_size_y));
  READ_METADATA_INT(fh, TK_RADAR_GRID_SIZE_Z, &(metadata->grid_size_z));
  READ_METADATA_FLOAT(fh, TK_RADAR_ORIGIN_LAT, &(metadata->lat));
  READ_METADATA_FLOAT(fh, TK_RADAR_ORIGIN_LON, &(metadata->lon));
  READ_METADATA_FLOAT(fh, TK_RADAR_ORIGIN_ALT, &(metadata->alt));

  READ_METADATA_FLOAT(fh, TK_WEST_BOUND_COORD, &(metadata->west_bound_coord));
  READ_METADATA_FLOAT(fh, TK_EAST_BOUND_COORD, &(metadata->east_bound_coord));
  READ_METADATA_FLOAT(fh, TK_NORTH_BOUND_COORD, &(metadata->north_bound_coord));
  READ_METADATA_FLOAT(fh, TK_SOUTH_BOUND_COORD, &(metadata->south_bound_coord));
  READ_METADATA_FLOAT(fh, TK_LATITUDE_RES, &(metadata->lat_res));
  READ_METADATA_FLOAT(fh, TK_LONGITUDE_RES, &(metadata->lon_res));
  READ_METADATA_CHAR(fh, TK_GEO_COORD_UNITS, metadata->geo_coord_units);

  return 1;
}

/************************************************************************/
/*                                                                      */
/*                         get_metadata_from_metadata_file              */
/*                                                                      */
/************************************************************************/
int get_metadata_from_metadata_file(char *metadata_file, 
									DATE_STR *begin_date, DATE_STR *end_date,
									gv_metadata_t *metadata)
{
  /*
   * Get metadata from metadata file.
   * It will call 'grep_runtime_parm.pl' with begin_date first, if nothing
   * is matched, it calls it with the end_date.
   *
   * The format for metadata_file (contain min and hour inorder to 
   * use 'grep_runtime_parm.pl'):
   *  The format of each line is:
   *   MIN HOUR DAY MONTH YEAR PARAMETERS
   *
   *   where:
   #      MIN        : 0 - 59
   #      HOUR       : 0 - 23
   #      DAY        : 1 - 31
   #      MONTH      : 1 - 12
   #      YEAR       : 1997...
   #      PARAMETERS : 
   #                   {-Q qa_param_value}
   *                   {-R reprocess_status}
   *                   [-c data_center_source]
   *                   [-P product_version_number]
   *
   *     Note: Use double quote for each param value.
   *  
   * i.e. * * * * * -R "Yes" -P 2.0
   *
   # You can specify:
   #    *              --  Wild match.  This matches all possibilities.
   #    n-m            --  Range.  n=begin, m=end, inclusive.
   #    Exact values   --  Exact time specification.
   #
   * Return 1 for successful; -1, otherwise.
   */  
  char cmd[MAX_CMD_LEN], line[MAX_LINE_LEN];
  char *token;
  FILE *fp;
  DATE_STR *date;
  enum {Q = 1, R = 2};
  int required_options = Q|R;  /* All required options */
  int specified_options = 0;

  if (metadata_file == NULL || metadata == NULL) 
	return -1;

  if (begin_date != NULL) date = begin_date;
  else if (end_date != NULL) date = end_date;
  else return -1;
  memset(cmd, '\0', MAX_CMD_LEN);  
  sprintf(cmd, "grep_runtime_parm.pl %d/%d/%d < %s", 
		  date->tkmonth, date->tkday, date->tkyear, metadata_file);
  /*  fprintf(stderr,"GREP1: cmd=<%s>\n", cmd); */
  if ((fp = (FILE *) popen(cmd, "r")) == NULL) return -1;
  memset(line, '\0', MAX_LINE_LEN);
  fgets(line, MAX_LINE_LEN, fp);
  pclose(fp);
  if (strlen(line) == 0) {
	/* Try with different end date. */
	if (begin_date && end_date != NULL) {
	  date = end_date;
	  memset(cmd, '\0', MAX_CMD_LEN);
	  sprintf(cmd, "grep_runtime_parm.pl %d/%d/%d < %s", 
			  date->tkmonth, date->tkday, date->tkyear, metadata_file);
	  /*	  fprintf(stderr,"GREP2: cmd=<%s>\n", cmd); */
	  if ((fp = (FILE *) popen(cmd, "r")) == NULL) return -1;
	  memset(line, '\0', MAX_LINE_LEN);
	  fgets(line, MAX_LINE_LEN, fp);
	  pclose(fp);
	}
	else return -1;
  }

  /*  fprintf(stderr, "TOKIZE metadata.txt file: <%s>\n", line); */
  token = strtok(line, " ");
  while (token != NULL) {
	if (strcmp(token, "-Q") == 0) {
	  token = strtok(NULL, "\"");
	  if (token == NULL)
		goto ERROR;
	  strcpy(metadata->qa_param_value, token);
	  specified_options |= Q;
	}
	else if (strcmp(token, "-R") == 0) {
	  token = strtok(NULL, "\"");
	  if (token == NULL)
		goto ERROR;
	  strcpy(metadata->repro_stat, token);
	  specified_options |= R;
	}
	else if (strcmp(token, "-c") == 0) {
	  token = strtok(NULL, "\"");
	  if (token == NULL)
		goto ERROR;
	  strcpy(metadata->data_center_src, token);
	}
	else if (strcmp(token, "-P") == 0) {
	  token = strtok(NULL, "\"");
	  if (token == NULL)
		goto ERROR;
	  sscanf(token, "%d", &metadata->product_version);
	}
	token = strtok(NULL, " ");
  }
  if ((specified_options & required_options) == 0) {
	fprintf(stderr, "Not all required options were specified on the metadata file.\n");
	goto ERROR;
  }
  return 1;
ERROR:
  fprintf(stderr, "Unable to parse \" token for metadata parameter.\n");
  return -1;
} /* get_metadata_from_metadata_file */

