#include <stdio.h>
#include "error.h"

static struct {
  long number;
  char *text;
} warning[] =  {
  {HEAD_YEAR_WARN,"year"},
  {HEAD_MONTH_WARN,"month"},
  {HEAD_DAY_WARN,"day"},
  {HEAD_JULIAN_DAY_WARN,"julian_day"},
  {HEAD_SECTOR_NUMBER_WARN,"sector_number"},
  {HEAD_RANGE_BIN_WARN,"range_bin"},
  {HEAD_GATE_SIZE_WARN,"gate_size"},
  {HEAD_PRF_WARN,"prf"},
  {HEAD_UNAMBIGUOUS_RANGE_WARN,"unambiguous_range"},
  {HEAD_VELOCITY_RESOLUTION_WARN,"velocity_resolution"},
  {HEAD_SWEEP_RATE_WARN,"sweep_rate"},
  {HEAD_SWEEP_AZIMUTH_RATE_WARN,"sweep_azimuth_rate"},
  {HEAD_FIX_ANGLE_WARN,"fix_angle"},
  {HEAD_PULSE_COUNT_WARN,"pulse_count"},
  {HEAD_PULSE_WIDTH_WARN,"pulse_width"},
  {HEAD_MINIMUM_DELTA_WARN,"minimum_delta"}};


static struct {
  long number;
  char *text;
} error[] =  {
  {HEAD_INVALID_MONTH_ERR,"invalid_month"},
  {HEAD_INVALID_DAY_ERR,"invalid_day"},
  {HEAD_INVALID_HOUR_ERR,"invalid_hour"},
  {HEAD_INVALID_MINUTE_ERR,"invalid_minute"},
  {HEAD_INVALID_SECOND_ERR,"invalid_second"},
  {HEAD_FREQUENCY_ERR,"frequency"},
  {HEAD_WAVELENGTH_ERR,"wavelength"},
  {HEAD_NYQUIST_ERR,"nyquist"},
  {HEAD_ATTENUATION_ERR,"attenuation"},
  {HEAD_SYSTEM_CALIBRATION_ERR,"system_calibration"},
  {HEAD_BEAM_WIDTH_ERR,"beam_width"},
  {HEAD_ELEVATION_ERR,"elevation"},
  {HEAD_NHEADER_LINES_ERR,"nheader_lines"},
  {HEAD_HEADER_TYPE_ERR,"header_type"},
  {HEAD_RADAR_ID_ERR,"radar_id"},
  {HEAD_RADAR_NAME_ERR,"radar_name"},
  {HEAD_TYPE_OF_DATA_ERR,"type_of_data"},
  {HEAD_NSWEEPS_ERR,"nsweeps"},
  {HEAD_NRAYS_ERR,"nrays"},
  {HEAD_ELEVATION_NUMBER_ERR,"elevation_number"},
  {HEAD_SMALLEST_VALUE_ERR,"smallest_value"},
  {HEAD_LARGEST_VALUE_ERR,"largest_value"},
  {HEAD_STATUS_ERR,"status"},
  {HEAD_TIME_ERR,"time"},
  {HEAD_ANGLE_ERR,"angle"}};



void printError(FILE* fp,long error_number)
{
  int  i,n;
  
  n = sizeof(error)/(sizeof(error[0].number)+sizeof(error[0].text));
  for (i=0;i<=n-1;i++){
    if (error_number & (1L << i))
      fprintf(fp,"ERROR: %s\n",error[i].text);
  }
}

void printWarning(FILE* fp,long warning_number)
{
  int  i,n;
  
  n = sizeof(error)/(sizeof(warning[0].number)+sizeof(warning[0].text));
  for (i=0;i<=n-1;i++){
    if (warning_number & (1L << i))
      fprintf(fp,"WARNING: %s\n",warning[i].text);
  }
}









