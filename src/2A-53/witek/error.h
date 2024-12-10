#ifndef _ERROR_INCLUDED
#define _ERROR_INCLUDED

#define HEAD_NO_WARN                   (long)(0)
#define HEAD_NO_ERR                    (long)(0)

#define HEAD_YEAR_WARN                 (long)(1L<<0)
#define HEAD_MONTH_WARN                (long)(1L<<1)
#define HEAD_DAY_WARN                  (long)(1L<<2)
#define HEAD_JULIAN_DAY_WARN           (long)(1L<<3)
#define HEAD_SECTOR_NUMBER_WARN        (long)(1L<<4)
#define HEAD_RANGE_BIN_WARN            (long)(1L<<5)
#define HEAD_GATE_SIZE_WARN            (long)(1L<<6)
#define HEAD_PRF_WARN                  (long)(1L<<7)
#define HEAD_UNAMBIGUOUS_RANGE_WARN    (long)(1L<<8)
#define HEAD_VELOCITY_RESOLUTION_WARN  (long)(1L<<9)
#define HEAD_SWEEP_RATE_WARN           (long)(1L<<10)
#define HEAD_SWEEP_AZIMUTH_RATE_WARN   (long)(1L<<11)
#define HEAD_FIX_ANGLE_WARN            (long)(1L<<12)
#define HEAD_PULSE_COUNT_WARN          (long)(1L<<13)
#define HEAD_PULSE_WIDTH_WARN          (long)(1L<<14)
#define HEAD_MINIMUM_DELTA_WARN        (long)(1L<<15)

#define HEAD_INVALID_MONTH_ERR         (long)(1L<<0)
#define HEAD_INVALID_DAY_ERR           (long)(1L<<1)
#define HEAD_INVALID_HOUR_ERR          (long)(1L<<2)
#define HEAD_INVALID_MINUTE_ERR        (long)(1L<<3)
#define HEAD_INVALID_SECOND_ERR        (long)(1L<<4)
#define HEAD_FREQUENCY_ERR             (long)(1L<<5)
#define HEAD_WAVELENGTH_ERR            (long)(1L<<6)
#define HEAD_NYQUIST_ERR               (long)(1L<<7)
#define HEAD_ATTENUATION_ERR           (long)(1L<<8)
#define HEAD_SYSTEM_CALIBRATION_ERR    (long)(1L<<9)
#define HEAD_BEAM_WIDTH_ERR            (long)(1L<<10)
#define HEAD_ELEVATION_ERR             (long)(1L<<11)
#define HEAD_NHEADER_LINES_ERR         (long)(1L<<12)
#define HEAD_HEADER_TYPE_ERR           (long)(1L<<13)
#define HEAD_RADAR_ID_ERR              (long)(1L<<14)
#define HEAD_RADAR_NAME_ERR            (long)(1L<<15)
#define HEAD_TYPE_OF_DATA_ERR          (long)(1L<<16)
#define HEAD_NSWEEPS_ERR               (long)(1L<<17)
#define HEAD_NRAYS_ERR                 (long)(1L<<18)
#define HEAD_ELEVATION_NUMBER_ERR      (long)(1L<<19)
#define HEAD_SMALLEST_VALUE_ERR        (long)(1L<<20)
#define HEAD_LARGEST_VALUE_ERR         (long)(1L<<21)
#define HEAD_STATUS_ERR                (long)(1L<<22)
#define HEAD_TIME_ERR                  (long)(1L<<23)
#define HEAD_ANGLE_ERR                 (long)(1L<<24)

void printError(FILE* fp,long error_number);
void printWarning(FILE* fp,long warning_number);

#endif


