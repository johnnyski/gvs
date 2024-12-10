/*
 * gvs_sat_coin.h
 *
 */

#ifndef __GVS_sat_coin_H__
#define __GVS_sat_coin_H__ 1


#define MAX_FILENAME_LEN 256

/* Satellite Coincident info. */
#define DEFAULT_CA_TIME_STR     "NULL"
#define DEFAULT_CA_DATE_STR     "NULL"
#define DEFAULT_CA_DISTANCE     -9999.9


/* get_sat_coinc_info:
 * Get satellite coincident info based on vos_date, vos_time (+/-30 minutes),
 * site_id, and sat_coinc_file(s).  If sate_coinc_dir is not passed in, will 
 * query from TSDIS' database instead of from that file.
 *
 * Will set ca_date_str, ca_time_str, ca_distance, and hit.
 *
 * Return 1 for successful; -1, otherwise.
 *
 * Note: Since satellite coincidence file contains coincidence data for
 *       one day, 2 satellite coincidence files (file of the previous day
 *       will be used if the vos_time is closed to the beginning of the day;
 *       file of the next day will be used if the vos_time is closed to the
 *       end of the day) may be required to do lookup for VOS having 
 *       begin/end times (+/- 30 mins) occur in two different days.
 *       
 *       The second sat. coin. file will be checked if no hit has been found
 *       in the first file (Note: the first searched file may either be
 *       the file from the previous day or from the specified day).
 *
 *       Satellite coincidence filename: CT.yymmdd.
 */
int get_sat_coinc_info(int site_id, DATE_STR *vos_date, TIME_STR *vos_time,
					   int verbose,
					   char *sat_coinc_dir, char *ca_date_str,
					   char *ca_time_str, float *ca_distance, int *hit);

  /* get_sat_coinc_info2:
   * Get satellite coincident info based on sdate, stime (- 30 minutes)
   * edate/etime (+30 minutes),
   * site_id, and sat_coinc_file(s).  If sate_coinc_dir is not passed in, 
   * will query from TSDIS' database instead of from that file. 
   * Will set ca_date_str, ca_time_str, ca_distance, and hit.
   *
   * Note: This routine is unlike the get_sat_coinc_info() that
   *       it takes in the end date/time.
   *
   * Return 0 if no hit found; 1 if there is a hit; -1 if error occurred.
   *
   * Note: Since satellite coincidence file contains coincidence data for
   *       one day, 2 satellite coincidence files (file of the previous day
   *       will be used if the vos_time is closed to the beginning of the day;
   *       file of the next day will be used if the vos_time is closed to the
   *       end of the day) may be required to do lookup for VOS having 
   *       begin/end times (+/- 30 mins) occur in two different days.
   *       
   *       The second sat. coin. file will be checked if no hit has been found
   *       in the first file (Note: the first searched file may either be
   *       the file from the previous day or from the specified day).
   *
   *       Satellite coincidence filename: CT.yymmdd.
   */
int get_sat_coinc_info2(int site_id, DATE_STR sdate, TIME_STR stime, 
						DATE_STR edate, TIME_STR etime, int verbose,
						char *sat_coinc_dir, char *ca_date_str,
						char *ca_time_str, float *ca_distance);

/* do_query_coinc_info: 
 * Get satellite coincident info based on start/end date, 
 * start/end time (START/END DATE SHOULD BE ON THE SAME DAY), 
 * site_id, and sat_coinc_file (CT.yymmdd) associated to the start or end
 * date in sat_coinc_dir. If sate_coinc_dir is not passed in, will query from
 * TSDIS' database instead of from that file.
 * Will set ca_date_str (yyyy-mm-dd), ca_time_str (hh:mm:ss.sss),
 * ca_distance, if there is a hit.
 *
 * Return 0 if no hit found; 1 if there is a hit; -1 if error occurred.
 */
int do_query_coinc_info(int site_id, DATE_STR *svos_date, TIME_STR *svos_time, 
						DATE_STR *evos_date, TIME_STR *evos_time, int verbose,
						char *sat_coinc_dir, char *ca_date_str,
						char *ca_time_str, float *ca_distance);


#endif
