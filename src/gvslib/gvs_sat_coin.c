/* 
 * gvs_sat_coin.c: Contain utility routines.
 *
 *
 *-----------------------------------------------------------------------------
 *
 * By: Thuy Nguyen
 *     NASA/TRMM Office
 *     Thuy.Nguyen@gsfc.nasa.gov
 *     6/16/97
 *
 *****************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include <IO.h>
#include <gv_utils.h>
#include "gvs_sat_coin.h"

/****************************************************************************/
/*                                                                          */
/*                        do_query_coinc_info                               */
/*                                                                          */
/****************************************************************************/
int do_query_coinc_info(int site_id, DATE_STR *sdate, TIME_STR *stime, 
						DATE_STR *edate, TIME_STR *etime, int verbose,
						char *sat_coinc_dir, char *ca_date_str,
						char *ca_time_str, float *ca_distance)
{
  /* Get satellite coincident info based on start/end date, 
   * start/end time (Start/end date should be  on the same day), 
   * site_id, and sat_coinc_file (CT.yymmdd) associated to the start or end
   * date in sat_coinc_dir. If sate_coinc_dir is not passed in, will query from
   * TSDIS' database instead of from that file.
   * Will set ca_date_str (yyyy-mm-dd), ca_time_str (hh:mm:ss.sss), 
   * ca_distance if there is a hit.
   * Return 0 if no hit found; 1 if there is a hit; -1 if error occurred.
   */
  
  char *sat_coinc_fname = NULL, tmp_name[MAX_FILENAME_LEN];
  int rc;

  if (sdate == NULL || stime == NULL ||
	  edate == NULL || etime == NULL ||
	  ca_date_str == NULL || ca_time_str == NULL || ca_distance == NULL) 
	return -1;

  /* Get satellite coinc file for the start/end date if sat_coinc_dir != NULL. */
  if (sat_coinc_dir != NULL && strlen(sat_coinc_dir) > 0) {
	memset(tmp_name, '\0', MAX_FILENAME_LEN);
	sprintf(tmp_name, "%s/CT.%.2d%.2d%.2d", sat_coinc_dir, 
			sdate->tkyear % 100, sdate->tkmonth, sdate->tkday);
	sat_coinc_fname = tmp_name;
  }
  if (verbose) {
	fprintf(stderr, "sat_coinc_fname: %s\n", sat_coinc_fname);
	fprintf(stderr, "TKqueryGVCoincidenceInfo with: sat_coinc_fname: %s,  date/time start: %d-%d-%d/%d:%d:%d end: %d-%d-%d/%d:%d:%d, site id: %d.\n", sat_coinc_fname, sdate->tkyear, sdate->tkmonth, sdate->tkday, stime->tkhour, stime->tkminute, stime->tksecond, edate->tkyear, edate->tkmonth, edate->tkday, etime->tkhour, etime->tkminute, etime->tksecond, site_id);
  }


  /* Get Closest area's distance, date, and time if hit exists. */
  rc = TKqueryGVCoincidenceInfo(site_id, sdate, stime, 
								edate, etime, sat_coinc_fname,
								ca_date_str, ca_time_str, ca_distance);

  if (rc != TK_SUCCESS && rc != TK_NO_RECORD) {
	return -1;
  }
  else if (rc == TK_NO_RECORD) {
	return 0;
  }

  return 1;
} /* do_query_coinc_info */

/****************************************************************************/
/*                                                                          */
/*                        get_sat_coinc_info                                */
/*                                                                          */
/****************************************************************************/
int get_sat_coinc_info(int site_id, DATE_STR *vos_date, TIME_STR *vos_time,
					   int verbose,
					   char *sat_coinc_dir, char *ca_date_str,
					   char *ca_time_str, float *ca_distance, int *hit)
{
  /* Get satellite coincident info based on vos_date, vos_time (+/-30 minutes),
   * site_id, and sat_coinc_file(s).  If sate_coinc_dir is not passed in, 
   * will query from TSDIS' database instead of from that file. 
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

  DATE_STR svos_date, evos_date;
  TIME_STR svos_time, evos_time;
  int rc = 0;

  if (vos_date == NULL || vos_time == NULL ||
	  ca_date_str == NULL || ca_time_str == NULL || ca_distance == NULL ||
	  hit == NULL) return -1;

  /* 
   * Use time offset of +/-30 minutes. 
   *   start time = vos time - 30 min.
   *   end time   = vos time + 30 min.
   */
  memcpy(&svos_date, vos_date, sizeof(DATE_STR));
  memcpy(&evos_date, vos_date, sizeof(DATE_STR));
  memcpy(&svos_time, vos_time, sizeof(TIME_STR));
  memcpy(&evos_time, vos_time, sizeof(TIME_STR));
  
  adjust_dateNtime(&svos_date, &svos_time, -30 * 60);
  adjust_dateNtime(&evos_date, &evos_time, +30 * 60);

  *hit = 1;	

#define TEST 0
#if TEST
  svos_date.tkday = 2;
  svos_date.tkmonth = 10;
  svos_date.tkyear = 1992;

  svos_time.tkhour = 23;
  svos_time.tkminute = 59;
  svos_time.tksecond = 00;

  evos_date.tkday = 3;
  evos_date.tkmonth = 10;
  evos_date.tkyear = 1992;

  evos_time.tkhour = 0;
  evos_time.tkminute = 20;
  evos_time.tksecond = 00;

  
#endif
  rc = do_query_coinc_info(site_id, &svos_date, &svos_time, 
								&evos_date, &evos_time, verbose, sat_coinc_dir,
								ca_date_str, ca_time_str, ca_distance);
  if (rc == -1)
	return -1;
  else if (rc == 0) {
	if (sat_coinc_dir != NULL) {
	  /* Do another query with the second file if the VOS' end time occurs
	   * on the next day of the start time.
	   */
	  if (svos_date.tkday != evos_date.tkday) {
		/* Reset the begin date/time */
		memcpy(&svos_date, &evos_date, sizeof(DATE_STR));
		svos_time.tkhour = 0;
		svos_time.tkminute = 0;
		svos_time.tksecond = 0;
		rc = do_query_coinc_info(site_id, &svos_date, &svos_time, 
								&evos_date, &evos_time, verbose, sat_coinc_dir,
								ca_date_str, ca_time_str, ca_distance);
		if (rc == -1) 
		  return -1;
		else if (rc == 1)
		  return 1;
	  }
	}
	/* No hit occurs */
	*hit = 0;
	strcpy(ca_time_str, DEFAULT_CA_TIME_STR);
	strcpy(ca_date_str, DEFAULT_CA_DATE_STR);
	*ca_distance = DEFAULT_CA_DISTANCE;
  }
  return 1;
} /* get_sat_coinc_info */

/****************************************************************************/
/*                                                                          */
/*                        get_sat_coinc_info2                               */
/*                                                                          */
/****************************************************************************/
int get_sat_coinc_info2(int site_id, DATE_STR sdate, TIME_STR stime, 
						DATE_STR edate, TIME_STR etime, int verbose,
						char *sat_coinc_dir, char *ca_date_str,
						char *ca_time_str, float *ca_distance)

{
  /* Get satellite coincident info based on sdate, stime (- 30 minutes)
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

  int rc = 0;

  if (ca_date_str == NULL || ca_time_str == NULL || ca_distance == NULL)
	return -1;
  /* 
   * Use time offset of +/-30 minutes. 
   *   start time = start time - 30 min.
   *   end time   = end time + 30 min.
   */

  adjust_dateNtime(&sdate, &stime, -30*60);
  adjust_dateNtime(&edate, &etime, 30*60);

  rc = do_query_coinc_info(site_id, &sdate, &stime, 
								&edate, &etime, verbose, sat_coinc_dir,
								ca_date_str, ca_time_str, ca_distance);
  if (rc == -1)
	return -1;
  else if (rc == 0) {
	if (sat_coinc_dir != NULL) {
	  /* Do another query with the second file if the VOS' end time occurs
	   * on the next day of the start time.
	   */
	  if (sdate.tkday != edate.tkday) {
		/* Reset the begin date/time */
		memcpy(&sdate, &edate, sizeof(DATE_STR));
		stime.tkhour = 0;
		stime.tkminute = 0;
		stime.tksecond = 0;
		rc = do_query_coinc_info(site_id, &sdate, &stime, 
								&edate, &etime, verbose, sat_coinc_dir,
								ca_date_str, ca_time_str, ca_distance);
		if (rc == -1) 
		  return -1;
		else if (rc == 1)
		  return 1; /* Hit */
	  }
	}
  }
  else 
	return 1; /* hit */
  return 0;
} /* get_sat_coinc_info2 */

