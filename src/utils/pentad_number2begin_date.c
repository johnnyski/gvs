/*
 *
 * pentad_number2begin_date.c
 *     Get begin date for the pentad number.  Outputs the result to stdout.
 *
 *-----------------------------------------------------------------------------
 *
 * By: Thuy Nguyen
 *     Science Systems and Applications, Inc. (SSAI)
 *     NASA/TRMM Office
 *     nguyen@trmm.gsfc.nasa.gov
 *     10/1/97
 *
 */

#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gv_utils.h>
#include <IO.h>

main (int argc, char *argv[])
{
  int pentad;
  DATE_STR begin_date;
  int i, year;

  if (argc != 3) {
  USAGE:
	fprintf(stderr,"Usage (%s)\n", PROG_VERSION);
	fprintf(stderr, " %s pentad_number yyyy\n", argv[0]);
	fprintf(stderr, "  The result 'mm dd yyyy' will be output to stdout.\n");
	exit(-1);
  }
  i = 1;
  pentad = atoi(argv[i++]);
  year = atoi(argv[i++]);

  if (year < 1900 || pentad <= 0 || pentad > 73) exit(-1);

  pentad_number_to_datesNtimes(pentad, year, &begin_date, NULL, NULL, NULL);
  fprintf(stdout, "%d %d %d", begin_date.tkmonth, begin_date.tkday, begin_date.tkyear);
  exit(0);
}
