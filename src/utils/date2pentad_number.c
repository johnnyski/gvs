/*
 * date2pentad_number.c
 *        Converts date to pentad number. Outputs the result to stdout.
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


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gv_utils.h"

main (int argc, char *argv[])
{
  int yy, mm, dd, i;
  int pentad;

  if (argc != 4) {
  USAGE:
	fprintf(stderr,"Usage (%s)\n", PROG_VERSION);
	fprintf(stderr, " %s dd mm yyyy\n", argv[0]);
	fprintf(stderr, "  The result will be output to stdout.\n");
	exit(-1);
  }
  i = 1;

  dd = atoi(argv[i++]);
  mm = atoi(argv[i++]);
  yy = atoi(argv[i++]);

  if (dd < 0 || mm < 0 || yy < 0) exit(-1);

  pentad = date2pentad_number(mm, dd, yy);
  fprintf(stdout, "%d", pentad);
  exit(0);
}
