/*****************************************************************************
 *   The function 'READgauge' reads the coordinates from the raingauges
 *   that are used for radar-calibrational purposes.
 *
 *   Version: Matthias Steiner, June 1994
 *   Version: Joe Tenerelli, July 1994
 *
 *   Usage:   READgauge()
 *
 *****************************************************************************/

#include <stdio.h>                        /* standard input - output         */
#include <stdlib.h>                       /* standard library functions      */
#include <math.h>                         /* mathematical functions          */
#include <volume.h>                       /* general header for volume       */
extern "C" {
   #include "exitCodes.h"
}

int Volume::READgauge(void)
{
    FILE *fpin2;                          /* input-2 file                    */

    char rest_of_line[MAX_LINE+1];        /* used to read rest of lines      */

    int i, k;                             /* loop indices                    */
    int test;                             /* test variable                   */
    char *file = params.inputRainGaugeSitesFn;

/*  Open the input-2 file (= raingauge sites):
 *  ------------------------------------------                               */
    if ((fpin2 = fopen(file, "r")) == NULL) {
        fprintf(stderr, "Sorry, can't open input file %s\n", file);
		TKreportWarning(W_2A54_CANT_OPEN_RAINGUAGE);
        exit(FAILED_CODE2);
    }

/*  Read the coordinates of the raingauge sites:
 *  --------------------------------------------                             */
    for (k = 0; k < 6; ++k) {
	fgets(rest_of_line, MAX_LINE, fpin2);
    }
    for (i = 0; i < MAX_SITES; ++i) {
        fgets(site[i].name, MAX_NAME+1, fpin2);
        test = fscanf(fpin2, "%s %d %d %f %f %f", site[i].shortName,
                      &site[i].dir, &site[i].dist, &site[i].x,
                      &site[i].y, &site[i].rain);
        if (test != 6) break;
        fgets(rest_of_line, MAX_LINE, fpin2);
    }
    params.isite = i;

    return 0;
}
