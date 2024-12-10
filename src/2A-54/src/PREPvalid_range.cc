/*****************************************************************************
 *   The function 'PREPvalid_range' determines a mask to blend out ranges,
 *   where physically no data were recorded by the radar (e.g., in the
 *   center and the corners of the domain) or simply ranges that should not
 *   get included to the analysis.
 *
 *   Version:  Matthias Steiner, October 1994
 *   Modified: Joe Tenerelli, July, 1994
 *
 *   Usage:   PREPvalid_range()
 *
 *****************************************************************************/

#include <stdio.h>                        /* standard input - output         */
#include <stdlib.h>                       /* standard library functions      */
#include <math.h>                         /* mathematical functions          */
#include <volume.h>                       /* general header for volume       */

int Volume::PREPvalid_range(void)
{
    int i, j;                             /* loop indices                    */
    int icp, jcp;                         /* indices of grid center point    */
    int count_1, count_0;                 /* counts valid/no-valid pixels    */
    float x_range_sq, y_range_sq;         /* x,y distance to center squared  */
    float range;                          /* range from grid center point    */

/*  Determine the mask to blend out the "no valid data" ranges:
 *   - ranges with "valid data"     ->  1
 *   - ranges with "no valid data"  ->  0
 *  -----------------------------------------------------------              */
    icp = params.xydim / 2;
    jcp = params.xydim / 2;
    count_1 = 0;
    count_0 = 0;
    float min_range2 = params.min_range * params.min_range;
    float max_range2 = params.max_range * params.max_range;
    for (j = 0; j < params.xydim; ++j) {
	y_range_sq = (jcp - j) * params.xyres * (jcp - j) * params.xyres;
	for (i = 0; i < params.xydim; ++i) {
	    x_range_sq = (icp - i) * params.xyres * (icp - i) * params.xyres;
	    range = x_range_sq + y_range_sq;
	    if ((range >= min_range2) && (range < max_range2)) {
		grids.range_mask[j][i] = 1;
		++count_1;
	    }
	    else {
		grids.range_mask[j][i] = 0;
		++count_0;
	    }
	}
    }
    fprintf(params.fpout1, "   Valid data range mask: valid pixels =%7d",
	    count_1);
    fprintf(params.fpout1, "   no-valid pixels =%7d\n\n", count_0);

    return 0;
}
