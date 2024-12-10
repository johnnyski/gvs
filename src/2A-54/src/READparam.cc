/*****************************************************************************
 *   The function 'READparam' reads the parameters necessary to analyze
 *   the radar volume scan in a customized way.
 *
 *   Version: Matthias Steiner, June 1994
 *   Version: Joe Tenerelli, July, 1994
 *
 *   Usage:   READparam()
 *
 *****************************************************************************/

#include <stdio.h>                        /* standard input - output         */
#include <stdlib.h>                       /* standard library functions      */
#include <math.h>                         /* mathematical functions          */
#include <volume.h>                       /* general header for volume       */

extern "C" {
   #include "exitCodes.h"
}

int Volume::READparam(void)
{
    float refl;
    FILE *fpin4;                          /* input-4 file                    */

    char rest_of_line[MAX_LINE+1];        /* used to read rest of lines      */

    int i, k;                             /* loop indices                    */
    int test;                             /* test variable                   */

/*  Open the input-5 file (= valid data range information):
 *  -------------------------------------------------------               */
    {
      FILE *fpin5;
      if ((fpin5 = fopen(params.inputValidRangeFn, "r")) == NULL) {
	fprintf(stderr, "Sorry, can't open input file %s\n",
		params.inputValidRangeFn);
	TKreportWarning(W_2A54_OPEN_VALIDRANGE_IFILE_FAILED);
	exit(FAILED_CODE2);
      }
      test = fscanf(fpin5, "%f %f", &params.min_range, &params.max_range);
      
      if (test != 2) {
	fprintf(stderr, "Problem with valid data range file %s!\n",
		params.inputValidRangeFn);
	TKreportWarning(W_2A54_INVALID_FORMAT_VALIDRANGE_IFILE);
	exit(FAILED_CODE3);
      }
    }

/*  Open the input-4 file (= Z-R conversion information):
 *  -----------------------------------------------------                    */
    char *file = params.inputZRTranFn;

    if ((fpin4 = fopen(params.inputZRTranFn, "r")) == NULL) {
        fprintf(stderr, 
		"Sorry, can't open input file %s\n", params.inputZRTranFn);
	TKreportWarning(W_2A54_OPEN_ZRTRAN_IFILE_FAILED);
        exit(FAILED_CODE2);
      }
    fprintf(params.fpout1, "Input parameters:\n");
    fprintf(params.fpout1, "-----------------\n");
    
/*  Read which functions should be used for the analysis:
 *  -----------------------------------------------------                    */
    for (k = 0; k < NUM_ANAL_INDICES; ++k) {
	scanf("%d", &params.anal_ind[k]);
    }

    if ((params.xydim > MAX_X_DIM) || (params.xydim > MAX_Y_DIM) || (params.zdim > MAX_Z_DIM)) {
        fprintf(stderr, "Grid dimensions too big!\n");
		TKreportWarning(W_2A54_GRID_DIM_TOO_BIG);
        exit(FAILED_CODE3);
    }
    fprintf(params.fpout1, "   Grid dimensions (x/y/z):  %3d/%3d/%3d\n\n", params.xydim,
	    params.xydim, params.zdim);

/*  Read the height level for the convective/stratiform separation
 *  and for the rainfall analysis:
 *  --------------------------------------------------------------           */
   scanf("%d", &params.level);
    
    if ((0 >= params.level) || (params.level >= params.zdim))
      {
	fprintf(stderr, "Invalid analysis level: must be 0 <= level < %d!\n",
		params.zdim);
	TKreportWarning(W_2A54_INVALID_ANAL_LEVEL);
	exit(FAILED_CODE3);
      }

    fprintf(params.fpout1, "   Height-level in use: %4.1f km",
	    params.zmin + params.level * params.zres);

/*  Read how the surface rainfall should be estimated:
 *  --------------------------------------------------                       */
    scanf("%d", &params.maxtype);
    if ((params.maxtype != 1) && (params.maxtype != 0) &&
	(params.maxtype != 2)) {
	fprintf(stderr, "Invalid maxtype: must be either 0 or 1!\n");
	TKreportWarning(W_2A54_INVALID_MAXTYPE_LEVEL);
	exit(FAILED_CODE3);
    }
    if (params.maxtype == 1) {
	fprintf(params.fpout1, "     (Maximum projection for surface rainfall)\n\n");
    }
    else {
	fprintf(params.fpout1, "\n\n");
    }

/*  Read how radar reflectivity should be converted to rainfall intensity:
 *  ----------------------------------------------------------------------   */
    scanf("%d", &params.ZRtype);
    fprintf(params.fpout1, "   Z-R conversion: ZRtype = %d", params.ZRtype);
    if (params.ZRtype == 0) {
	test = fscanf(fpin4, "%f %f", &params.ZR_a, &params.ZR_b);
	if (test != 2) {
	    fprintf(stderr, "Problem with Z-R conversion file %s!\n", file);
		TKreportWarning(W_2A54_INVALID_FORMAT_ZRTRAN_IFILE);
	    exit(FAILED_CODE3);
	}
	fprintf(params.fpout1, "   %s\n\n", file);
    }
    else if (params.ZRtype == 1) {
	test = fscanf(fpin4, "%f %f", &params.ZR_a_conv, &params.ZR_b_conv);
	if (test != 2) {
	    fprintf(stderr, "Problem with Z-R conversion file %s!\n", file);
		TKreportWarning(W_2A54_INVALID_FORMAT_ZRTRAN_IFILE);
	    exit(FAILED_CODE3);
	}
	test = fscanf(fpin4, "%f %f", &params.ZR_a_stra, &params.ZR_b_stra);
	if (test != 2) {
	    fprintf(stderr, "Problem with Z-R conversion file %s!\n", file);
		TKreportWarning(W_2A54_INVALID_FORMAT_ZRTRAN_IFILE);
	    exit(FAILED_CODE3);
	}
	fprintf(params.fpout1, "   %s\n\n", file);
    }
    else if (params.ZRtype == 2) {
	for (k = 0; k < 3; ++k) {
	    fgets(rest_of_line, MAX_LINE, fpin4);
	}
	for (k = 0; k < MAX_FILE; ++k) {
	    test = fscanf(fpin4, "%d %f %f %f", &i,
			  &params.Rvalue[k], &refl,
			  &params.Zvalue[k]);
	    if (test != 4) break;
	}
	params.ZR_WPMMdim = k;
	fprintf(params.fpout1, "   %s\n\n", file);
    }
    else {
	fprintf(stderr, "Invalid type of Z-R conversion!\n");
	TKreportWarning(W_2A54_INVALID_ZRTRAN_TYPE);
	exit(FAILED_CODE3);
    }

    fclose(fpin4);

/*  Read how the convective/stratiform separation should be done:
 *  ---------------------------------------------------------------------    */
    scanf("%d", &params.csAlgorithm);
    scanf("%f", &params.csSimpleSimonThreshold);

/*  Read how the radar estimates should be determined at the gauge sites:
 *  ---------------------------------------------------------------------    */
    scanf("%d", &params.radar_gauge_type);
    if ((params.radar_gauge_type != 0) && (params.radar_gauge_type != 1) &&
	(params.radar_gauge_type != 2)) {
	fprintf(stderr,
		"Invalid determination of radar estimates at gauge sites!\n");
	TKreportWarning(W_2A54_INVALID_RADAR_EST_GS);
	exit(FAILED_CODE3);
    }
    if (params.radar_gauge_type != 0) {
	scanf("%f", &params.max_radius);
    }
    if (params.max_radius <= 0.) {
	params.radar_gauge_type = 0;
    }
    else {
	if (params.max_radius <= (0.1 + params.xyres)) {
	    params.max_radius = 0.1 + params.xyres;
	}
    }
    fprintf(params.fpout1, "   Radar estimates at the gauge sites:");
    if (params.radar_gauge_type == 0) {
	fprintf(params.fpout1, "   closest grid point\n\n");
    }
    else if (params.radar_gauge_type == 1) {
	fprintf(params.fpout1,
		"   linear average within%5.1f km\n\n", params.max_radius);
    }
    else {
	fprintf(params.fpout1,
		"   maximum value within%5.1f km\n\n", params.max_radius);
    }
    return 0;
}
