
#include <stdio.h>                        /* standard input - output         */
#include <stdlib.h>                       /* standard library functions      */
#include <math.h>                         /* mathematical functions          */
#include <volume.h>                       /* general header for volume       */
#include <findPoints.h>
#include <sys/types.h>

extern "C"
{
    #include "exitCodes.h"    
    void srand48(long seedval);
    pid_t  getpid(void);
    double drand48(void);
}

#define BG_RADIUS      11.0               /* background radius [km]          */
#define THRESHOLD      40.0               /* reflectivity threshold [dBZ]    */
#define SH94_PAR      180.0               /* conv/stra demarcation parameter
                                           * (Steiner + Houze)               */
#define SH94_AREA_FAC   0.0               /* convective area scaling factor
                                           * (Steiner + Houze)               */
#define ZR_B_FACTOR     1.25              /* fixed power factor of Z-R
                                           * relationship used for conv/stra
                                           * separation (Churchill + Houze)  */

/*****************************************************************************
 *   The function 'ANALconv_straSimpleSimon' separates a gridded horizontal
 *   radar reflectivity echo pattern into its convective, stratiform,
 *   and no-rain elements.
 *
 *   This function performs a simple convective/stratiform separation based
 *   upon a simple dBZ threshold which the user can set.
 *****************************************************************************/
int Volume::ANALconv_stra(void)
{
    if (params.csAlgorithm == 0)
      {
	  return(ANALconv_straCH84orSH94());
      }
    else if (params.csAlgorithm == 1)
      {
	  return(ANALconv_straSimpleSimon());
      }
    else if (params.csAlgorithm == 2)
      {
	  return(ANALconv_straRandom());
      }
    else
      {
	  fprintf(stderr,
		  "Error: No convective/stratiform separation algorithm.\n");
	  TKreportWarning(W_2A54_NO_CONV_STRAT_SEP_ALG);
	  exit(FAILED_CODE2); 
      }
    return(1);
}

/*****************************************************************************
 *   The function 'ANALconv_straSimpleSimon' separates a gridded horizontal
 *   radar reflectivity echo pattern into its convective, stratiform,
 *   and no-rain elements.
 *
 *   This function performs a simple convective/stratiform separation based
 *   upon a simple dBZ threshold which the user can set.
 *****************************************************************************/
int Volume::ANALconv_straSimpleSimon(void)
{
  // This function does a simple convective/stratiform separation
  // on the reflmap working map.
  // The Simple Simon algorithm uses a variable dBZ cutoff
  // below which the rain type is stratiform.  Otherwise
  // the rain type is convective unless the reflectivity is missing, in
  // which case the rain type is no echo.

    int i = 0;
    int j = 0;

    fprintf(params.fpout1, "Convective/stratiform separation:\n");
    fprintf(params.fpout1, "---------------------------------\n");
    fprintf(params.fpout1,
	    "   Simple Simon scheme is used (threshold = %5.2f dBZ)\n\n",
	    params.csSimpleSimonThreshold);
  
    for (j = 0; j < params.xydim; j++) {
	for (i = 0; i < params.xydim; i++) {
	    float val = grids.reflmap[j][i];
	    if (val <= MISSINGV) {
		grids.rain_type[j][i] = 0;
	    }
	    else if (val < params.csSimpleSimonThreshold) {
		grids.rain_type[j][i] = 1;
	    }
	    else {
		grids.rain_type[j][i] = 2;
	    }
	}
    }

/*  Check data on "valid range":
 *  ----------------------------                                             */
//    int jj = 0;
    for (j = 0; j < params.xydim; ++j) {
	for (i = 0; i < params.xydim; ++i) {
	    if (grids.range_mask[j][i] == 0) {
		grids.rain_type[j][i] = MISSINGV;
/*
		if (grids.rain_type[j][i] != 0) {
		    grids.rain_type[j][i] = 0;
		    ++jj;
		}
*/
	    }
	}
    }
/*
    if (jj != 0) {
	fprintf(params.fpout1, "   ATTENTION: Index jj not 0!");
	fprintf(params.fpout1, " Please check on valid range.\n\n");
    }
*/

/*  OUTPUT: Convective/stratiform map (TRMM GT Product #2A-54):
 *  -----------------------------------------------------------              */
    fprintf(params.fpout1, "   Convective/stratiform map:  %s\n\n",
	    params.outputConvStraMapFn);

    return(1);
}

/*****************************************************************************
 *   The function 'ANALconv_straRandom' separates a gridded horizontal
 *   radar reflectivity echo pattern into its convective, stratiform,
 *   and no-rain elements.
 *
 *   This function performs a simple convective/stratiform separation based
 *   upon a simple random number generator which provides an 50%
 *   probability of convective, 50% stratiform.
 *****************************************************************************/
int Volume::ANALconv_straRandom(void)
{
  /// Initialize the random number generator.
    srand48(getpid());

  // This function does a randomly generated convective/stratiform separation
  // on the reflmap working map.

    int i = 0; 
    int j = 0;

    fprintf(params.fpout1, "Convective/stratiform separation:\n");
    fprintf(params.fpout1, "---------------------------------\n");
    fprintf(params.fpout1,
	    "   Random Generator scheme is used\n\n");
  
    for (j = 0; j < params.xydim; j++) {
	for (i = 0; i < params.xydim; i++) {
	    float val = grids.reflmap[j][i];
	    if (val <= MISSINGV) {
		grids.rain_type[j][i] = 0;
	    }
	    else {
		double rn = drand48();
		if (rn < 0.5) {
		    grids.rain_type[j][i] = 1;
		}
		else {
		    grids.rain_type[j][i] = 2;
		}
	    }
	}
    }

/*  Check data on "valid range":
 *  ----------------------------                                             */
//    int jj = 0;
    for (j = 0; j < params.xydim; ++j) {
	for (i = 0; i < params.xydim; ++i) {
	    if (grids.range_mask[j][i] == 0) {
		grids.rain_type[j][i] = MISSINGV;
/*
		if (grids.rain_type[j][i] != 0) {
		    grids.rain_type[j][i] = 0;
		    ++jj;
		}
*/
	    }
	}
    }
/*
    if (jj != 0) {
	fprintf(params.fpout1, "   ATTENTION: Index jj not 0!");
	fprintf(params.fpout1, " Please check on valid range.\n\n");
    }
*/

/*  OUTPUT: Convective/stratiform map (TRMM GT Product #2A-54):
 *  -----------------------------------------------------------              */
    fprintf(params.fpout1, "   Convective/stratiform map:  %s\n\n",
	    params.outputConvStraMapFn);

    return(1);
}

/*****************************************************************************
 *   The function 'ANALconv_straCH84orSH94' separates a gridded horizontal
 *   radar
 *   reflectivity echo pattern into its convective, stratiform, and no-rain
 *   elements.
 *
 *   The basic idea is to look for reflectivity intensity cores (relative
 *   maxima in a given background area). The applied method is adapted from
 *   Churchill and Houze (JAS 1984); however, it has been further refined
 *   to work on a 2-km resolution grid and tuned towards a more accurate
 *   convective/stratiform separation. The method works on the gridded
 *   radar reflectivity field, which enables the use of different Z-R
 *   relations for different types of precipitation elements. For further
 *   details, see Steiner, Houze and Yuter (JAM 1995).
 *
 *   ATTENTION: The convective/stratiform separation algorithms are
 *              sensitive to the grid resolution of the data!
 *              Therefore,
 *                 if (xyres == 2.)       ->  Steiner and Houze scheme (SH94)
 *                 else if (xyres == 4.)  ->  Churchill and Houze scheme (CH84)
 *                 else                   ->  stop processing
 *
 *   Version:  Matthias Steiner, June 1994
 *   Modified: Joe Tenerelli, July, 1994
 *
 *   Usage:   ANALconv_stra()
 *
 *****************************************************************************/
int Volume::ANALconv_straCH84orSH94(void)
{
    int      np,npoints;
    FrPoint *points;

    int i, j, ii, jj;                     /* loop indices                    */
    int iimax = 0, jjmax = 0;             /* indices for background area     */
    int bg_count;                         /* support value to derive bg_Zint */
    int type_trash[MAX_Y_DIM][MAX_X_DIM]; /* temporary precipitation type    */

    float bg_Z;                           /* support value to derive bg_Zint */
    float refl_diff = 0.0;                /* difference in reflectivity      */
    float conv_radius;                    /* defines conv area around core   */
 
/*  Check on the convective/stratiform separation algorithm:
 *  --------------------------------------------------------                 */
    if ((params.xyres != 2.0) && (params.xyres != 4.0)) {
	fprintf(stderr,
		"No convective/stratiform algorithm for grid resolution %f\n",
		params.xyres);
	TKreportWarning(W_2A54_NO_CONV_STRAT_GRID_ALG);
	exit(FAILED_CODE3);
    }

    fprintf(params.fpout1, "Convective/stratiform separation:\n");
    fprintf(params.fpout1, "---------------------------------\n");
    if (params.xyres == 2.0) {
	fprintf(params.fpout1,
		"   Steiner and Houze (1994) scheme is used\n\n");
    }
    else if (params.xyres == 4.0) {
	fprintf(params.fpout1,
		"   Churchill and Houze (1944) scheme is used\n\n");
    }
    else {
    }

/*  Determine the average intensity within the background area for each
 *  grid point on the working map:
 *  (linear average of all the non-zero pixels)
 *  -------------------------------------------------------------------      */
    if (params.xyres == 2.0) {            /* SH94 background area = 388 km^2 */
	// Set up the radius for the point search.
	if (!frInit(params.xydim, params.xydim,
		    params.xyres, params.xyres, BG_RADIUS))
	  {
	      fprintf(stderr, "Internal algorithm failure.\n");
		  TKreportWarning(W_2A54_INTERNAL_ALG_FAILED);
	      exit(FAILED_CODE1);
	  }

	
	// Just use the mask.
	if (!frGetMask(&points, &npoints))
	  {
	      fprintf(stderr, "Internal algorithm failure.\n");
		  TKreportWarning(W_2A54_INTERNAL_ALG_FAILED);
	      exit(FAILED_CODE1);
	  }
    }
    else if (params.xyres == 4.0) {       /* CH84 background area = 400 km^2 */
	iimax = 2;
	jjmax = 2;
    }
    else {
/*  ... room for including other separation schemes (for different grid)     */
    }
    for (j = 0; j < params.xydim; ++j) {
        for (i = 0; i < params.xydim; ++i) {
	    if (grids.reflmap[j][i] <= MISSINGV) {
		grids.bg_Zint[j][i] = MISSINGV;
	    }
            else {
                bg_Z = 0.;
                bg_count = 0;
		if (params.xyres == 2.0) {                  /* SH94 scheme   */
		    for (np = 0; np < npoints; np++) {
			ii = points[np].x + i;
			jj = points[np].y + j;
			if ((ii < 0) || (ii >= params.xydim) ||
			    (jj < 0) || (jj >= params.xydim)) continue;
			if (grids.reflmap[jj][ii] > MISSINGV) {
			    bg_Z = bg_Z + grids.reflinmap[jj][ii];
			    ++bg_count;
			}
		    }
		}
		else if (params.xyres == 4.0) {             /* CH84 scheme   */
		    for (jj = (j - jjmax); jj <= (j + jjmax); ++jj) {
			if ((jj >= 0) && (jj < params.xydim)) {
			    for (ii = (i - iimax); ii <= (i + iimax); ++ii) {
				if ((ii >= 0) && (ii < params.xydim)) {
				    if (grids.reflmap[jj][ii] > MISSINGV) {
					bg_Z = bg_Z + grids.reflinmap[jj][ii];
					++bg_count;
				    }
				}
			    }
			}
		    }
		}
		else {                                      /* other scheme  */
		}

		if (bg_count <= 0) {
		  TKreportWarning(W_2A54_NEGATIVE_BG_COUNT);
		  exit(FAILED_CODE1);
		}
		else {
		    bg_Z = bg_Z / (float)bg_count;
		    grids.bg_Zint[j][i] = 10. * log10(bg_Z);     /* in [dBZ] */
		}
	    }
	}
    }

/*  Determine the type of echo (grids.rain_type) at each pixel of the gridded
 *  radar data pattern:
 *           - no surface rainfall  -> no-rain    = 0
 *           - surface rainfall     -> stratiform = 1
 *                                  -> convective = 2
 *
 *  Method:  - search for relative reflectivity maxima that exhibit an
 *             echo intensity which is a certain amount higher than the
 *             background intensity (the minimum reflectivity difference
 *             to overcome in order to be identified as convective pixel
 *             is dependent on the intensity of the background)
 *           - every radar echo with intensity higher than a given threshold
 *             is automatically identified as convective
 *           - for each "convective core" a background intensity dependent
 *             area around that core is also set to be convective
 *  ------------------------------------------------------------------------ */

/*  1. Step: Identification of "convective cores"                            */
    for (j = 0; j < params.xydim; ++j) {
	for (i = 0; i < params.xydim; ++i) {
	    if (grids.reflmap[j][i] <= MISSINGV) {
		grids.rain_type[j][i] = 0;
	    }
	    else {
		if (params.xyres == 2.0) {                  /* SH94 scheme   */
		    if (grids.bg_Zint[j][i] < 0.) {
			refl_diff = 10.;
		    }
		    else {
			refl_diff = 10. - grids.bg_Zint[j][i] *
			  grids.bg_Zint[j][i] / SH94_PAR;
			if (refl_diff < 0.) refl_diff = 0.;
		    }
		}
		else if (params.xyres == 4.0) {             /* CH84 scheme   */
		    refl_diff = 10. * log10(2.) * ZR_B_FACTOR;
		}
		else {                                      /* other scheme  */
		}
		if ((grids.reflmap[j][i] - grids.bg_Zint[j][i]) >= refl_diff) {
		    grids.rain_type[j][i] = 2;
		}
		else if (grids.reflmap[j][i] >= THRESHOLD) {
		    grids.rain_type[j][i] = 2;
		}
		else {
		    grids.rain_type[j][i] = 1;
		}
	    }
	}
    }

/*  2. Step: Incorporate "convective area" around identified cores           */
    for (j = 0; j < params.xydim; ++j) {
	for (i = 0; i < params.xydim; ++i) {
	    type_trash[j][i] = grids.rain_type[j][i];
	}
    }
    for (j = 0; j < params.xydim; ++j) {
	for (i = 0; i < params.xydim; ++i) {
	    if (type_trash[j][i] == 2) {
		if (params.xyres == 2.0) {  /* Steiner and Houze scheme:
                                             * intensity dependent area size */
		    if (grids.bg_Zint[j][i] >= (40. + SH94_AREA_FAC)) {
			conv_radius = 5.0;
		    }
		    else if (grids.bg_Zint[j][i] >= (35. + SH94_AREA_FAC)) {
			conv_radius = 4.0;
		    }
		    else if (grids.bg_Zint[j][i] >= (30. + SH94_AREA_FAC)) {
			conv_radius = 3.0;
		    }
		    else if (grids.bg_Zint[j][i] >= (25. + SH94_AREA_FAC)) {
			conv_radius = 2.0;
		    }
		    else {
			conv_radius = 1.0;
		    }
		    iimax = 2 + (int)(conv_radius / params.xyres);
		    jjmax = 2 + (int)(conv_radius / params.xyres);


		    // Set up the radius for the point search.
		    if (!frInit(params.xydim, params.xydim,
				params.xyres, params.xyres, conv_radius))
		      {
			  fprintf(stderr, "Internal algorithm failure.\n");
			  TKreportWarning(W_2A54_INTERNAL_ALG_FAILED);
			  exit(FAILED_CODE1);
		      }
		    
		    if (!frGetPoints(i, j, &points, &npoints))
		      {
   			  TKreportWarning(W_2A54_INTERNAL_ALG_FAILED);
			  fprintf(stderr, "Internal algorithm failure.\n");
			  exit(FAILED_CODE1);
		      }
		    
		    // Go through the list of points and incorporate the
		    // points inside conv_radius into the computed precip.
		    // type domain.
		    for (np = 0; np < npoints; np++) {
			ii = points[np].x;
		        jj = points[np].y;
			if (grids.reflmap[jj][ii] > MISSINGV) {
			    grids.rain_type[jj][ii] = 2;
			}
		    }
		}
		else if (params.xyres == 4.0) {  /* Churchill and Houze scheme:
						  * conv. area = 144 km^2    */
		    iimax = 1;
		    jjmax = 1;

		    for (jj = (j - jjmax); jj <= (j + jjmax); ++jj) {
			if ((jj >= 0) && (jj < params.xydim)) {
			    for (ii = (i - iimax); ii <= (i + iimax); ++ii) {
				if ((ii >= 0) && (ii < params.xydim)) {
				    if (grids.reflmap[jj][ii] > MISSINGV) {
					grids.rain_type[jj][ii] = 2;
				    }
				}
				else {                      /* other scheme  */
				}
			    }
			}
		    }
		}
		else {                    /* other scheme                    */
		}
	    }
	}
    }

/*  Check data on "valid range":
 *  ----------------------------                                             */
    jj = 0;
    for (j = 0; j < params.xydim; ++j) {
	for (i = 0; i < params.xydim; ++i) {
	    if (grids.range_mask[j][i] == 0) {
		grids.rain_type[j][i] = MISSINGV;
/*
		if (grids.rain_type[j][i] != 0) {
		    grids.rain_type[j][i] = 0;
		    ++jj;
		}
*/
	    }
	}
    }
/*
    if (jj != 0) {
	fprintf(params.fpout1, "   ATTENTION: Index jj not 0!");
	fprintf(params.fpout1, " Please check on valid range.\n\n");
    }
*/

/*  OUTPUT: Convective/stratiform map (TRMM GT Product #2A-54):
 *  -----------------------------------------------------------              */
    fprintf(params.fpout1, "   Convective/stratiform map:  %s\n\n",
	    params.outputConvStraMapFn);

    frClose();

    return 0;
}
