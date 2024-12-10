/*****************************************************************************
 *   The function 'ANALhist_cfad' determines the frequency distribution of
 *   radar reflectivity as a function of altitude. The information prepared
 *   in this function is used to calculate the contoured frequency by altitude
 *   diagrams (CFAD; Yuter and Houze 1994).
 *
 *
 *   The following 12 categories are used:
 *    -> total     -> conv          -> stra          -> anvil
 *    -> land      -> conv_land     -> stra_land     -> anvil_land
 *    -> sea       -> conv_sea      -> stra_sea      -> anvil_sea
 *
 *   Version: Matthias Steiner, June 1994
 *   Version: Joe Tenerelli, July 1994
 *   Version: Sandra Yuter, August 1996
 *   Modified to correct bug, previously missing data and no echo were
 *   both lumped into "no echo" for classification of anvil. Now if
 *   data in low level scan is missing, echo above is not classified
 *   into any cfad category. Also used terser syntax. 
 *
 *   Usage:   ANALhist_cfad()
 *
 *****************************************************************************/

#include <stdio.h>                        /* standard input - output         */
#include <stdlib.h>                       /* standard library functions      */
#include <math.h>                         /* mathematical functions          */
#include <volume.h>                       /* general header for volume       */
extern "C" {
  #include "exitCodes.h"
}

#define CONV_MAPCODE 2
#define SF_MAPCODE 1
#define NOECHO_MAPCODE 0

int Volume::ANALhist_cfad(void)
{
    int i, j, k, l;                       /* loop indices                    */
    int Zindex;                           /* variable to reflectivity bin    */

/*  Check on histogram dimensions:
 *  ------------------------------                                           */
    cfad.max_Zindex = (int)(ZMAX_BIN - ZMIN_BIN);
    if (cfad.max_Zindex >= MAX_CLASS) {
	fprintf(stderr, "Please check the histogram dimensions!\n");
	TKreportWarning(W_2A55_INVALID_HISTOGRAM_DIM);
	exit(FAILED_CODE5);
    }

/*  Determine the reflectivity frequency distribution as a function of
 *  altitude. This information builds the basis for the CFAD calculation:
 *  ---------------------------------------------------------------------    */
    for (k = 0; k < params.zdim; ++k) {
	for (l = 0; l < MAX_CLASS; ++l) {
	    cfad.count_Zhist_tot[k][l] = 0;
	    cfad.count_Zhist_land[k][l] = 0;
	    cfad.count_Zhist_sea[k][l] = 0;
	    cfad.count_Zhist_conv[k][l] = 0;
	    cfad.count_Zhist_conv_land[k][l] = 0;
	    cfad.count_Zhist_conv_sea[k][l] = 0;
	    cfad.count_Zhist_stra[k][l] = 0;
	    cfad.count_Zhist_stra_land[k][l] = 0;
	    cfad.count_Zhist_stra_sea[k][l] = 0;
	    cfad.count_Zhist_anvil[k][l] = 0;
	    cfad.count_Zhist_anvil_land[k][l] = 0;
	    cfad.count_Zhist_anvil_sea[k][l] = 0;
	}
    }
    for (j = 0; j < params.xydim; ++j) {
	for (i = 0; i < params.xydim; ++i) {
	    if (grids.range_mask[j][i] == 1) {
		/* only compute cfad where lower level data is not
		       missing in cs map (not masked), note that
		       missing in Z map is set to no echo in cs map
		       */
		if ((grids.rain_type[j][i] == CONV_MAPCODE) ||
		    (grids.rain_type[j][i] == SF_MAPCODE) ||
		    (grids.rain_type[j][i] == NOECHO_MAPCODE))
		  {
		    for (k = 0; k < params.zdim; ++k) { /* for each level */
			/* printf("level is %d\n",k); */
			Zindex = (int)(grids.reflog[k][j][i] - ZMIN_BIN);
			/* handle Zindex out of bounds conditions */
			if (Zindex > cfad.max_Zindex) {
			    Zindex = cfad.max_Zindex; 
			}
			/* Zindex is <=0 for very weak and missing Z 
			   (i.e. no echo)
			 only do cfad where there is "echo"*/
			if (Zindex > 0) {
					
		    	++cfad.count_Zhist_tot[k][Zindex]; /* total cfad */
			
			if (grids.geomap[j][i] == 1) {       /* land  cfad  */
			    ++cfad.count_Zhist_land[k][Zindex];
			}
			else {                               /* ocean cfad  */
			    ++cfad.count_Zhist_sea[k][Zindex];
			}
			/* convective  */
			if (grids.rain_type[j][i] == CONV_MAPCODE) { 
			    ++cfad.count_Zhist_conv[k][Zindex];

			    if (grids.geomap[j][i] == 1) {  /* conv land */
				++cfad.count_Zhist_conv_land[k][Zindex];
			    }
			    else { /* conv ocean */
				++cfad.count_Zhist_conv_sea[k][Zindex];
			    }}
			/*stratiform*/
			else if (grids.rain_type[j][i] == SF_MAPCODE) {  
			    ++cfad.count_Zhist_stra[k][Zindex];

			    if (grids.geomap[j][i] == 1) {  /* sf land */
				++cfad.count_Zhist_stra_land[k][Zindex];
			    }
			    else { /* sf ocean */
				++cfad.count_Zhist_stra_sea[k][Zindex];
			    }}
			else /* only do anvil if no echo at bottom and 
				not on bottom level */
			  if ((grids.rain_type[j][i] == NOECHO_MAPCODE) 
			      && (k > 0)){
			      ++cfad.count_Zhist_anvil[k][Zindex];

			      if (grids.geomap[j][i] == 1) {  /* land     */
				  ++cfad.count_Zhist_anvil_land[k][Zindex];
			      }
			      else {                          /* ocean   */
				  ++cfad.count_Zhist_anvil_sea[k][Zindex];
			      }
			  }}}}

	    }}}

/*  OUTPUT: 3D reflectivity histograms -> CFADs (TRMM GT Product #2A-55):
 *  ---------------------------------------------------------------------    */
    fprintf(params.fpout1, "Reflectivity histograms (CFADs):\n");
    fprintf(params.fpout1, "--------------------------------\n");
    fprintf(params.fpout1, "   Reflectivity histograms:  %s\n\n",
	    params.outputCFADFn);

    return(0);
}
