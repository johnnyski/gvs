/*****************************************************************************
 *   The function 'ANALmean_prof' determines the mean vertical profile
 *   of radar reflectivity for the following 12 categories:
 *    -> total     -> conv          -> stra          -> anvil
 *    -> land      -> conv_land     -> stra_land     -> anvil_land
 *    -> sea       -> conv_sea      -> stra_sea      -> anvil_sea
 *
 *   Version: Matthias Steiner, June 1994
 *   Version: Joe Tenerelli, July, 1994
 *   Version: Sandra Yuter, August 1996
 *   terser syntax and only computes anvil when lowest level is noecho
 *
 *   Usage:   ANALmean_prof()
 *
 *****************************************************************************/

#include <stdio.h>                        /* standard input - output         */
#include <stdlib.h>                       /* standard library functions      */
#include <math.h>                         /* mathematical functions          */
#include <volume.h>                       /* general header for volume       */
#define CONV_MAPCODE 2
#define SF_MAPCODE 1
#define NOECHO_MAPCODE 0

int Volume::ANALmean_prof(void)
{
    float computeZmean (float runningreflin, int count);
    int i, j, k;                          /* loop indices                    */
    float reflin;                         /* reflectivity in mm6/m3          */

/*  Determine the mean vertical radar reflectivity profile(s):
 *  ----------------------------------------------------------               */
    for (k = 0; k < params.zdim; ++k) {
	prof.mean_Zprof_tot[k] = 0.;
	prof.count_Zprof_tot[k] = 0;
	prof.mean_Zprof_land[k] = 0.;
	prof.count_Zprof_land[k] = 0;
	prof.mean_Zprof_sea[k] = 0.;
	prof.count_Zprof_sea[k] = 0;
	prof.mean_Zprof_conv[k] = 0.;
	prof.count_Zprof_conv[k] = 0;
	prof.mean_Zprof_conv_land[k] = 0.;
	prof.count_Zprof_conv_land[k] = 0;
	prof.mean_Zprof_conv_sea[k] = 0.;
	prof.count_Zprof_conv_sea[k] = 0;
	prof.mean_Zprof_stra[k] = 0.;
	prof.count_Zprof_stra[k] = 0;
	prof.mean_Zprof_stra_land[k] = 0.;
	prof.count_Zprof_stra_land[k] = 0;
	prof.mean_Zprof_stra_sea[k] = 0.;
	prof.count_Zprof_stra_sea[k] = 0;
 	prof.mean_Zprof_anvil[k] = 0.;
	prof.count_Zprof_anvil[k] = 0;
 	prof.mean_Zprof_anvil_land[k] = 0.;
	prof.count_Zprof_anvil_land[k] = 0;
  	prof.mean_Zprof_anvil_sea[k] = 0.;
	prof.count_Zprof_anvil_sea[k] = 0;
  }

    /* first compute running sums in linear Z units */
    for (j = 0; j < params.xydim; ++j) {
	for (i = 0; i < params.xydim; ++i) {
	    if (grids.range_mask[j][i] == 1) {
		/* only compute vertical profile where lower level data is not
		       missing in cs map*/
		if ((grids.rain_type[j][i] == CONV_MAPCODE) ||
		    (grids.rain_type[j][i] == SF_MAPCODE) ||
		    (grids.rain_type[j][i] == NOECHO_MAPCODE))
		  {
		for (k = 0; k < params.zdim; ++k) {
		    /* only use "echo" in mean profile */
		    if (grids.reflog[k][j][i] > MISSINGV) {

			/* convert dBZ to linear Z value */
			reflin = pow(10., (grids.reflog[k][j][i] / 10.));

			prof.mean_Zprof_tot[k] += reflin;
			++prof.count_Zprof_tot[k];
			if (grids.geomap[j][i] == 1) {      /* land          */
			    prof.mean_Zprof_land[k] += reflin;
			    ++prof.count_Zprof_land[k];
			}
			else {                        /* ocean               */
			    prof.mean_Zprof_sea[k] += reflin;
			    ++prof.count_Zprof_sea[k];
			}
			/* convective  */
			if (grids.rain_type[j][i] == CONV_MAPCODE) {     
			    prof.mean_Zprof_conv[k] += reflin;
			    ++prof.count_Zprof_conv[k];
			    if (grids.geomap[j][i] == 1) {        /* land    */
				prof.mean_Zprof_conv_land[k] += reflin;
				++prof.count_Zprof_conv_land[k];
			    }
			    else {                          /* ocean         */
				prof.mean_Zprof_conv_sea[k] += reflin;
				++prof.count_Zprof_conv_sea[k];
			    }
			}
			/* stratiform */
			else if (grids.rain_type[j][i] == SF_MAPCODE) { 
			    prof.mean_Zprof_stra[k] += reflin;
			    ++prof.count_Zprof_stra[k];
			    if (grids.geomap[j][i] == 1) {      /* land      */
				prof.mean_Zprof_stra_land[k] += reflin;
				++prof.count_Zprof_stra_land[k];
			    }
			    else {                          /* ocean         */
				prof.mean_Zprof_stra_sea[k] += reflin;
				++prof.count_Zprof_stra_sea[k];
			    }
			}
			/* anvil only do above lowest level */
			else {  
			    if ((grids.rain_type[j][i] == NOECHO_MAPCODE) 
			      && (k > 0)) {                            
			    prof.mean_Zprof_anvil[k] += reflin;
			    ++prof.count_Zprof_anvil[k];
			    if (grids.geomap[j][i] == 1) {      /* land      */
				prof.mean_Zprof_anvil_land[k] += reflin;
				++prof.count_Zprof_anvil_land[k];
			    }
			    else {                          /* ocean         */
				prof.mean_Zprof_anvil_sea[k] += reflin;
				++prof.count_Zprof_anvil_sea[k];
			    }}
			}
		    }
		}}
	    }
	}
    }
    /* now compute the means from the running totals */
    for (k = 0; k < params.zdim; ++k) {
	
	prof.mean_Zprof_tot[k]  = computeZmean(prof.mean_Zprof_tot[k],
					      prof.count_Zprof_tot[k]);
	prof.mean_Zprof_land[k] = computeZmean(prof.mean_Zprof_land[k],
					       prof.count_Zprof_land[k]);
	prof.mean_Zprof_sea[k]  = computeZmean(prof.mean_Zprof_sea[k],
					       prof.count_Zprof_sea[k]);
	prof.mean_Zprof_conv[k] = computeZmean(prof.mean_Zprof_conv[k],
					       prof.count_Zprof_conv[k]);
	prof.mean_Zprof_conv_land[k] = 
	  computeZmean(prof.mean_Zprof_conv_land[k],
		       prof.count_Zprof_conv_land[k]);
	prof.mean_Zprof_conv_sea[k]  =
	  computeZmean(prof.mean_Zprof_conv_sea[k],
		       prof.count_Zprof_conv_sea[k]);
	prof.mean_Zprof_stra[k] = computeZmean(prof.mean_Zprof_stra[k],
						prof.count_Zprof_stra[k]);
	prof.mean_Zprof_stra_land[k] =
	  computeZmean(prof.mean_Zprof_stra_land[k],
		       prof.count_Zprof_stra_land[k]);
	prof.mean_Zprof_stra_sea[k] =
	  computeZmean(prof.mean_Zprof_stra_sea[k],
		       prof.count_Zprof_stra_sea[k]);
	prof.mean_Zprof_anvil[k] = computeZmean(prof.mean_Zprof_anvil[k],
						prof.count_Zprof_anvil[k]);
	prof.mean_Zprof_anvil_land[k] = 
	  computeZmean(prof.mean_Zprof_anvil_land[k],
		       prof.count_Zprof_anvil_land[k]);
	prof.mean_Zprof_anvil_sea[k]  =
	  computeZmean(prof.mean_Zprof_anvil_sea[k],
		       prof.count_Zprof_anvil_sea[k]);

    }

/*  OUTPUT: Mean vertical reflectivity profile (TRMM GT Product #2A-55):
 *  --------------------------------------------------------------------     */
    fprintf(params.fpout1, "Mean vertical structure:\n");
    fprintf(params.fpout1, "------------------------\n");
    fprintf(params.fpout1, "   Mean reflectivity profiles:  %s\n\n",
	    params.outputMeanVertProfFn);

    return(0);
}

/* SEY 
A subroutine to make life simpler, this handles the
checking that the count is > 0 and converts to dBZ units 
input is runningsum of reflectivity in linear Z units and count
*/ 
float computeZmean(float runningreflin, int count) {

float meanval;

if (count<=0) {
    meanval= MISSINGV;
}
else
  {
      meanval = runningreflin / (float) count;
      /* convert to dBZ units */
      meanval = 10 * log10(meanval);
  }

return (meanval);
} /* end computeZmean */
