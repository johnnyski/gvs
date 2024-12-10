/*
 * Quality Control of radar reflectivity values 
 * (and differential reflectivity values?)
 * 
 * Uses Danny Rosenfeld's QC algorithm, implemented in Fortran.
 *
 * Adapted for 1C-51 HDF file generation: 23 Aug 96
 * mike.kolander@trmm.gsfc.nasa.gov
 *
 * Initiated: 3/11/96
 * By: John Merritt
 *
 * 1. Construct a 3D array, Z(nbins, nrays, nsweeps), for the appropriate
 *    volume: DZ or CZ.
 *
 * 2. Construct 2D array of azimuths, AZIM(nrays, nsweeps)
 *
 * 3. Construct 1D array of tilt angles, TILTS(nsweeps)
 *
 * 4. Call QCVOSWIN.  This is Danny's QC algorithm.
 *
 * 5. Output the resultant rainmap.
 *
 */

#include <stdio.h>
#include <unistd.h>
#ifdef __linux
#include <getopt.h>
#endif
#include <stdlib.h>
#include <string.h>

#include "rsl.h"
/* Parameter definitions for 1B-51 and 1C-51 HDF
	 file handling applications using the TSDIS toolkit. */
#include "toolkit_1BC-51_appl.h"

/*char *strdup(const char *s);*/
short     badValue;  /* passed to voswin. */

/* QC verbosity. Set to 1 when verbosity required during testing. */
static int verbose=0;
extern int radar_verbose_flag;

void
#if defined(__linux) || defined(__sgi)
  voswin_
#else
  voswin
#endif
(void *verbose, void *hThresh1, void *hThresh2, void *hThresh3,
void *zThresh0, void *zThresh1, void *zThresh2, void *zThresh3,
void *rayWidth, void *range1, void *binLength,
void *dbzNoise, void *radElev, void *nbins, void *nrays, void *nsteps,
void *Z, void *azim, void *tilts, void *badValue, void *status);

#define MAX_CLUTTER_ELEV 2.0  /* Clutter map to be applied to all sweeps below this */

typedef struct{
  int azimuth;
  float ground_range;
} Clutter_point;


/*************************************************************/
/*                                                           */
/*                     nonNULLobjects                        */
/*                                                           */
/*************************************************************/
static int nonNULLobjects(void **object, int nobjects)
{
	/* Count and return the no. of non-null objects in a linear
		 array of length 'nobjects'.
  */
	int dim, j;

	dim = 0;
	for (j=0; j<nobjects; j++)
	  if (object[j] != NULL) dim++;
	return(dim);
}

/*************************************************************/
/*                                                           */
/*                       max_nrays                           */
/*                                                           */
/*************************************************************/
int max_nrays(Volume *v)
{
  int max = 0;
  Sweep *sweep;
  int i;

  if (v == NULL) return 0;

  for (i=0; i<v->h.nsweeps; i++) {
        if (v->sweep == NULL) continue;
        sweep = v->sweep[i];
        if (sweep)
          if (sweep->h.nrays > max) max = sweep->h.nrays;
  }
  return max;
}

/*************************************************************/
/*                                                           */
/*                      max_nbins                            */
/*                                                           */
/*************************************************************/
int max_nbins(Volume *v)
{
  int max = 0;
  Ray *ray;
  Sweep *sweep;
  int i, j;

  if (v == NULL) return 0;

  for (i=0; i<v->h.nsweeps; i++) {
	if (v->sweep == NULL) continue;
	sweep = v->sweep[i];
	if (sweep) {
	  for (j=0; j<sweep->h.nrays; j++) {
		ray = sweep->ray[i];
		if (ray)
		  if (ray->h.nbins > max) max = ray->h.nbins;
	  }
	}
  }
  return max;
}

/*************************************************************/
/*                                                           */
/*                    construct_Z                            */
/*                                                           */
/*************************************************************/
short *construct_Z(Volume *v)
{
  short *Z;
  Sweep *sweep;
  Ray *ray;
  int nbins, nrays, nsteps;
  int index;
  int i, j, k;
  float x;

  if (v == NULL) return NULL;
  /* Z's dimensions are nbins X nrays X nsweeps. */

  nbins  = max_nbins(v);
  nrays  = max_nrays(v);
  nsteps = v->h.nsweeps;

  Z = (short *)calloc(nbins*nrays*nsteps, sizeof(short));
  if (Z == NULL) perror("construct_Z");

/* Treat the indexing as if we have a 3D array: Z(nsteps, nrays, nbins).
 * Note, that notation is for C.  When passed to Fortran, nbins will be
 * incrimented the fastest; to avoid paging.
 */
  for (i=0; i<v->h.nsweeps; i++) {
	if (v->sweep == NULL) continue;
	sweep = v->sweep[i];
	if (sweep) {
	  for (j=0; j<sweep->h.nrays; j++) {
		if (sweep->ray == NULL) continue;
		ray = sweep->ray[j];
		if (ray) {
		  for (k=0; k<ray->h.nbins; k++) {
			/* Necessary math because we could have skip structures and
			 * we want them to be 0's in Z.
			 */
			index = i*(nbins*nrays) + j*(nbins) + k;
			x = ray->h.f(ray->range[k]);
			if (x == BADVAL || x == NOECHO) Z[index] = badValue; /* Global. */
			else Z[index] = x * 10; /* Scaled by 10. */
		  }
		}
	  }
	}
  }
  return Z;
}
/*************************************************************/
/*                                                           */
/*                    construct_AZIM                         */
/*                                                           */
/*************************************************************/
float *construct_AZIM(Volume *v)
{
  float *AZIM;
  Sweep *sweep;
  Ray *ray;
  int nrays, nsteps;
  int index;
  int i, j;

  if (v == NULL) return NULL;
  nrays  = max_nrays(v);
  nsteps = v->h.nsweeps;

  AZIM = (float *)calloc(nrays*nsteps, sizeof(float));
  if (AZIM == NULL) perror("construct_AZIM");

/* Initialize AZIM to -999.  This means NO RAY present. */
  for (i=0, index=0; i<nsteps; i++)
	for (j=0; j<nrays; j++)
	  AZIM[index++] = -999;

/* Treat the indexing as if we have a 3D array: AZIM(nsteps, nrays).
 * Note, that notation is for C.  When passed to Fortran, nrays will be
 * incrimented the fastest; to avoid paging.
 */
  for (i=0; i<v->h.nsweeps; i++) {
	if (v->sweep == NULL) continue;
	sweep = v->sweep[i];
	if (sweep) {
	  for (j=0; j<sweep->h.nrays; j++) {
		if (sweep->ray == NULL) continue;
		ray = sweep->ray[j];
		if (ray) {
		  index = i*(nrays) + j;
		  AZIM[index] = ray->h.azimuth;
		}
	  }
	}
  }

  return AZIM;
}
/*************************************************************/
/*                                                           */
/*                    construct_TILTS                        */
/*                                                           */
/*************************************************************/
float *construct_TILTS(Volume *v)
{
  float *TILTS;
  Sweep *sweep;
  int nsteps;
  int index;
  int i;

  if (v == NULL) return NULL;
  nsteps = v->h.nsweeps;

  TILTS = (float *)calloc(nsteps, sizeof(float));
  if (TILTS == NULL) perror("construct_TILTS");

/* Treat the indexing as if we have a 1D array: TILTS(nsteps).
 * Note, that notation is for C.  When passed to Fortran, nsteps
 * incrimented the fastest; to avoid paging.
 */
  for (i=0; i<v->h.nsweeps; i++) {
	if (v->sweep == NULL) continue;
	sweep = v->sweep[i];
	if (sweep) {
	  index = i;
	  TILTS[index] = sweep->h.elev;
	}
  }

  return TILTS;
}

/*************************************************************/
/*                                                           */
/*                          clean_CD                         */
/*                                                           */
/*************************************************************/
Volume *clean_CD(Volume *v)
{
	/* Clean the random "popcorn" noise from the QC'd ZDR (CD) volume. 
	   Lassen ZDR data contains isolated groups of 1 or 2 adjacent bins
		 containing valid data values, while surrounding bins contain
		 BADVAL flags. This function locates such isolated groups of bins,
		 and writes BADVAL flags into them.
	*/
	int sindex, rindex, bindex;
	Sweep *sweep;
	Range badval;
	
	badval = v->h.invf(BADVAL);
	for (sindex=0; sindex<v->h.nsweeps; sindex++)
	{
		sweep = v->sweep[sindex];
		for (rindex=1; rindex<sweep->h.nrays-1; rindex++)
		{
			if (sweep->ray[rindex+1] == NULL) continue;
		  for (bindex=1; bindex<sweep->ray[rindex]->h.nbins-1; bindex++)
			{
				/* Check the four bins in the sweep directly adjacent to this bin.
					 If they all contain BADVAL flags, then write BADVAL in this bin
					 too. */
				if (sweep->ray[rindex]->range[bindex] != badval)
					if (sweep->ray[rindex]->range[bindex-1] == badval)
					  if (sweep->ray[rindex]->range[bindex+1] == badval)
						  if (sweep->ray[rindex-1]->range[bindex] == badval)
							  if (sweep->ray[rindex+1]->range[bindex] == badval)
								{
								  sweep->ray[rindex]->range[bindex] = badval;
									continue;
								}
				/* Check the four bins diagonally adjacent to this bin. If they
				   all contain BADVAL flags, then write BADVAL in this bin too.*/
				if (sweep->ray[rindex]->range[bindex] != badval)
					if (sweep->ray[rindex-1]->range[bindex-1] == badval)
					  if (sweep->ray[rindex-1]->range[bindex+1] == badval)
						  if (sweep->ray[rindex+1]->range[bindex-1] == badval)
						    if (sweep->ray[rindex+1]->range[bindex+1] == badval)
								  sweep->ray[rindex]->range[bindex] = badval;
			}  /* end for (bindex=1... */
			/* Handle last bin in the ray. */
			bindex = sweep->ray[rindex]->h.nbins - 1;
			if (sweep->ray[rindex]->range[bindex] != badval)
			  if (sweep->ray[rindex]->range[bindex-1] == badval)
				  if (sweep->ray[rindex-1]->range[bindex] == badval)
				    if (sweep->ray[rindex+1]->range[bindex] == badval)
					    sweep->ray[rindex]->range[bindex] = badval;
		}  /* end for (rindex=1... */
	} /* end for (sindex=0... */

	return(v);
}

/*************************************************************/
/*                                                           */
/*                        load_Z_and_CD                      */
/*                                                           */
/*************************************************************/
Radar *load_Z_and_CD(Radar *radar, short *Z)
{
/* This function is virtually a copy of the function 'load_Z' below.
	 Difference:
	   Function 'load_Z' creates only a 'CZ' volume.
		 Function 'load_Z_and_CD' creates concurrently both a corrected 
		 differential reflectivity volume 'CD' and a 'CZ' volume.
*/

  Volume *cz, *cd, *dz;

  Sweep *sweep;
  Ray *ray;
  int nbins, nrays;
  int index;
  int i, j, k;
  float x;

	dz = radar->v[DZ_INDEX];
  /* Z's dimensions are nbins X nrays X nsweeps. */

  nbins  = max_nbins(dz);
  nrays  = max_nrays(dz);

  cz = RSL_copy_volume(dz);
  cd = RSL_copy_volume(radar->v[ZD_INDEX]);
/* Treat the indexing as if we have a 3D array: Z(nsteps, nrays, nbins).
 * Note, that notation is for C.  When passed to Fortran, nbins will be
 * incremented the fastest; to avoid paging.
 */

  for (i=0; i<cz->h.nsweeps; i++)
	{
		if (cz->sweep == NULL) continue;
		sweep = cz->sweep[i];
		if (sweep)
		{
			for (j=0; j<sweep->h.nrays; j++)
			{
				if (sweep->ray == NULL) continue;
				ray = sweep->ray[j];
				if (ray)
				{
					for (k=0; k<ray->h.nbins; k++)
					{
						/* Necessary math because we could have skip structures and
						 * we want them to be 0's in Z.
						 */
						index = i*(nbins*nrays) + j*(nbins) + k;
						x = Z[index];
						if (x == -999)
						{
							ray->range[k] = ray->h.invf(BADVAL);
							cd->sweep[i]->ray[j]->range[k] = cd->h.invf(BADVAL);
						}
						else
						{
							/* ray->range[k] = ray->h.invf(x / 10.0); */
						}
					}
				}
			}
		}
  }
	radar->v[CZ_INDEX] = cz;
	/* Clean the random "popcorn" noise from the QC'd ZDR (CD) volume. */
	radar->v[CD_INDEX] = clean_CD(cd);
  return radar;
}

/*************************************************************/
/*                                                           */
/*                        load_Z                             */
/*                                                           */
/*************************************************************/
Volume *load_Z(Volume *dz, short *Z)
{
  Volume *cz;

  Sweep *sweep;
  Ray *ray;
  int nbins, nrays;
  int index;
  int i, j, k;
  float x;

  if (dz == NULL) return NULL;
  /* Z's dimensions are nbins X nrays X nsweeps. */

  nbins  = max_nbins(dz);
  nrays  = max_nrays(dz);

  cz = RSL_copy_volume(dz);
  
/* Treat the indexing as if we have a 3D array: Z(nsteps, nrays, nbins).
 * Note, that notation is for C.  When passed to Fortran, nbins will be
 * incremented the fastest; to avoid paging.
 */

  for (i=0; i<cz->h.nsweeps; i++)
	{
		if (cz->sweep == NULL) continue;
		sweep = cz->sweep[i];
		if (sweep)
		{
			for (j=0; j<sweep->h.nrays; j++)
			{
				if (sweep->ray == NULL) continue;
				ray = sweep->ray[j];
				if (ray)
				{
					for (k=0; k<ray->h.nbins; k++)
					{
						/* Necessary math because we could have skip structures and
						 * we want them to be 0's in Z.
						 */
						index = i*(nbins*nrays) + j*(nbins) + k;
						x = Z[index];
						if (x == -999) 
							ray->range[k] = ray->h.invf(BADVAL);
						else
						{
							/* x /= 10;
								 ray->range[k] = ray->h.invf(x); */
						}
					}
				}
			}
		}
	}
	return cz;
}

/*************************************************************/
/*                                                           */
/*                      add_calibr_factor                    */
/*                                                           */
/*************************************************************/
void add_calibr_factor(Volume *v, float zcal)
{
/* Add the calibration factor 'zcal' to all reflectivity
 * values in volume 'v'.
*/
  int isweep, iray, ibin;
  Ray *r;
  float val; 

  for (isweep=0; isweep<v->h.nsweeps; isweep++)
  {
	if (v->sweep[isweep] == NULL) continue;
	for (iray=0; iray<v->sweep[isweep]->h.nrays; iray++)
	{
	  if (v->sweep[isweep]->ray[iray] == NULL) continue;
	  r = v->sweep[isweep]->ray[iray];
	  for (ibin=0; ibin<r->h.nbins; ibin++)
	  {
		val = r->h.f(r->range[ibin]);
		if (val != (float)BADVAL)
		  r->range[ibin] = r->h.invf(val + zcal);
	  } /* end for (ibin=0...*/
	} /* end for (iray=0...*/
  } /* end for (isweep=0... */
}


/*************************************************************/
/*                                                           */
/*                      qc_using_vr_mask                     */
/*                                                           */
/*************************************************************/
Volume *qc_using_vr_mask(Volume *dz_volume, Volume *vr_volume,
						 int vrMask)
{
  /*
	Use velocity (VR) data as a mask to QC the reflectivity (Z) data.
	Set to BADVAL any Z value that is associated with an invalid
	or zero VR value.
	  vrMask=0: Do not QC using velocity mask.
	         1: Mask bins where velocity is invalid
             2: Mask bins where velocity is invalid or zero
	Requires that the VR bins overlay the corresponding Z bins
	EXACTLY, ie, each VR bin must pair with a Z bin with respect 
	to the following: (ray_no_in_sweep, elev, azim, gate_size)

	Returns: QC'd Z volume.
  */

  int isweep, iray, ibin;
  unsigned int invalid_velocity_count=0;
  unsigned int zero_velocity_count=0;
  float vel;
  Ray *qc_ray, *vr_ray;
  Volume *qc_volume;      /* The QC'd Z volume */

  qc_volume = RSL_copy_volume(dz_volume);

  /* This function should only be called if vrMask non-zero. */
  if (vrMask == 0)
  {
	fprintf(stderr, "\nqc_using_vr_mask(): FATAL SOFTWARE ERROR\n\n");
	goto abort;
  }

  /* Work thru the entire qc volume, replacing each Z value with
	 BADVAL when the corresponding velocity value warrants it.
	 Each velocity bin must pair EXACTLY with a Z bin (ray_no_in_sweep,
	 elev, azim, gate_size).  Exit if non-matching bins found.
  */
  for (isweep=0; isweep<qc_volume->h.nsweeps; isweep++)
  {
	if (qc_volume->sweep[isweep] == NULL || vr_volume->sweep[isweep] == NULL)
	  goto abort;
	if (qc_volume->sweep[isweep]->h.elev != vr_volume->sweep[isweep]->h.elev)
	  goto abort;
	for (iray=0; iray<qc_volume->sweep[isweep]->h.nrays; iray++)
	{
	  qc_ray = qc_volume->sweep[isweep]->ray[iray];
	  vr_ray = vr_volume->sweep[isweep]->ray[iray];
	  if (qc_ray == NULL || vr_ray == NULL)
		goto abort;
	  if (qc_ray->h.nbins != vr_ray->h.nbins)
		goto abort;
	  if (qc_ray->h.gate_size != vr_ray->h.gate_size)
		goto abort;
	  if (qc_ray->h.elev != vr_ray->h.elev)
		goto abort;
	  if (qc_ray->h.azimuth != vr_ray->h.azimuth)
		goto abort;
	  /* OK, the VR bins seem to pair up with the Z bins, for this ray.
		 So, now go down the ray looking for invalid VR values. */
	  for (ibin=0; ibin<qc_ray->h.nbins; ibin++)
	  {
		vel = vr_ray->h.f(vr_ray->range[ibin]);
	   	if (vel<=BADVAL && vel>=NOECHO)
		{
		  invalid_velocity_count++;
		  qc_ray->range[ibin] = qc_ray->h.invf(BADVAL);
		}
		else if (vrMask==2 && vel==0.0)
		{
		  zero_velocity_count++;
		  qc_ray->range[ibin] = qc_ray->h.invf(BADVAL);
		}
	  } /* end for (ibin=0...*/
	} /* end for (iray=0... */
  } /* end for (isweep=0...*/

  /* Completed entire volume. */
  if (radar_verbose_flag)
  {
	fprintf(stderr, "Succeeded.\n");
	fprintf(stderr, "  Num invalid_velocity_bins = %d\n", invalid_velocity_count);
	if (vrMask==2)
	  fprintf(stderr, "  Num zero_velocity_bins = %d\n", zero_velocity_count);
  }
  return(qc_volume);

 abort:
  if (radar_verbose_flag)
	fprintf(stderr, "Failed.\n");
  return(qc_volume);
}

/*************************************************************/
/*                                                           */
/*                 applyCluttermapToSweep                    */
/*                                                           */
/*************************************************************/

void applyCluttermapToSweep(Sweep *sweep, Clutter_point* clutter_map, int clutter_map_length) 
{
  
  int i,j,k, min_range_bin_num, azim,azimlast,last_azim_start_index;
  float min_slant_range, elev, height, gate_size, range_bin1;
  Ray *ray;
  Range (*invf)(float x);

  invf = sweep->h.invf;
  azimlast=0;
  last_azim_start_index = 0;
  elev = sweep->h.elev;

  for (i = 0; i < sweep->h.nrays; i++) {
    ray = sweep->ray[i];
    if (ray != NULL)
      {
	/* round azimuth to nearest int */
	azim = (int) (ray->h.azimuth + 0.5);
	if (azim >= 360) azim = azim - 360;

        /* the following line handles the case of an azimuth crossing from
           360 to 0; if this occurs, reset the cluttermap table index to
           the start of the table (azimlast is the azimuth of the prior ray) */

        if((azim-azimlast) < 0) last_azim_start_index=0;

	/* 
	   Find the azimuth in the cluttermap - 
	   Save time by not searching the whole array.
	   last_azim_start_index keeps track of the last index accessed in the array.

	   Note that some ray azimuth float values skip 
	   integer azim values when they are rounded to nearest int.
	*/

	k = last_azim_start_index;
	while(k < clutter_map_length && clutter_map[k].azimuth < azim) {
	  k++;
	}
	last_azim_start_index = k;

	/* convert this ray's gate_size and range_bin1 to km */
	gate_size = ray->h.gate_size/1000.0;
	range_bin1 = ray->h.range_bin1/1000.0;

	/* foreach entry under this azimuth in cluttermap */
	j = 0;
	while((k + j < clutter_map_length) && clutter_map[k + j].azimuth == azim) {

	  /*********************************************************************************
	   * Using the clutter map, assuming clutter points have 1km resolution,           *
	   * mask out values centered at the ground ranges in the clutter map.             *
	   *                                                                               *
	   * This code assumes the cluttermap ranges are strictly increasing.              *
	   * It allows for skipped azimuth values.                                         *
	   *                                                                               *
	   * get the ground range from clutter_map                                         *
	   * subtract  0.5 km to center the mask on this range                             *
	   *   ground range is very close to slant range at this elevation                 *
	   * first range bin = (min_slant_range - range to first bin)/(gate size)          *
	   * gate size maps to 4 range bins, apply mask to 4 starting at min_range_bin_num *
	   *********************************************************************************/

	  /* SPEED: Since the azimuths are low, assume ground_range == slant_range          */
	  /* At 150 km, for 1.9 deg elevation, ground range, slant range differ by 0.186 km, 
             using RSL_get_slantr_and_h() */

	  min_slant_range = clutter_map[k + j].ground_range - 0.5;
	  min_range_bin_num = (int) ((min_slant_range -  range_bin1)/gate_size);

	  /* SPEED: Unroll this loop if bounds are ok.                    */
	  /* Can do since diff between min and max slant range are fixed. */
	    	  
	  ray->range[min_range_bin_num] = ray->h.invf(BADVAL);
	  ray->range[min_range_bin_num + 1] = ray->h.invf(BADVAL);
	  ray->range[min_range_bin_num + 2] = ray->h.invf(BADVAL);
	  ray->range[min_range_bin_num + 3] = ray->h.invf(BADVAL);
	  
	  j++;
	}
        /* store the azimuth of the just completed ray into azimlast */

        azimlast=azim;
      }
  }
}


/*************************************************************/
/*                                                           */
/*                    applyClutterMap                        */
/*                                                           */
/*************************************************************/


Volume *applyClutterMap(Volume *cz_volume, Clutter_point *clutter_map, int clutter_map_length)
{
  Sweep *sweep;
  int i = 0;

  if (cz_volume != NULL) {
    sweep = cz_volume->sweep[0];

    /* Apply clutter map to sweeps below MAX_CLUTTER_ELEV */
    while(sweep != NULL && i < cz_volume->h.nsweeps 
	  && sweep->h.elev < MAX_CLUTTER_ELEV) 
      {
	applyCluttermapToSweep(sweep, clutter_map, clutter_map_length);
	i++;
	sweep = cz_volume->sweep[i];
      }
  }

  return cz_volume;
}


/*************************************************************/
/*                                                           */
/*                    clutterMapInit                         */
/*                                                           */
/*************************************************************/

int clutterMapInit(Clutter_point **clutter_map, int *clutter_map_length, char *clutter_map_fname) {

  Clutter_point *clutter_list;
  int num_entries, i;
  FILE *fp;

  fp = fopen(clutter_map_fname,"r");

  if (fp == NULL)
	{
	  fprintf(stderr, "\nclutterMapInit(): Error opening clutter map: %s\n",
		  clutter_map_fname);
	  return(ABORT);
	}

  fscanf(fp,"%d",&num_entries);

  clutter_list = (Clutter_point *) calloc(num_entries, sizeof(Clutter_point));

  *clutter_map_length = 0;
  while(fscanf(fp,"%d %f",&clutter_list[*clutter_map_length].azimuth, &clutter_list[*clutter_map_length].ground_range)
	!= EOF)
      (*clutter_map_length)++;

  fclose(fp);

  *clutter_map = clutter_list;
  return(OK);
  
}


/*************************************************************/
/*                                                           */
/*                        qualityControl                     */
/*                                                           */
/*************************************************************/
Radar *qualityControl(Radar *radar, float *qcParm, int vrMask, char *clutter_map_fname)
{
/* 
   Perform QC processing for radar reflectivity volume 'DZ'.

   QC processing consists of two stages.
   1. If the user-selected flag 'vrMask' is set,
      then uses the VR volume as a mask to reject those Z values
	  associated with invalid velocity values.
	  vrMask=0: Do not QC using velocity mask.
	         1: Mask bins where velocity is invalid
             2: Mask bins where velocity is invalid or zero
   2. Applies Danny's algorithm to edit the reflectivity values
      for noise; ie, attempts to remove echoes associated with ground
	  clutter, etc, from the Z volume. The editted reflectivity values
	  are placed into volume 'CZ'.

   Returns a radar structure containing:
   1. the (unchanged) volumes passed to this function.
   2. QC'ed volume 'CZ' produced by this function.
*/
  Volume    *dz_volume;
  Sweep     *sweep;
  Ray       *ray;

  float     *azim;
  float     *tilts;
  float     range1,  binLength,  radElev, dbzNoise;
  float     hThresh1, hThresh2, hThresh3;
  float     zThresh0, zThresh1, zThresh2, zThresh3;
  float     rayWidth;
  int       nbins, nrays, nsteps;
  short     *Z, status;


  Clutter_point *clutter_map;
  int clutter_map_size;

	
  /* Must have DZ volume to do any QC processing. */
  if (radar->v[DZ_INDEX] == NULL)
  {
	if (radar_verbose_flag)
	  fprintf(stderr, "No reflectivity volume 'DZ' for QC processing...\n");
	return(radar);
  }

  /* Set up QC parameters. */
  dbzNoise = qcParm[DBZNOISE];
  hThresh1 = qcParm[HTHRESH1];
  hThresh2 = qcParm[HTHRESH2];
  hThresh3 = qcParm[HTHRESH3];
  zThresh0 = qcParm[ZTHRESH0];
  zThresh1 = qcParm[ZTHRESH1];
  zThresh2 = qcParm[ZTHRESH2];
  zThresh3 = qcParm[ZTHRESH3];

  dz_volume = radar->v[DZ_INDEX];

  /*
	If a velocity volume exists, and if the user wishes to use it to
	mask out DZ values, then QC the DZ volume using the VR volume as
	a mask.
  */
  if (vrMask)
	if (radar->v[VR_INDEX] != NULL)
	{
	  if (radar_verbose_flag)
		fprintf(stderr, "Applying VR mask -->>     ");
	  dz_volume = qc_using_vr_mask(radar->v[DZ_INDEX], radar->v[VR_INDEX],
								   vrMask);
	}

  if (radar_verbose_flag)
	fprintf(stderr, "Applying Danny Rosenfelds algorithm -->>  ");
  /* 
	 Now set things up for Danny's QC algorithm.
	 Construct Z array.  This is a basic repackaging of the Volume
	 structure suitable for passing to a Fortran subroutine.
	 Optimization note: combine the construction of Z, azim and tilts.
  */
  badValue = -999;  /* Define the global variable for Z construction. */
  Z = construct_Z(dz_volume);

  /* Z's dimensions are nbins X nrays X nsweeps. */

  nbins  = max_nbins(dz_volume);
  nrays  = max_nrays(dz_volume);
  nsteps = dz_volume->h.nsweeps;
  nsteps = nonNULLobjects((void **)dz_volume->sweep, dz_volume->h.nsweeps);
  if (verbose) printf("nbins %d, nrays %d, nsteps %d\n", nbins, nrays, nsteps);
  azim = construct_AZIM(dz_volume);

  tilts = construct_TILTS(dz_volume);

	/*
	 * DEFINE ADDITIONAL PARAMETERS.
	 */

  ray       = RSL_get_first_ray_of_volume(dz_volume);
  sweep     = RSL_get_first_sweep_of_volume(dz_volume);
  range1    = ray->h.range_bin1 / 1000.0;  /* KM */
  binLength = ray->h.gate_size  / 1000.0;  /* KM */
  radElev   = radar->h.height   / 1000.0;  /* KM */
  rayWidth  = sweep->h.beam_width;

	/*
	 * To access elements of Z such that it matches the fortran syntax,
	 *    Z(istep, iray, ibin), use the following index calculation:
	 *        index = i*(nbins*nrays) + j*(nbins) + k;
	 *    Where, i = istep, j = iray, and k = ibin.
	 *
	 * To access elements of azim such that it matches the fortran syntax,
	 *    azim(istep, iray), use the following index calculation:
	 * 		  index = i*(nrays) + j;
	 *    Where, i = istep, and j = iray.
	 */

#if defined(__linux) || defined(__sgi)
  voswin_
#else
  voswin
#endif
(&verbose, &hThresh1, &hThresh2, &hThresh3,
&zThresh0, &zThresh1, &zThresh2, &zThresh3,
&rayWidth, &range1, &binLength,
&dbzNoise, &radElev, &nbins, &nrays, &nsteps,
Z, azim, tilts, &badValue, &status);

  /* If the voswin routine failed, exit. */
  if (status < 0) 
  {
	if (radar_verbose_flag)
	  fprintf(stderr, "Failed.\n");
	radar->v[CZ_INDEX] = NULL;
	return(radar);
  }

  if (radar_verbose_flag)
	fprintf(stderr, "Succeeded.\n\n");

  if (radar->h.nvolumes < 15) { /* We need to allocate a new radar so that
                                 * we have the MZ & MD mask volume slots.
                                 */
	Radar *save_radar;
	int i;

	save_radar = radar;
	radar = RSL_new_radar(15);
	radar->h = save_radar->h;
	radar->h.nvolumes = 15; /* Oops, the last statement
				 * destroyed this value. 
				 */
	for (i=0; i<save_radar->h.nvolumes; i++)
	  radar->v[i] = save_radar->v[i];
  }  /* end if (radar->h.nvolumes < 15) */

  /* 
	 Now construct a CZ volume.  Use DZ as a model and 
	 the data comes from Z. No differential volume CD needed.
	 Just create CZ.
  */
  radar->v[CZ_INDEX] = load_Z(radar->v[DZ_INDEX], Z);
/****  Removed this code 17 Jun 1998 ****************
  if (radar->v[ZD_INDEX] == NULL)
		radar->v[CZ_INDEX] = load_Z(radar->v[DZ_INDEX], Z);
  else
		radar = load_Z_and_CD(radar, Z);
******************************************************/

  /* Free all dynamically allocated arrays. */
  if (azim != NULL) free(azim);
  if (tilts != NULL) free(tilts);
  if (Z != NULL) free(Z);
  /* 
	 If VR mask processing was done, free tha QC'd Z volume
     which was allocated in function qc_using_vr_mask()
  */
  if (vrMask)
	if (radar->v[VR_INDEX] != NULL)
	  RSL_free_volume(dz_volume);


  /* Apply clutter map to cz volume, to mask out known clutter locations. */
  if(clutter_map_fname != NULL) {
    status = clutterMapInit(&clutter_map, &clutter_map_size, clutter_map_fname);
    if (status < 0) {
      if(radar_verbose_flag)      
	fprintf(stderr, "****** Failed Initializing Clutter Map.\n\n");
      radar->v[CZ_INDEX] = NULL;
      return(radar);
    }
    
    if (radar_verbose_flag)
      fprintf(stderr,"****** Applying Clutter Map: %s\n\n",clutter_map_fname);
    
    /* Apply clutter map to CZ volume */
    radar->v[CZ_INDEX] = applyClutterMap(radar->v[CZ_INDEX], clutter_map, clutter_map_size); 
    
    if (radar->v[CZ_INDEX] == NULL)
      {
	if(radar_verbose_flag)   
	  fprintf(stderr, "****** Failed Applying Clutter Map.\n\n");
	return(radar);
      }
  }  

  /*
   * If the Z calibration factor is non-zero, adjust all CZ
   * values by the factor.
   */
  if ((qcParm[ZCAL] != 0.0) && (radar->v[CZ_INDEX] != NULL))
	add_calibr_factor(radar->v[CZ_INDEX], qcParm[ZCAL]);

  return(radar);
}

