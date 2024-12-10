/*
 * truncate_volume.c
 *
 * Truncate the input volume to a max range of max_range km and 
 * set all dbz'z < min_dbz == min<dbz.
 *
 * Routines written by: Thuy Nguyen, modified for min_dbz by David Wolff
 *  
 * 7/26/95
 * Used untouched in qcwin program.
 */

#include <stdio.h>
#include <math.h>
#include "rsl.h"
#include "truncate_volume.h"

extern int radar_verbose_flag;

Ray * truncate_ray(Ray * r, float max_range, float min_dbz)
   {
   int i;
   float x, current_max_range;

   if(r==NULL) return NULL;
	
   /* Convert range in km to meters */
   current_max_range = r->h.range_bin1/1000.0 +
      r->h.nbins*r->h.gate_size/1000.0;
   if(max_range > current_max_range) {
	 max_range = current_max_range;
   }


   r->h.nbins =  (int)((max_range * 1000) - r->h.range_bin1) / r->h.gate_size;
	
   /* Set all dbz<min_dbz == min_dbz */
   for(i=0; i<r->h.nbins; i++) {
	 x = r->h.f(r->range[i]);
	 if(x < min_dbz) x = min_dbz;
	 r->range[i] = r->h.invf(x);
   }
   return r;
   }

Sweep * truncate_sweep(Sweep *s, float max_range, float min_dbz)
   {
   int i;
   if (s == NULL)
      return NULL;
   for (i = 0; i < s->h.nrays; i++) {
	 if (s->ray)
	   truncate_ray(s->ray[i], max_range, min_dbz);
   }
   return s;
   }

Volume * truncate_volume(Volume *v, float max_range, float min_dbz)
   {
   int i;
   if (v == NULL)
      return NULL;
   for (i = 0; i < v->h.nsweeps; i++) {
	 if (v->sweep)
	   truncate_sweep(v->sweep[i], max_range, min_dbz);
   }

   if (radar_verbose_flag)
   printf("********** max_range = %f;  min_dbz = %f\n", max_range, min_dbz);
   return v;
   }
