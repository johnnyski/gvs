#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <math.h>
/* BADVAL, NOECHO,... */
#include "rsl.h"

#include "aiw.h"
#include "compress.h"
/* Although these two conversion functions use BAVAL, RFVAL, etc., it
 * doesn't matter what their values are.  So long as one function is 
 * the inverse of the other.  -JHM 3/6/96.
 *
 */

float HMLF(int x)
{
  if (x == 0) return (float)BADVAL;
  if (x == 1) return (float)RFVAL;
  if (x == 2) return (float)APFLAG;  
  if (x == 3) return (float)NOECHO;  
  return ((float)((x-4)/2.0 - 64.0));
}

unsigned char aiw_invf(float x)
{
  if (x == BADVAL) return (unsigned char)0;
  if (x == RFVAL)  return (unsigned char)1;
  if (x == APFLAG)  return (unsigned char)2;
  if (x == NOECHO)  return (unsigned char)3;
  return (unsigned char)(2*(x+64)+.5 + 4);
}

void readAzimuth(FILE * fp1,sweep_header_t sweep,ray_header_t *ray,
                 float * value)
{
  int    ray_number,ray_azimuth,nprep;
  int    prep[2000];
  int    i,tmp[2000];
  double start_time,ray_time,delta;

  /* 
  ** The sweep header provides the defaults for the ray header, so copy
  ** it to the ray header. Then read ray header from the RLE file. This
  ** overrides some of the fields.
  */

  memcpy((void *)ray,(void *)&sweep,(size_t)(sizeof(sweep_header_t)));
  fscanf(fp1,"%d %d %d %lg\n",&ray_number,&ray_azimuth,&nprep,&delta);

  /* 
  ** If the ray_number field is -1, then a complete custom header
  ** has been encoded. Read it.
  */

  if (ray_number == -1) {

    /* 
    ** We have a custom header; read it. The header is followed by
    ** the normal header, which contains junk values. Read, but
    ** ignore.
    */
    readSweepHeader(fp1,ray);
    fscanf(fp1,"%d %d %d %lg\n",&ray_number,&ray_azimuth,&nprep,&delta);
    ray->azimuth = (float)ray_azimuth/(64.0);
  }
  else {

    ray->number = ray_number;
    ray->ray_number = sweep.ray_number + ray->number;
    ray->azimuth = (float)ray_azimuth/(64.0);
    start_time = sweep.hour*3600.0 + sweep.minute*60.0 + sweep.second;
    ray_time = start_time + delta;
    ray->hour = (int)(ray_time/3600.0);
    ray->minute = (int)((ray_time - ray->hour*3600)/60);
    ray->second  = (float)(ray_time - ray->hour*3600 - ray->minute*60);
  }
  
  /* Now read the data and decode/uncompress */
  asciiDecode(fp1,prep,nprep);
  prepUnCompress(prep,nprep,tmp,&i);
  ray->nbins = i;

  /* 
  ** In almost all cases sweep.nbins == ray->nbins. However, there may be 
  ** cases where this is not true. In other words, the number of bins in a
  ** ray is different from the number of bins in the other rays in this
  ** sweep. If this ray is such a ray, first clear the array that holds the
  ** the return values. This will avert problems if a user uses sweep.nbins
  ** in his/her code.
  */ 

  if (ray->nbins != sweep.nbins){
    for (i=0;i<=sweep.nbins-1;i++)
      value[i] = HMLF(0);
  }
  
  /* Now generate the return values */
  
  for(i=0;i<=ray->nbins-1;i++)
    value[i] = HMLF(tmp[i]);
}

void dumpAzimuth(FILE *fp1,ray_header_t ray, int *value)
{
  /* 
  ** For debugging purposes: dumps major parts of the ray header, and
  ** then the ray values to the file associated with fp1.
  */

  int  i;

  fprintf(fp1,"%d %d %d %ld %d %d %d %d %d %7.3f %d %g %g %g %g\n",ray.number,
          (int)(ray.azimuth*64.0),ray.nbins,ray.julian_day,ray.year,
          ray.month,ray.day,ray.hour,ray.minute,ray.second,ray.gate_size, 
          ray.frequency,ray.pulse_width,ray.pulse_count,ray.wavelength);
  for (i=0;i<=ray.nbins-1;i++)
    fprintf(fp1,"%d ",value[i]);
  fprintf(fp1,"\n");
}

void writeSweepHeader(FILE *fp,sweep_header_t sweep)
{
  fprintf(fp,"%d ",sweep.nheader_lines);     /* number of lines in header */
  fprintf(fp,"%4.2f ",sweep.version);        /* ASCII-RLE version */
  fprintf(fp,"%d ",sweep.header_type);       /* 0 => RLE binary */
                                             /* 1 => RLE ASCII-coded */
  fprintf(fp,"%d ",sweep.radar_id);          /* unique integer ID for radar */
  if (strcmp(sweep.radar_name, "") == 0)
	fprintf(fp,"\"unknown\" ");    /* name of radar, in quotes */  
  else
	fprintf(fp,"\"%s\" ",sweep.radar_name);    /* name of radar, in quotes */  
  fprintf(fp,"%d ",sweep.type_of_data);      /* 0 => RAW reflectivity */
                                             /* 1 => QC reflectivity */
  fprintf(fp,"%d ",sweep.nsweeps);           /* # of sweeps in this volume */
  fprintf(fp," %d ",sweep.latd);
  fprintf(fp," %d ",sweep.latm);
  fprintf(fp," %d ",sweep.lats);
  fprintf(fp," %d ",sweep.lond);
  fprintf(fp," %d ",sweep.lonm);
  fprintf(fp," %d ",sweep.lons);
  
  /* Variables that (may) change every sweep follow */

  fprintf(fp,"%d ",sweep.nrays);             /* number of rays in this sweep */
  fprintf(fp,"%d ",sweep.elevation_number);  /* elevation # within volume */
  fprintf(fp,"%d ",sweep.range_bin1);        /* range to first gate (m) */
  fprintf(fp,"%d ",sweep.gate_size);         /* data gate size */
  fprintf(fp,"%d ",sweep.sector_number);     /* sector # within cut */
  fprintf(fp,"%d ",sweep.prf);               /* pulse repetition freq. (Hz) */
  fprintf(fp,"%g ",sweep.rays_per_second);   /* number of rays per second */
  fprintf(fp,"%g ",sweep.unambiguous_range); /* unambiguous range (km) */
  fprintf(fp,"%g",sweep.beam_width);         /* see "angle_units" */
  fprintf(fp,"\n");
  fprintf(fp,"%g ",sweep.elevation);          /* see "angle_units" */
  fprintf(fp,"%g ",sweep.system_calibration); /* gain calibration constant */
  fprintf(fp,"%g ",sweep.velocity_resolution);/* doppler velocity resolution */
  fprintf(fp,"%g ",sweep.sweep_rate);	       /* sweep rate (sweeps/min) */
  fprintf(fp,"%g ",sweep.azimuth_rate);       /*  */
  fprintf(fp,"%g ",sweep.fix_angle);	       /*  */
  fprintf(fp,"%g ",sweep.pulse_count);	       /*  */
  fprintf(fp,"%g ",sweep.pulse_width);	       /* pulse width (micro-sec) */
  fprintf(fp,"%g ",sweep.frequency);	       /* bandwidth (MHz) */
  fprintf(fp,"%g ",sweep.wavelength);	       /* wavelength (m) */
  fprintf(fp,"%g ",sweep.nyquist_velocity);   /* Nyquist velocity (m/s) */
  fprintf(fp,"%g ",sweep.attenuation);	       /* atmospheric att. factor. */
  fprintf(fp,"%g",sweep.minimum_delta);       /*  */
  fprintf(fp,"\n");

  /* Variables that (may) change every ray follow */

  fprintf(fp,"%ld ",sweep.julian_day);        /* julian day  */
  fprintf(fp,"%d ",sweep.year);	             /* e.g. 1994 */
  fprintf(fp,"%d ",sweep.month);	             /* 1 = January, 12 = December */
  fprintf(fp,"%d ",sweep.day);	             /* range 1...31 */
  fprintf(fp,"%d ",sweep.hour);	             /* range 0...32 */
  fprintf(fp,"%d ",sweep.minute);	          /* range: 0-59 */
  fprintf(fp,"%g ",sweep.second);	          /* range: 0-59 */
  fprintf(fp,"%s ",sweep.time_units);	       /* "GMT" or "LOCAL" */
  fprintf(fp,"%d ",sweep.status);	          /* 0 = new elevation (sweep) */
                                              /* 1 = other */
  fprintf(fp,"%d ",sweep.number);	          /* number of this sweep/ray */
  fprintf(fp,"%d ",sweep.ray_number);	       /* full of this ray */
  fprintf(fp,"%d ",sweep.nbins);	             /* sweep: max, ray: actual   */
  fprintf(fp,"%d ",sweep.nonzero);	          /* # of nonzero rays or bins */
  fprintf(fp,"%d ",sweep.smallest_value);	    /* smallest valid data value */
  fprintf(fp,"%d ",sweep.largest_value);	    /* largest valid data value */
  fprintf(fp,"%g ",sweep.azimuth);	          /* sweep: azimuth of first ray */
                                              /* ray: actual azimuth */
  fprintf(fp,"%s",sweep.angle_units);	       /* "RAD" or "DEG" */
  fprintf(fp,"\n");
  fprintf(fp,"%ld ",sweep.warning_number);    /* warning number */
  fprintf(fp,"%ld",sweep.error_number);       /* error number */
  fprintf(fp,"\n");
}

void readSweepHeader(FILE *fp,sweep_header_t *sweep)
{

  /* 
  ** Early versions of the file format all had 3 header lines but no
  ** explicit header version information.
  */
 
  fscanf(fp," %d",&sweep->nheader_lines);
  if (sweep->nheader_lines > 3)
    fscanf(fp,"%g ",&sweep->version);         /* ASCII-RLE version */
  else
    sweep->version = UNKNOWN;

  fscanf(fp," %d",&sweep->header_type);
  fscanf(fp," %d",&sweep->radar_id);

  /* Read name of radar, which is in quotes */

  fscanf(fp,"%*[ \"]");                      /* read up (including) " */
  fscanf(fp,"%[^\"]",sweep->radar_name);     /* read up (excluding) " */
  fscanf(fp,"%*[\"]");                       /* read final "  */

  fscanf(fp," %d",&sweep->type_of_data);
  fscanf(fp," %d",&sweep->nsweeps);
  fscanf(fp," %d",&sweep->latd);
  fscanf(fp," %d",&sweep->latm);
  fscanf(fp," %d",&sweep->lats);
  fscanf(fp," %d",&sweep->lond);
  fscanf(fp," %d",&sweep->lonm);
  fscanf(fp," %d",&sweep->lons);
  
  fscanf(fp," %d",&sweep->nrays);
  fscanf(fp," %d",&sweep->elevation_number);
  fscanf(fp," %d",&sweep->range_bin1);
  fscanf(fp," %d",&sweep->gate_size);
  fscanf(fp," %d",&sweep->sector_number);
  fscanf(fp," %d",&sweep->prf);
  fscanf(fp," %g",&sweep->rays_per_second);
  fscanf(fp," %g",&sweep->unambiguous_range);
  fscanf(fp," %g",&sweep->beam_width);
  fscanf(fp," %g",&sweep->elevation);
  fscanf(fp," %g",&sweep->system_calibration);
  fscanf(fp," %g",&sweep->velocity_resolution);
  fscanf(fp," %g",&sweep->sweep_rate);
  fscanf(fp," %g",&sweep->azimuth_rate);
  fscanf(fp," %g",&sweep->fix_angle);
  fscanf(fp," %g",&sweep->pulse_count);
  fscanf(fp," %g",&sweep->pulse_width);
  fscanf(fp," %g",&sweep->frequency);
  fscanf(fp," %g",&sweep->wavelength);
  fscanf(fp," %g",&sweep->nyquist_velocity);
  fscanf(fp," %g",&sweep->attenuation);
  fscanf(fp," %g",&sweep->minimum_delta);
  fscanf(fp," %ld",&sweep->julian_day);
  fscanf(fp," %d",&sweep->year);
  fscanf(fp," %d",&sweep->month);
  fscanf(fp," %d",&sweep->day);
  fscanf(fp," %d",&sweep->hour);
  fscanf(fp," %d",&sweep->minute);
  fscanf(fp," %g",&sweep->second);
  fscanf(fp," %s",sweep->time_units);
  fscanf(fp," %d",&sweep->status);
  fscanf(fp," %d",&sweep->number);
  fscanf(fp," %d",&sweep->ray_number);
  fscanf(fp," %d",&sweep->nbins);

  fscanf(fp," %d",&sweep->nonzero);
  fscanf(fp," %d",&sweep->smallest_value);
  fscanf(fp," %d",&sweep->largest_value);
  fscanf(fp," %g",&sweep->azimuth);
  fscanf(fp," %s",sweep->angle_units);

  /* 
  ** Early versions of the file format all had 3 header lines but no
  ** explicit header version information. These files also had no
  ** warning or error numbers;
  */

  sweep->warning_number = UNKNOWN;
  sweep->error_number = UNKNOWN;
  if (sweep->nheader_lines > 3) {
    fscanf(fp," %ld",&sweep->warning_number);
    fscanf(fp," %ld",&sweep->error_number);
  }
  fscanf(fp,"\n");
}








