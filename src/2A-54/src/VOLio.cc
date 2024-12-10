#include <volume.h>
#include <stdio.h>
#include <string.h>

extern "C" {
   #include "exitCodes.h"
}
// This function reads one radar volume from a file.  It then places
// field number nfield in the 3-d array used by the analysis routines.
int Volume::readRadarVolume(const int nfield)
{
  int i = 0;
  int j = 0;
  int k = 0;

  // Read the radar volume scan data from the input file (MUDRAS Format).
  FILE *fp = fopen(params.inputVolumeFn, "r");

  if (!fp)
    {
      fprintf(stderr, "Problem opening file %s\n", params.inputVolumeFn);
      return(1);
    }

  // Now read the radar data in the format specified.
  if (params.radarFormat == 0)
    {
      if (!mudfile.readCSUMudrasVolume(fp))
	{
	  fprintf(stderr, "Problem reading mudras file into array.\n");
	  return(1);
	}
    }
  else if (params.radarFormat == 1)
    {
      if (!mudfile.readCedricTapeVolume(fp))
	{
	  fprintf(stderr, "Problem reading mudras file into array.\n");
	  return(1);
	}
    }
  else if (params.radarFormat == 2)
    {
      if (!mudfile.readCedricDiskVolume(fp))
	{
	  fprintf(stderr, "Problem reading mudras file into array.\n");
	  return(1);
	}
    }
  else if (params.radarFormat == 3)
    {
      if (!mudfile.readPureCedricVolume(fp, params.nVolume))
	{
	  fprintf(stderr, "Problem reading mudras file into array.\n");
	  return(1);
	}
    }

  mudfile.getField(nfield, (float *)grids.reflog,
		   MAX_X_DIM, MAX_Y_DIM, MAX_Z_DIM,
		   MISSINGV);

  // Copy some dimension information.
  params.year   = mudfile.header.VolStart.Year;
  params.month  = mudfile.header.VolStart.Month;
  params.day    = mudfile.header.VolStart.Day;
  params.hour   = mudfile.header.VolStart.Hour;
  params.minute = mudfile.header.VolStart.Minute;
  params.time   = params.hour * 100 + params.minute;

  params.xymin  = float(mudfile.header.X.Min) / mudfile.header.SF;
  params.xymax  = float(mudfile.header.X.Max) / mudfile.header.SF;
  params.xydim  = mudfile.header.X.N;
  params.xyres  = mudfile.header.X.Spacing / 1000.0;

  params.zmin   = float(mudfile.header.Z.Min) / mudfile.header.SF;
  params.zmax   = float(mudfile.header.Z.Max) / mudfile.header.SF; 
  params.zdim   = mudfile.header.Z.N;
  params.zres   = mudfile.header.Z.Spacing / 1000.0;

  fclose(fp);

  // Now scan for the threshold filename.
  sprintf(thres_fn, "/home/disk/hail/joe/radar/old/thresholds");
  scanf("%s", thres_fn);
  fprintf(stderr, "Threshold filename = %s\n", thres_fn);
  
  // We now have the data.  Threshold it if we have to.
  {
    // If this filename is not "-", then try to read the file
    // into the threshold grid.
    if (strcmp(thres_fn, "-") != 0)
      {
	FILE *fp = fopen(thres_fn, "r");
	if (!fp)
	  {
	    fprintf(stderr, "Error: Cannot open threshold file.\n");
		TKreportWarning(W_2A54_OPEN_THRESHOLD_FILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	else
	  {
	    float *thres = new
	      float[params.xydim * params.xydim * params.zdim];
	    fread(thres,
		  params.xydim * params.xydim * params.zdim * sizeof(float),
		  1, fp);
	    fclose(fp);

	    // Theshold the data.
	    for (k = 0; k < params.zdim; k++)
	      {
		for (j = 0; j < params.xydim; j++)
		  {
		    for (i = 0; i < params.xydim; i++)
		      {
			float &v = grids.reflog[k][j][i];
			if (v < thres[k * params.xydim * params.xydim +
				      j * params.xydim + i])
			  {
			    v = MISSINGV;
			  }
		      }
		  }
	      }

	    delete[] thres;
	  }
      }
  }
  
  return(0);
}

// ASCII output function.  This function writes out
// the various data sets based upon a single volume of radar data
// as required by the TRMM Ground Truth algorithm
// Intercomparison Workshop Format Specifications.
int Volume::writeASCII(void)
{
  int i = 0;
  int j = 0;
  int k = 0;
  int l = 0;
  int m = 0;

  if (params.ASCIIformat < 0) return(0);

  if (params.ASCIIformat == 0)
    {
      // Write out an ASCII summary file summarizing some of our results.
      char fn[256];
      sprintf(fn, "%s/results.%s.%02d%02d%02d.%02d%02d",
	      params.outputPath,
	      params.siteName,
	      params.year,
	      params.month,
	      params.day,
	      params.hour,
	      params.minute);
      
      FILE *fp = fopen(fn, "w");
      for (j = 0; j < params.xydim; j++)
	{
	  for (i = 0; i < params.xydim; i++)
	    {
	      fprintf(fp, "%3d %3d %7.2f %8.2f %2d %2d\n", j, i,
		      grids.reflmap[j][i],
		      grids.Rint_map[j][i],
		      grids.rain_type[j][i],
		      grids.geomap[j][i]);
	    }
	}
      fclose(fp);
    }

  if (params.ASCIIformat == 2)
    {
      // Write out an ASCII summary file summarizing some of our results.
      char fn[256];

      sprintf(fn, "%s/%s.%02d%02d%02d.%02d%02d.csmap",
	      params.outputPath,
	      params.siteName,
	      params.year,
	      params.month,
	      params.day,
	      params.hour,
	      params.minute);
      
      FILE *fp = fopen(fn, "w");

      {
	for (j = 0; j < params.xydim; j++)
	  {
	    for (i = 0; i < params.xydim; i++)
	      {
		fprintf(fp, "%1d ", grids.rain_type[params.xydim - 1 - j][i]);
	      }
	    fprintf(fp, "\n");
	  }
      }

      fclose(fp);

      // That's it for this format.
      return(0);
    }
  
  // 2A-53:  "Radar Site Rain Map": Instantaneous
  // (highest temporal, 2 km horizontal
  // resolution) rain rate maps to be produced from 1C-51 reflectivity 
  // fields. These require the application of an appropriate reflectivity 
  // (Z) to rain (R) transformation. 
  {
    char fn[MAX_LINE];
    if (params.ASCIIformat == 1)
      {
	sprintf(fn, "%s/%s.%02d%02d%02d.%02d%02d.rrmap",
		params.outputPath,
		params.siteName,
		params.year,
		params.month,
		params.day,
		params.hour,
		params.minute);
      }
    else
      {
	sprintf(fn, params.outputRainIntFn);
      }

    FILE *fp = fopen(fn, "w");
    if (!fp)
      {
	fprintf(stderr, "Error opening ASCII output file '%s'.\n", fn);
	return(0);
      }

    if (params.ASCIIformat == 1)
      {
	for (j = 0; j < params.xydim; j++)
	  {
	    for (i = 0; i < params.xydim; i++)
	      {
		fprintf(fp, "%8.2f ", grids.Rint_map[j][i]);
	      }
	    fprintf(fp, "\n");
	  }
      }
    else
      {
	for (j = 0; j < params.xydim; ++j)
	  {
	    for (i = 0; i < params.xydim; ++i)
	      {
		fprintf(fp, "%3d %3d %8.2f\n", j, i, grids.Rint_map[j][i]);
	      }
	  }
      }
    fclose(fp);
  }

  // 2A-54:  "Radar Site Convective/Stratiform Map": Instantaneous (highest 
  // temporal, 2 km horizontal resolution) maps identifying the surface 
  // precipitation as convective or stratiform. This product will be 
  // generated from the reflectivity product 1C-51. 
  {
    char fn[MAX_LINE];
    if (params.ASCIIformat == 1)
      {
	sprintf(fn, "%s/%s.%02d%02d%02d.%02d%02d.csmap", 
		params.outputPath,
		params.siteName,
		params.year,
		params.month,
		params.day,
		params.hour,
		params.minute);
      }
    else
      {
	sprintf(fn, params.outputConvStraMapFn);
      }

    FILE *fp = fopen(fn, "w");
    if (!fp)
      {
	fprintf(stderr, "Error opening ASCII output file.\n");
	return(0);
      }

    if (params.ASCIIformat == 1)
      {
	for (j = 0; j < params.xydim; j++)
	  {
	    for (i = 0; i < params.xydim; i++)
	      {
		fprintf(fp, "%1d ", grids.rain_type[j][i]);
	      }
	    fprintf(fp, "\n");
	  }
      }
    else
      {
	for (j = 0; j < params.xydim; ++j)
	  {
	    for (i = 0; i < params.xydim; ++i)
	      {
		fprintf(fp, "%3d %3d %7.2f %2d %7.2f %3d %3d %7.2f %2d\n",
			i, j, grids.reflmap[j][i], grids.rain_type[j][i],
			grids.bg_Zint[j][i],
			0, 0, 0.0, grids.range_mask[j][i]);
	      }
	  }
      }
    
    fclose(fp);
  }

  //  2A-55:  "Radar Site 3-D Reflectivities":
  //  Instantaneous (highest temporal, 2 km
  //  horizontal resolution) interpolated from volume scans onto a 3-D 
  //  Cartesian grid with 1.5 km vertical resolution. The origin and outer 
  //  boundaries of the 3-D Cartesian grid will be specified by the TRMM 
  //  Ground Truth Team. In addition to the 3D gridded reflectivity data, 
  //  this product will include vertical profiles of the mean reflectivity 
  //  and the frequency distribution at each height. The frequency 
  //  distribution will be provided in the form of contoured frequency by 
  //  altitude diagrams (CFADs). Vertical profiles and CFADs will be provided
  //  for the following categories: total, total-land, total-ocean, 
  //  total-convective, total-stratiform, total-anvil, land-convective, 
  //  land-stratiform, land-anvil, ocean-convective, ocean-stratiform, 
  //  ocean-anvil. 
  //  NOTE: Requires TWO sets of product files for workshop; mean profile 
  //  specified by Format 5, and CFADs determined by Format 6. 
  //  Here are the categories of domains over which statistics are calculated.
  //                 01 -> total
  //                 02 -> land
  //                 03 -> ocean
  //                 04 -> convective
  //                 05 -> convective-land
  //                 06 -> convective-ocean
  //                 07 -> stratiform
  //                 08 -> stratiform-land
  //                 09 -> stratiform-ocean
  //                 10 -> no surface rain (anvil)
  //                 11 -> no surface rain-land
  //                 12 -> no surface rain-ocean
  // First write out the mean profile grid.
  {
    char fn[MAX_LINE];
    if (params.ASCIIformat == 1)
      {
	sprintf(fn, "%s/%s.%02d%02d%02d.%02d%02d.vertp", 
		params.outputPath,
		params.siteName,
		params.year,
		params.month,
		params.day,
		params.hour,
		params.minute);
      }
    else
      {
	sprintf(fn, params.outputMeanVertProfFn);
      }

    FILE *fp = fopen(fn, "w");
    if (!fp)
      {
	fprintf(stderr, "Error opening ASCII output file.\n");
	return(0);
      }
    
    if (params.ASCIIformat == 1)
      {
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "%8.2f ",
		    prof.mean_Zprof_tot[k]);
	  }
	fprintf(fp, "\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "%8.2f ",
		    prof.mean_Zprof_land[k]);
	  }
	fprintf(fp, "\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "%8.2f ",
		    prof.mean_Zprof_sea[k]);
	  }
	fprintf(fp, "\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "%8.2f ",
		    prof.mean_Zprof_conv[k]);
	  }
	fprintf(fp, "\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "%8.2f ",
		    prof.mean_Zprof_conv_land[k]);
	  }
	fprintf(fp, "\n");
	for (k = 0; k < params.zdim; ++k) 
	  {
	    fprintf(fp, "%8.2f ",
		    prof.mean_Zprof_conv_sea[k]);
	  }
	fprintf(fp, "\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "%8.2f ",
		    prof.mean_Zprof_stra[k]);
	  }
	fprintf(fp, "\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "%8.2f ",
		    prof.mean_Zprof_stra_land[k]);
	  }
	fprintf(fp, "\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "%8.2f ",
		    prof.mean_Zprof_stra_sea[k]);
	  }
	fprintf(fp, "\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "%8.2f ",
		    prof.mean_Zprof_anvil[k]);
	  }
	fprintf(fp, "\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "%8.2f ",
		    prof.mean_Zprof_anvil_land[k]);
	  }
	fprintf(fp, "\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "%8.2f ",
		    prof.mean_Zprof_anvil_sea[k]);
	  }
      }
    else
      {
	fprintf(fp, "   Category 01: total\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "      %2d  %8.2f [dBZ]  %10d pixels\n", k,
		    prof.mean_Zprof_tot[k], prof.count_Zprof_tot[k]);
	  }
	fprintf(fp, "\n");
	fprintf(fp, "   Category 02: land\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "      %2d  %8.2f [dBZ]  %10d pixels\n", k,
		    prof.mean_Zprof_land[k], prof.count_Zprof_land[k]);
	  }
	fprintf(fp, "\n");
	fprintf(fp, "   Category 03: ocean\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "      %2d  %8.2f [dBZ]  %10d pixels\n", k,
		    prof.mean_Zprof_sea[k], prof.count_Zprof_sea[k]);
	  }
	fprintf(fp, "\n");
	fprintf(fp, "   Category 04: convective\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "      %2d  %8.2f [dBZ]  %10d pixels\n", k,
		    prof.mean_Zprof_conv[k], prof.count_Zprof_conv[k]);
	  }
	fprintf(fp, "\n");
	fprintf(fp, "   Category 05: convective-land\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "      %2d  %8.2f [dBZ]  %10d pixels\n", k,
		    prof.mean_Zprof_conv_land[k],
		    prof.count_Zprof_conv_land[k]);
	  }
	fprintf(fp, "\n");
	fprintf(fp, "   Category 06: convective-ocean\n");
	for (k = 0; k < params.zdim; ++k) 
	  {
	    fprintf(fp, "      %2d  %8.2f [dBZ]  %10d pixels\n", k,
		    prof.mean_Zprof_conv_sea[k], prof.count_Zprof_conv_sea[k]);
	  }
	fprintf(fp, "\n");
	fprintf(fp, "   Category 07: stratiform\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "      %2d  %8.2f [dBZ]  %10d pixels\n", k,
		    prof.mean_Zprof_stra[k], prof.count_Zprof_stra[k]);
	  }
	fprintf(fp, "\n");
	fprintf(fp, "   Category 08: stratiform-land\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "      %2d  %8.2f [dBZ]  %10d pixels\n", k,
		    prof.mean_Zprof_stra_land[k],
		    prof.count_Zprof_stra_land[k]);
	  }
	fprintf(fp, "\n");
	fprintf(fp, "   Category 09: stratiform-ocean\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "      %2d  %8.2f [dBZ]  %10d pixels\n", k,
		    prof.mean_Zprof_stra_sea[k],
		    prof.count_Zprof_stra_sea[k]);
	  }
	fprintf(fp, "\n");
	fprintf(fp, "   Category 10: anvil (no surface rainfall)\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "      %2d  %8.2f [dBZ]  %10d pixels\n", k,
		    prof.mean_Zprof_anvil[k], prof.count_Zprof_anvil[k]);
	  }
	fprintf(fp, "\n");
	fprintf(fp, "   Category 11: anvil-land\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "      %2d  %8.2f [dBZ]  %10d pixels\n", k,
		    prof.mean_Zprof_anvil_land[k],
		    prof.count_Zprof_anvil_land[k]);
	  }
	fprintf(fp, "\n");
	fprintf(fp, "   Category 12: anvil-ocean\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fprintf(fp, "      %2d  %8.2f [dBZ]  %10d pixels\n", k,
		    prof.mean_Zprof_anvil_sea[k],
		    prof.count_Zprof_anvil_sea[k]);
	  }
	
	fprintf(fp, "\n");
      }
    fclose(fp);
  }
  
  // Now write out the CFAD grids.  Note that there are 12 files
  // corresponding to CFADS over the various domains.
  {
    if (params.ASCIIformat == 1)
      {
	// Write out the CFAD data here.

	// Category 01: total
	char fn[MAX_LINE];
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		1);
	FILE *fp = fopen(fn, "w");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_OFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_tot[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fclose(fp);

	// Category 02: land
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		2);
	fp = fopen(fn, "w");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_OFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_land[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fclose(fp);

	// Category 03: ocean
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		3);
	fp = fopen(fn, "w");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_OFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_sea[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fclose(fp);

	// Category 04: convective
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		4);
	fp = fopen(fn, "w");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_OFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_conv[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fclose(fp);

	// Category 05: convective - land
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		5);
	fp = fopen(fn, "w");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_OFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_conv_land[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fclose(fp);

	// Category 06: convective - ocean\n
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		6);
	fp = fopen(fn, "w");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_OFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_conv_sea[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fclose(fp);

	// Category 07: stratiform
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		7);
	fp = fopen(fn, "w");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_OFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_stra[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fclose(fp);

	// Category 08: stratiform - land
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		8);
	fp = fopen(fn, "w");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_OFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_stra_land[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fclose(fp);

	// Category 09: stratiform - ocean
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		9);
	fp = fopen(fn, "w");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_OFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_stra_sea[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fclose(fp);

	// Category 10: anvil
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		10);
	fp = fopen(fn, "w");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_OFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_anvil[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fclose(fp);

	// Category 11: anvil - land
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		11);
	fp = fopen(fn, "w");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_OFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_anvil_land[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fclose(fp);

	// Category 12: anvil - ocean
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		12);
	fp = fopen(fn, "w");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_OFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l) 
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_anvil_sea[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fclose(fp);
      }
    else
      {
	char fn[MAX_LINE];
	sprintf(fn, params.outputCFADFn);
	FILE *fp = fopen(fn, "w");
	fprintf(fp, "Category 01: total\n");
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_tot[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fprintf(fp, "\n");
	fprintf(fp, "Category 02: land\n");
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_land[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fprintf(fp, "\n");
	fprintf(fp, "Category 03: ocean\n");
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_sea[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fprintf(fp, "\n");
	fprintf(fp, "Category 04: convective\n");
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_conv[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
        fprintf(fp, "\n");
	fprintf(fp, "Category 05: convective - land\n");
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_conv_land[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fprintf(fp, "\n");
	fprintf(fp, "Category 06: convective - ocean\n");
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_conv_sea[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fprintf(fp, "\n");
	fprintf(fp, "Category 07: stratiform\n");
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_stra[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fprintf(fp, "\n");
	fprintf(fp, "Category 08: stratiform - land\n");
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_stra_land[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fprintf(fp, "\n");
	fprintf(fp, "Category 09: stratiform - ocean\n");
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_stra_sea[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fprintf(fp, "\n");
	fprintf(fp, "Category 10: anvil\n");
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_anvil[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fprintf(fp, "\n");
	fprintf(fp, "Category 11: anvil - land\n");
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_anvil_land[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fprintf(fp, "\n");
	fprintf(fp, "Category 12: anvil - ocean\n");
	for (l = 0; l <= cfad.max_Zindex; ++l) 
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fprintf(fp, "%6d", cfad.count_Zhist_anvil_sea[k][l]);
	      }
	    fprintf(fp, "\n");
	  }
	fprintf(fp, "\n");
	fclose(fp);
      }
  }
  
  // Write out the range-dependant CFAD information.
  if (params.ASCIIformat == 0)
    {
      FILE *fp = fopen(params.outputCFADRangeFn, "w");
      if (!fp)
	{
	  fprintf(stderr, "Sorry, can't open output file %s\n",
		  params.outputCFADRangeFn);
	  TKreportWarning(W_2A54_OPEN_CFADRANGE_OFILE_FAILED);
	  exit(FAILED_CODE2);
	}
      
      for (m = 0; m < RANGE_CLASS-1; ++m)
	{
	  fprintf(fp, "CFAD #%2d: Range %3.0f - %3.0f km\n", m+1,
		  m * RANGE_RES, (m+1) * RANGE_RES);
	  for (l = 0; l <= cfad.max_Zindex; ++l)
	    {
	      for (k = 0; k < params.zdim; ++k)
		{
		  fprintf(fp, "%6d", cfad.count_Zhist_range[m][k][l]);
		}
	      fprintf(fp, "\n");
	    }
	  fprintf(fp, "\n");
        }	
      fprintf(fp, "CFAD #%2d: Range     > %3.0f km\n", m+1, m * RANGE_RES);
      for (l = 0; l <= cfad.max_Zindex; ++l)
	{
	  for (k = 0; k < params.zdim; ++k)
	    {
	      fprintf(fp, "%6d", cfad.count_Zhist_range[m][k][l]);
	    }
	  fprintf(fp, "\n");
	}
      fprintf(fp, "\n");
    }
  
  return(0);
}

// Binary output function.  This function writes out
// the various data sets required by the time integration routines
// which produce time-averaged products required by the
// TRMM GT specifications.  The idea here is to create one binary file
// that contains all products that need to be integrated in time
// to produce new products.
int Volume::writeBinary(void)
{
  // Open the binary file.
  char fn[MAX_LINE];
  sprintf(fn, "%s/%s.%s.%s.bin",
	  params.outputPath,
	  params.siteName,
	  params.dateString,
	  params.timeString);
  FILE *fp = fopen(fn, "w");
  if (!fp)
    {
      fprintf(stderr, "Error opening binary output file.\n");
      return(0);
    }

  FILE *fpold = params.fpout1;
  params.fpout1 = 0;
  fwrite(&params, sizeof(params), 1, fp);
  params.fpout1 = fpold;
  
  fwrite(&prof, sizeof(prof), 1, fp);
  fwrite(&cfad, sizeof(cfad), 1, fp);
  fwrite(&echoArea, sizeof(echoArea), 1, fp);
  fwrite(&arealRain, sizeof(arealRain), 1, fp);
  fwrite(site, sizeof(site), 1, fp);

  // Only write out certain grids.
  fwrite(grids.Rint_map, sizeof(grids.Rint_map), 1, fp);
  fwrite(grids.geomap, sizeof(grids.geomap), 1, fp);
  fwrite(grids.range_mask, sizeof(grids.range_mask), 1, fp);
  fwrite(grids.rain_type, sizeof(grids.rain_type), 1, fp);
  fwrite(grids.bg_Zint, sizeof(grids.bg_Zint), 1, fp);

  // Working maps.
  fwrite(grids.reflmap, sizeof(grids.reflmap), 1, fp);
  fwrite(grids.reflinmap, sizeof(grids.reflinmap), 1, fp);

  fclose(fp);
  return(0);
}

int Volume::writeBinaryAIW(FILE *fp)
{
  FILE *fpold = params.fpout1;
  params.fpout1 = 0;
  fwrite(&params, sizeof(params), 1, fp);
  params.fpout1 = fpold;
  
  fwrite(&prof, sizeof(prof), 1, fp);
  fwrite(&cfad, sizeof(cfad), 1, fp);
  //fwrite(&echoArea, sizeof(echoArea), 1, fp);
  //fwrite(&arealRain, sizeof(arealRain), 1, fp);
  //fwrite(site, sizeof(site), 1, fp);

  // Only write out certain grids.
  fwrite(grids.Rint_map, sizeof(grids.Rint_map), 1, fp);
  //fwrite(grids.geomap, sizeof(grids.geomap), 1, fp);
  //fwrite(grids.range_mask, sizeof(grids.range_mask), 1, fp);
  fwrite(grids.rain_type, sizeof(grids.rain_type), 1, fp);
  //fwrite(grids.bg_Zint, sizeof(grids.bg_Zint), 1, fp);

  // Working maps.
  //fwrite(grids.reflmap, sizeof(grids.reflmap), 1, fp);
  //fwrite(grids.reflinmap, sizeof(grids.reflinmap), 1, fp);

  return(0);
}

// Binary input function.  This function reads in
// the various data sets required by the time integration routines
// which produce time-averaged products required by the
// TRMM GT specifications.
int Volume::readBinary(const char *fn)
{
  fprintf(stdout, "Now reading binary file.\n");
  FILE *fp = fopen(fn, "r");
  if (!fp)
    {
      fprintf(stderr, "Error opening binary intput file.\n");
      return(1);
    }

  FILE *fpold = params.fpout1;
  fread(&params, sizeof(params), 1, fp);
  params.fpout1  = fpold;

  if (fread(&prof, sizeof(prof), 1, fp) <= 0) return(1);
  if (fread(&cfad, sizeof(cfad), 1, fp) <= 0) return(1);
  if (fread(&echoArea, sizeof(echoArea), 1, fp) <= 0) return(1);
  if (fread(&arealRain, sizeof(arealRain), 1, fp) <= 0) return(1);
  if (fread(site, sizeof(site), 1, fp) <= 0) return(1);

  // Only read in certain grids.
  if (fread(grids.Rint_map, sizeof(grids.Rint_map), 1, fp) <= 0) return(1);
  if (fread(grids.geomap, sizeof(grids.geomap), 1, fp) <= 0) return(1);
  if (fread(grids.range_mask, sizeof(grids.range_mask), 1, fp) <= 0) return(1);
  if (fread(grids.rain_type, sizeof(grids.rain_type), 1, fp) <= 0) return(1);
  if (fread(grids.bg_Zint, sizeof(grids.bg_Zint), 1, fp) <= 0) return(1);

  // Working maps.
  if (fread(grids.reflmap, sizeof(grids.reflmap), 1, fp) <= 0) return(1);
  if (fread(grids.reflinmap, sizeof(grids.reflinmap), 1, fp) <= 0) return(1);

  fclose(fp);

  return(0);
}

int Volume::readBinaryAIW(FILE *fp)
{
  FILE *fpold = params.fpout1;
  fread(&params, sizeof(params), 1, fp);
  params.fpout1  = fpold;

  fread(&prof, sizeof(prof), 1, fp);
  fread(&cfad, sizeof(cfad), 1, fp);
  //fread(&echoArea, sizeof(echoArea), 1, fp);
  //fread(&arealRain, sizeof(arealRain), 1, fp);
  //fread(site, sizeof(site), 1, fp);

  // Only read in certain grids.
  fread(grids.Rint_map, sizeof(grids.Rint_map), 1, fp);
  //fread(grids.geomap, sizeof(grids.geomap), 1, fp);
  //fread(grids.range_mask, sizeof(grids.range_mask), 1, fp);
  fread(grids.rain_type, sizeof(grids.rain_type), 1, fp);
  //fread(grids.bg_Zint, sizeof(grids.bg_Zint), 1, fp);

  // Working maps.
  //fread(grids.reflmap, sizeof(grids.reflmap), 1, fp);
  //fread(grids.reflinmap, sizeof(grids.reflinmap), 1, fp);

  return(0);
}

// ASCII input function.  This function reads in
// the various data sets based upon a single volume of radar data
// as required by the TRMM Ground Truth algorithm
// Intercomparison Workshop Format Specifications.
int Volume::readASCII(void)
{
  int i = 0;
  int j = 0; 
  int k = 0;
  int l = 0;

  if (params.ASCIIformat <= 0) return(0);
  
  // 2A-53:  "Radar Site Rain Map": Instantaneous
  // (highest temporal, 2 km horizontal
  // resolution) rain rate maps to be produced from 1C-51 reflectivity 
  // fields. These require the application of an appropriate reflectivity 
  // (Z) to rain (R) transformation. 
  {
    char fn[MAX_LINE];
    if (params.ASCIIformat == 1)
      {
	sprintf(fn, "%s/%s.%02d%02d%02d.%02d%02d.rrmap",
		params.outputPath,
		params.siteName,
		params.year,
		params.month,
		params.day,
		params.hour,
		params.minute);
      }
    else
      {
	sprintf(fn, params.outputRainIntFn);
      }

    FILE *fp = fopen(fn, "r");
    if (!fp)
      {
	fprintf(stderr, "Error opening ASCII output file '%s'.\n", fn);
	return(0);
      }

    if (params.ASCIIformat == 1)
      {
	for (j = 0; j < params.xydim; j++)
	  {
	    for (i = 0; i < params.xydim; i++)
	      {
		fscanf(fp, "%f ", &grids.Rint_map[j][i]);
	      }
	  }
      }
    else
      {
	for (j = 0; j < params.xydim; ++j)
	  {
	    for (i = 0; i < params.xydim; ++i)
	      {
		int ti, tj;
		fscanf(fp, "%3d %3d %f\n", &tj, &ti, &grids.Rint_map[j][i]);
	      }
	  }
      }
    fclose(fp);
  }

  // 2A-54:  "Radar Site Convective/Stratiform Map": Instantaneous (highest 
  // temporal, 2 km horizontal resolution) maps identifying the surface 
  // precipitation as convective or stratiform. This product will be 
  // generated from the reflectivity product 1C-51. 
  {
    char fn[MAX_LINE];
    if (params.ASCIIformat == 1)
      {
	sprintf(fn, "%s/%s.%02d%02d%02d.%02d%02d.csmap", 
		params.outputPath,
		params.siteName,
		params.year,
		params.month,
		params.day,
		params.hour,
		params.minute);
      }
    else
      {
	sprintf(fn, params.outputConvStraMapFn);
      }

    FILE *fp = fopen(fn, "r");
    if (!fp)
      {
	fprintf(stderr, "Error opening ASCII output file.\n");
	return(0);
      }

    if (params.ASCIIformat == 1)
      {
	for (j = 0; j < params.xydim; j++)
	  {
	    for (i = 0; i < params.xydim; i++)
	      {
		fscanf(fp, "%1d ", &grids.rain_type[j][i]);
	      }
	  }
      }
    else
      {
	return(1);
      }
    fclose(fp);
  }

  //  2A-55:  "Radar Site 3-D Reflectivities":
  //  Instantaneous (highest temporal, 2 km
  //  horizontal resolution) interpolated from volume scans onto a 3-D 
  //  Cartesian grid with 1.5 km vertical resolution. The origin and outer 
  //  boundaries of the 3-D Cartesian grid will be specified by the TRMM 
  //  Ground Truth Team. In addition to the 3D gridded reflectivity data, 
  //  this product will include vertical profiles of the mean reflectivity 
  //  and the frequency distribution at each height. The frequency 
  //  distribution will be provided in the form of contoured frequency by 
  //  altitude diagrams (CFADs). Vertical profiles and CFADs will be provided
  //  for the following categories: total, total-land, total-ocean, 
  //  total-convective, total-stratiform, total-anvil, land-convective, 
  //  land-stratiform, land-anvil, ocean-convective, ocean-stratiform, 
  //  ocean-anvil. 
  //  NOTE: Requires TWO sets of product files for workshop; mean profile 
  //  specified by Format 5, and CFADs determined by Format 6. 
  //  Here are the categories of domains over which statistics are calculated.
  //                 01 -> total
  //                 02 -> land
  //                 03 -> ocean
  //                 04 -> convective
  //                 05 -> convective-land
  //                 06 -> convective-ocean
  //                 07 -> stratiform
  //                 08 -> stratiform-land
  //                 09 -> stratiform-ocean
  //                 10 -> no surface rain (anvil)
  //                 11 -> no surface rain-land
  //                 12 -> no surface rain-ocean
  // First write out the mean profile grid.
  {
    char fn[MAX_LINE];
    if (params.ASCIIformat == 1)
      {
	sprintf(fn, "%s/%s.%02d%02d%02d.%02d%02d.vertp", 
		params.outputPath,
		params.siteName,
		params.year,
		params.month,
		params.day,
		params.hour,
		params.minute);
      }
    else
      {
	return(1);
      }

    FILE *fp = fopen(fn, "r");
    if (!fp)
      {
	fprintf(stderr, "Error opening ASCII output file.\n");
	return(0);
      }
    
    if (params.ASCIIformat == 1)
      {
	for (k = 0; k < params.zdim; ++k)
	  {
	    fscanf(fp, "%f ",
		    &prof.mean_Zprof_tot[k]);
	  }
	fscanf(fp, "\n");
	for (k = 0; k < params.zdim; ++k)
	  {
	    fscanf(fp, "%f ",
		    &prof.mean_Zprof_land[k]);
	  }
	
	for (k = 0; k < params.zdim; ++k)
	  {
	    fscanf(fp, "%f ",
		    &prof.mean_Zprof_sea[k]);
	  }
	
	for (k = 0; k < params.zdim; ++k)
	  {
	    fscanf(fp, "%f ",
		    &prof.mean_Zprof_conv[k]);
	  }
	
	for (k = 0; k < params.zdim; ++k)
	  {
	    fscanf(fp, "%f ",
		    &prof.mean_Zprof_conv_land[k]);
	  }
	
	for (k = 0; k < params.zdim; ++k) 
	  {
	    fscanf(fp, "%f ",
		    &prof.mean_Zprof_conv_sea[k]);
	  }
	
	for (k = 0; k < params.zdim; ++k)
	  {
	    fscanf(fp, "%f ",
		    &prof.mean_Zprof_stra[k]);
	  }
	
	for (k = 0; k < params.zdim; ++k)
	  {
	    fscanf(fp, "%f ",
		    &prof.mean_Zprof_stra_land[k]);
	  }
	
	for (k = 0; k < params.zdim; ++k)
	  {
	    fscanf(fp, "%f ",
		    &prof.mean_Zprof_stra_sea[k]);
	  }
	
	for (k = 0; k < params.zdim; ++k)
	  {
	    fscanf(fp, "%f ",
		    &prof.mean_Zprof_anvil[k]);
	  }
	
	for (k = 0; k < params.zdim; ++k)
	  {
	    fscanf(fp, "%f ",
		    &prof.mean_Zprof_anvil_land[k]);
	  }
	
	for (k = 0; k < params.zdim; ++k)
	  {
	    fscanf(fp, "%f ",
		    &prof.mean_Zprof_anvil_sea[k]);
	  }
      }
    else
      {
	return(1);
      }
    fclose(fp);
  }
  
  // Now write out the CFAD grids.  Note that there are 12 files
  // corresponding to CFADS over the various domains.
  {
    if (params.ASCIIformat == 1)
      {
	// Write out the CFAD data here.

	// Category 01: total
	char fn[MAX_LINE];
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		1);
	FILE *fp = fopen(fn, "r");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_IFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fscanf(fp, "%6d", &cfad.count_Zhist_tot[k][l]);
	      }
	    
	  }
	fclose(fp);

	// Category 02: land
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		2);
	fp = fopen(fn, "r");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_IFILE_FAILED);
		exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fscanf(fp, "%6d", &cfad.count_Zhist_land[k][l]);
	      }
	    
	  }
	fclose(fp);

	// Category 03: ocean
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		3);
	fp = fopen(fn, "r");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_IFILE_FAILED);

	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fscanf(fp, "%6d", &cfad.count_Zhist_sea[k][l]);
	      }
	    
	  }
	fclose(fp);

	// Category 04: convective
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		4);
	fp = fopen(fn, "r");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_IFILE_FAILED);

	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fscanf(fp, "%6d", &cfad.count_Zhist_conv[k][l]);
	      }
	    
	  }
	fclose(fp);

	// Category 05: convective - land
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		5);
	fp = fopen(fn, "r");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_IFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fscanf(fp, "%6d", &cfad.count_Zhist_conv_land[k][l]);
	      }
	    
	  }
	fclose(fp);

	// Category 06: convective - ocean\n
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		6);
	fp = fopen(fn, "r");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_IFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fscanf(fp, "%6d", &cfad.count_Zhist_conv_sea[k][l]);
	      }
	    
	  }
	fclose(fp);

	// Category 07: stratiform
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		7);
	fp = fopen(fn, "r");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_IFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fscanf(fp, "%6d", &cfad.count_Zhist_stra[k][l]);
	      }
	    
	  }
	fclose(fp);

	// Category 08: stratiform - land
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		8);
	fp = fopen(fn, "r");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_IFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fscanf(fp, "%6d", &cfad.count_Zhist_stra_land[k][l]);
	      }
	    
	  }
	fclose(fp);

	// Category 09: stratiform - ocean
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		9);
	fp = fopen(fn, "r");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_IFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fscanf(fp, "%6d", &cfad.count_Zhist_stra_sea[k][l]);
	      }
	  }
	fclose(fp);

	// Category 10: anvil
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		10);
	fp = fopen(fn, "r");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_IFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fscanf(fp, "%6d", &cfad.count_Zhist_anvil[k][l]);
	      }
	  }
	fclose(fp);

	// Category 11: anvil - land
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		11);
	fp = fopen(fn, "r");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_IFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l)
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fscanf(fp, "%6d", &cfad.count_Zhist_anvil_land[k][l]);
	      }
	  }
	fclose(fp);

	// Category 12: anvil - ocean
	sprintf(fn, "%s/%s.%s.%s.cfad%02d",
		params.outputPath,
		params.siteName,
		params.dateString,
		params.timeString,
		12);
	fp = fopen(fn, "r");
	if (!fp)
	  {
	    fprintf(stderr, "Error opening CFAD ASCII output file.\n");
		TKreportWarning(W_2A54_OPEN_CFAD_ASCII_IFILE_FAILED);
	    exit(FAILED_CODE1);
	  }
	for (l = 0; l <= cfad.max_Zindex; ++l) 
	  {
	    for (k = 0; k < params.zdim; ++k)
	      {
		fscanf(fp, "%6d", &cfad.count_Zhist_anvil_sea[k][l]);
	      }
	  }
	fclose(fp);
      }
    else
      {
	return(1);
      }
  }
  
  // Write out the range-dependant CFAD information.
  if (params.ASCIIformat == 0)
    {
      return(1);
    }
  
  return(0);
}




