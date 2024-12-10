#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <fcntl.h>
#include <string.h>
#include <volume.h>

extern "C" {
   #include "exitCodes.h"
}

//   The Program 'analyze_volume.c' does the analysis of a single radar
//   volume scan in terms of:
//    - partitioning of radar echo patterns into convective and stratiform
//      elements based on the horizontal echo variability
//    - 3D echo analysis:  * horizontal echo area statistics
//                         * mean vertical reflectivity profile
//                         * contoured frequency by altitude diagrams (CFAD)
//    - rainfall analysis: * conversion of reflectivity to rainfall intensity
//                         * horizontal map of rainfall intensity
//                         * rainfall intensity at the raingauge sites
//
//   Version:   Matthias Steiner, June 1994
//   Comments:  1) Use the command 'gcc program-name -lm' to compile
//              2) Run: 'analyze_volume input[3 files] output month day time'
//                         * input-1   : MUDRAS file containing radar volume
//                         * input-2   : list of raingauge sites
//                         * input-3   : geographical map (land/ocean mask)
//                         * input-4   : Z-R conversion information
//                         * input-5   : valid data range information
//                         * output-1  : general output file
//                         * output-2  : convective/stratiform map (#2A-54)
//                         * output-3  : mean vertical profiles (#2A-55a)
//                         * output-4  : CFAD histograms (#2A-55b)
//                         * output-5  : CFAD histogram - function of range
//                         * output-6  : rainfall intensity map (#2A-53)
//                         * month     : month of year (01-12)
//                         * day       : day of month (01-31)
//                         * time      : time in HHMM Z format (0000 - 2359)

Volume::Volume(void)
{
  int i = 0;

  for (i = 0; i < NUM_ANAL_INDICES; i++)
    {
      params.anal_ind[i] = 0;
      params.anal_done[i] = 0;
    }

  params.fpout1 = 0;
  grids.validReflog  = 0;
  params.radarFormat = 1;
  params.nVolume     = 0;
  params.ASCIIformat = 1;
  return;
}

Volume::Volume(const char *fn)
{
  int i = 0;

  params.radarFormat = 1;
  params.nVolume     = 0;
  params.ASCIIformat = 0;

  readBinary(fn);

  // Open the general output file.
  if ((params.fpout1 = fopen(params.outputGeneralFn, "w")) == NULL)
    {
      fprintf(stderr, "Sorry, can't open output file %s\n",
	      params.outputGeneralFn);
	  TKreportWarning(W_2A54_OPEN_GEN_OFILE_FAILED);
      exit(FAILED_CODE3);
    }
  
  fprintf(params.fpout1, "RADAR VOLUME SCAN PROCESSING\n");
  fprintf(params.fpout1, "============================\n\n");
  fprintf(params.fpout1, "Input files:\n");
  fprintf(params.fpout1, "------------\n");
  for (i = 1; i <= 5; ++i)
    {
      //fprintf(params.fpout1, "   File #%2d: %s\n", i, argv[i]);
    }
  fprintf(params.fpout1, "\nOutput files:\n");
  fprintf(params.fpout1, "-------------\n");
  for (i = 6; i <= 11; ++i)
      {
	//fprintf(params.fpout1, "   File #%2d: %s\n", i, argv[i]);
      }
  fprintf(params.fpout1, "\n");
  
  return;
}

Volume::Volume(int argc, char **argv)
{
  int i = 0;

  params.radarFormat = 1;
  params.nVolume     = 0;
  params.ASCIIformat = 0;

  // Check for correct number of arguments of the job command line.
  if (argc != 20)
    {
      fprintf(stderr,
	      "usage: %s [input[5]]\n"
	      "       [output[3]]\n"
	      "       [month] [day] [time] [results path]\n"
	      "       [site name] [ASCII format] [radar format]\n",
	      argv[0]);
	  TKreportWarning(W_2A54_BAD_USAGE2);
      exit(FAILED_CODE3);
    }

  // Set the input filenames.
  strcpy(params.inputVolumeFn,         argv[1]);
  strcpy(params.inputRainGaugeSitesFn, argv[2]);
  strcpy(params.inputGeoMapFn,         argv[3]);
  strcpy(params.inputZRTranFn,         argv[4]);
  strcpy(params.inputValidRangeFn,     argv[5]);

  // Set the output filenames.
  strcpy(params.outputGeneralFn,      argv[6]);
  strcpy(params.outputConvStraMapFn,  argv[7]);
  strcpy(params.outputMeanVertProfFn, argv[8]);
  strcpy(params.outputCFADFn,         argv[9]);
  strcpy(params.outputCFADRangeFn,    argv[10]);
  strcpy(params.outputRainIntFn,      argv[11]);

  // Set some parameters based on command line arguments.
  params.year   = atoi(argv[12]);;
  params.month  = atoi(argv[13]);
  params.day    = atoi(argv[14]);
  params.hour   = atoi(argv[15]) / 100;
  params.minute = atoi(argv[15]) % 100;

  strcpy(params.outputPath, argv[16]);
  strcpy(params.siteName, argv[17]);
  params.ASCIIformat = atoi(argv[18]);
  params.radarFormat = atoi(argv[19]);

  // Create the date and time strings.
  sprintf(params.dateString, "%02d%02d%02d", params.year,
	  params.month, params.day);
  sprintf(params.timeString, "%02d%02d", params.hour, params.minute);
  
  // Open the general output file.
  if ((params.fpout1 = fopen(params.outputGeneralFn, "w")) == NULL)
    {
      fprintf(stderr, "Sorry, can't open output file %s\n",
	      params.outputGeneralFn);
	  TKreportWarning(W_2A54_OPEN_GEN_OFILE_FAILED);
      exit(FAILED_CODE3);
    }
  
  fprintf(params.fpout1, "RADAR VOLUME SCAN PROCESSING\n");
  fprintf(params.fpout1, "============================\n\n");
  fprintf(params.fpout1, "Input files:\n");
  fprintf(params.fpout1, "------------\n");
  for (i = 1; i <= 5; ++i)
    {
      fprintf(params.fpout1, "   File #%2d: %s\n", i, argv[i]);
    }
  fprintf(params.fpout1, "\nOutput files:\n");
  fprintf(params.fpout1, "-------------\n");
  for (i = 6; i <= 11; ++i)
      {
	fprintf(params.fpout1, "   File #%2d: %s\n", i, argv[i]);
      }
  fprintf(params.fpout1, "\n");
  
  // Read input parameters needed to the analysis of the radar volume:
  fprintf(stdout, "Now reading parameters.\n");
  int test = READparam();
  

  // Read the radar volume scan data from the input file (MUDRAS Format).
  readRadarVolume(0);

  // Copy some dimension information.
  params.xydim = mudfile.header.X.N;
  params.xyres = mudfile.header.X.Spacing / 1000.0;
  params.zdim = mudfile.header.Z.N;
  params.zres = mudfile.header.Z.Spacing / 1000.0;

  // Read the coordinates of the raingauge sites.
  fprintf(stdout, "Now reading raingauge network data.\n");
  test = READgauge();
  
  // Read the geographical map (same resolution as radar data).
  fprintf(stdout, "Now reading geographical mask.\n");
  test = READgeogr();
  return;
}

Volume::~Volume(void)
{
  // Close the main output file.
  if (params.fpout1) fclose(params.fpout1);

  return;
}
