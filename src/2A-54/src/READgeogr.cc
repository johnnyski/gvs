//   The function 'READgeogr.c' reads the geographical map (land/ocean mask)
//   and - if necessary - reformats the map the radar data grid resolution.
//
//   Version: Matthias Steiner, June 1994
//
#include <stdio.h>
#include <math.h>
#include <volume.h>
#include <string.h>

extern "C" {
   #include "exitCodes.h"
}

int Volume::READgeogrNew(void)
{
  int i = 0;
  int j = 0;
  FILE *fp = fopen(params.inputGeoMapFn, "r");
  if (!fp)
    {
      fprintf(stderr, "Sorry, can't open input file %s\n",
	      params.inputGeoMapFn);
      return(1);
    }

  int xdim, ydim;
  fread(&xdim, sizeof(xdim), 1, fp);
  fread(&ydim, sizeof(ydim), 1, fp);

  // Check on dimensions of geographical mask.
  if ((xdim != params.xydim) || (ydim != params.xydim))
    {
      fprintf(stderr, "Geographical mask dimensions are not correct!\n");
	  TKreportWarning(W_2A54_INVALID_GEOG_MASK_DIM);
      exit( FAILED_CODE1);
    }    

  {
    for (j = 0; j < params.xydim; j++)
      {
	for (i = 0; i < params.xydim; i++)
	  {
	    fread(&grids.geomap[j][i], sizeof(int), 1, fp);
	  }
      }
  }

  return(0);
}

int Volume::READgeogr(void)
{
  FILE *fpin3;                          // input-3 file
  int i, j, ii, jj;                     // loop indices
  int test;                             // test variable
  int index;                            // land (1) / ocean (0) index
  int ind_land;                         // counts pixels over land
  int ind_sea;                          // counts pixels over ocean
  int map_1km[MAP_GRID][MAP_GRID];      // high resolution land/ocean mask

  // Try to read a temporary binary geographical map file.
  {
    char fn[256];
    sprintf(fn, "/tmp/geogmap.bin.%s", params.siteName);
    FILE *fp = fopen(fn, "r");
    if (fp)
      {
	// Check the resolution.
	float xres, yres;
	char sn[MAX_LINE];
	fread(sn, sizeof(sn), 1, fp);
	fread(&xres, sizeof(float), 1, fp);
	fread(&yres, sizeof(float), 1, fp);
	if ((xres == params.xyres) && (yres == params.xyres) &&
	    (strcmp(sn, params.siteName) == 0))
	  {
	    fread(grids.geomap, sizeof(grids.geomap), 1, fp);
	    fclose(fp);
	    return(0);
	  }

	// If we get here then the map resolution or site was wrong so we need
	// to re-read the ASCII map.  Close the binary file we just opened.
	fclose(fp);
      }
  }

  // We have to do it the hard way.
  // Open the input-3 file (= geographical map):
  if ((fpin3 = fopen(params.inputGeoMapFn, "r")) == NULL)
    {
      fprintf(stderr, "Sorry, can't open input file %s\n",
	      params.inputGeoMapFn);
	  TKreportWarning(W_2A54_CANT_OPEN_GEOG_MAP);
      exit( FAILED_CODE2);
    }
  
  //  Read the high resolution (1 km) geographical map:
  for (j = 0; j < MAP_GRID; ++j)
    {
      for (i = 0; i < MAP_GRID; ++i)
	{
	  test = fscanf(fpin3, "%d %d %d", &jj, &ii, &index);
	  if (test != 3)
	    {
	      fprintf(stderr, "Problem with geographical map file!\n");
		  TKreportWarning(W_2A54_INVALID_GEOG_MAP_FORMAT);
	      exit( FAILED_CODE3);
            }
	  map_1km[jj][ii] = index;
        }
    }
  
  // Remapping of the geographical land/ocean mask:
  for (j = 0; j < params.xydim; ++j)
    {
      for (i = 0; i < params.xydim; ++i)
	{
	  ind_land = 0;
	  ind_sea = 0;
	  for (jj = int(params.xyres*j - 1); jj <= params.xyres*j + 1; ++jj)
	    {
	      if ((jj >= 0) && (jj < MAP_GRID))
		{
		  for (ii = int(params.xyres*i - 1);
		       ii <= params.xyres*i + 1; ++ii)
		    {
		      if ((ii >= 0) && (ii < MAP_GRID))
			{
			  if (map_1km[jj][ii] == 0)
			    {
			      ++ind_sea;
                            }
			  if (map_1km[jj][ii] == 1)
			    {
			      ++ind_land;
			    }
                        }
                    }
                }
            }
	  if ((ind_land == 0) && (ind_sea == 0))
	    {
	      fprintf(stderr,
		      "Something is wrong with the geographical mask!\n");
		  TKreportWarning(W_2A54_GEOG_MASK_WRONG);
	      exit( FAILED_CODE3);
            }
	  else
	    {
	      if (ind_land >= ind_sea)
		{
		  grids.geomap[j][i] = 1;
                }
	      else
		{
		  grids.geomap[j][i] = 0;
                }
            }
        }
    }
  
  // Create a temporary binary geographical map file.
  {
    char fn[256];
    sprintf(fn, "/tmp/geogmap.bin.%s", params.siteName);
    FILE *fp = fopen(fn, "w");
    if (fp)
      {
	fwrite(params.siteName, sizeof(params.siteName), 1, fp);
	fwrite(&params.xyres, sizeof(float), 1, fp);
	fwrite(&params.xyres, sizeof(float), 1, fp);
	fwrite(grids.geomap, sizeof(grids.geomap), 1, fp);
	fclose(fp);
      }
    else
      {
	fprintf(stderr, "Error creating geographical map file.\n");
      }
  }

  return(0);
}
