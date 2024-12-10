//--------------------------------------------------------------------------
// Given the 'Volume' object, output it to HDF as specified by
// the TSDIS toolkit (version 2.5) interface.
//
// By: John Merritt
//     Space Applications Corporation
//     Vienna, VA
//     John.Merritt@gsfc.nasa.gov
//
// Initial version: June 9, 1996
// Copyright: 1996
//--------------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>
#include <volume.h>
#include <string.h>

extern "C" {
  void write_grid_to_hdf(
						 float grid3d[][MAX_X_DIM][MAX_Y_DIM], /* 3D refl. */
						 float vprofile[][12], /* Array of V-profiles. */
						 int   cfad[][86][12], /* Array of CFAD's. */
						 int xydim, int zdim,
						 float xyres, float zres,
						 float lat, float lon, /* Lat/lon */
						 float alt,
						 int month, int day, int year,
						 int hour, int minute, int second,
						 char *metadata_fname,
						 int verbose,
						 char *hdffile);
  void *memcpy (void *s1, const void *s2, size_t n);


}

void Volume::output_hdf(char *metadata_fname, int verbose)
{
  // 3D reflectivities are ready to go.
  // Vertical profile is in 12 separate members.
  // Cfads are in 12 different members.
  //
  // For Vertical profiles and CFADS, collect them into one array
  // for loading into the appropriate TOOLKIT structures.  Loading
  // into TSDIS toolkit will be done via a C subroutine.
  //
  char *outfile;
  float vprofile[13][12];
  int the_cfad[13][86][12];      
  int i, j;

  outfile = params.outputHDFfile;

  // Collect the Vertical profile into one array for output.
  for (i=0; i<params.zdim; i++) {
	vprofile[i][0] = prof.mean_Zprof_tot[i];
	vprofile[i][1] = prof.mean_Zprof_land[i];
	vprofile[i][2] = prof.mean_Zprof_sea[i];
	vprofile[i][3] = prof.mean_Zprof_conv[i];
	vprofile[i][4] = prof.mean_Zprof_conv_land[i];
	vprofile[i][5] = prof.mean_Zprof_conv_sea[i];
	vprofile[i][6] = prof.mean_Zprof_stra[i];
	vprofile[i][7] = prof.mean_Zprof_stra_land[i];
	vprofile[i][8] = prof.mean_Zprof_stra_sea[i];
	vprofile[i][9] = prof.mean_Zprof_anvil[i];
	vprofile[i][10]= prof.mean_Zprof_anvil_land[i];
	vprofile[i][11]= prof.mean_Zprof_anvil_sea[i];
  }

  // Collect the CFAD's into one array for output.
  // Things are not quite contiguious.  Copy a 13x86 array from a 17x90.
  for(i=0; i<params.zdim; i++)
	for(j=0; j<86; j++) {
	  the_cfad[i][j][0] = cfad.count_Zhist_tot[i][j];
	  the_cfad[i][j][1] = cfad.count_Zhist_land[i][j];
	  the_cfad[i][j][2] = cfad.count_Zhist_sea[i][j];
	  the_cfad[i][j][3] = cfad.count_Zhist_conv[i][j];
	  the_cfad[i][j][4] = cfad.count_Zhist_conv_land[i][j];
	  the_cfad[i][j][5] = cfad.count_Zhist_conv_sea[i][j];
	  the_cfad[i][j][6] = cfad.count_Zhist_stra[i][j];
	  the_cfad[i][j][7] = cfad.count_Zhist_stra_land[i][j];
	  the_cfad[i][j][8] = cfad.count_Zhist_stra_sea[i][j];
	  the_cfad[i][j][9] = cfad.count_Zhist_anvil[i][j];
	  the_cfad[i][j][10] = cfad.count_Zhist_anvil_land[i][j];
	  the_cfad[i][j][11] = cfad.count_Zhist_anvil_sea[i][j];
	}

  write_grid_to_hdf(grids.reflog,
					vprofile,
					the_cfad,
					params.xydim, params.zdim,
					params.xyres, params.zres,
					params.lat, params.lon, /* Lat/lon */
					params.zmin,
					params.month, params.day, params.year,
					params.hour, params.minute, params.second,
					metadata_fname, verbose,
					outfile);
  
  exit(0);
}
