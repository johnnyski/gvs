/*
 * By: John H. Merritt
 *     Space Applications Corporation
 *     Vienna, VA
 *     John.Merritt@gsfc.nasa.gov
 *     Copyright 1996
 */

#include <stdio.h>
/* TSDIS toolkit include files*/
#include <IO.h>
#include <IO_GV.h>
#include <exitCodes.h>
#include <gvs_metadata.h>
#include "2A55.h"

/* From volume.h */
#define MAX_X_DIM       160               /* maximum x-array dimension */
#define MAX_Y_DIM       160               /* maximum y-array dimension */
#define MAX_Z_DIM        17               /* maximum z-array dimension */

void write_grid_to_hdf(
					   float grid3d[][MAX_Y_DIM][MAX_X_DIM], /* 3D refl. */
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
					   char *hdffile)
{
  int i, j, k;
  L2A_55_SINGLE_RADARGRID hdf_product;
  IO_HANDLE               hdf_fd;
  int                     dataType;
  char                    filemode;
  int                     status;
  gv_metadata_t           metadata;
  DATE_STR                tkdate;

  if (verbose) {
	printf("Output file is: %s\n", hdffile);
	printf("xydim %d, zdim %d\n", xydim, zdim);
	printf("xyres %d, zres %d\n", (int) xyres, (int) zres);
	printf("mm/dd/yy %2.2d/%2.2d/%2.2d, hh:mm:ss %2.2d:%2.2d:%2.2d\n",
		   month, day, year,
		   hour, minute, second);
	printf("lat/lon: %f/%f\n", lat, lon);
  }

  memset(&metadata, '\0', sizeof(gv_metadata_t));
  dataType = TK_L2A_55S;
  filemode = TK_NEW_FILE;

  status = TKopen(hdffile, dataType, filemode, &hdf_fd);
  
  if (status != TK_SUCCESS) {
	fprintf(stderr, "TKopen failed for file <<%s>>.\n", hdffile);
	TKreportWarning(W_2A55_OPEN_OUTFILE_FAILED);
	exit(FAILED_CODE4);
  }
  memset(&hdf_product, 0, sizeof(hdf_product));

  hdf_product.tktime.tkhour = hour;
  hdf_product.tktime.tkminute = minute;
  hdf_product.tktime.tksecond = second;
  tkdate.tkday = day;
  tkdate.tkmonth = month;
  tkdate.tkyear = year;

  /* Set metadata: store grid info for only 3D reflectivities. 
   * The grid info for the other grids will be set when the new toolkit
   * contained the fields for them exits.
   */
  if (set_l2_specific_metadata(&tkdate, &(hdf_product.tktime), xydim, xydim, zdim, 
							   xyres, xyres, zres, lat, lon, alt, 
							   metadata_fname,
							   P2A55_MIN_UNIT, P2A55_MAX_UNIT,    /* Range of dBZ values. */
							   P2A55_ALG_ID, P2A55_ALG_VERSION, 
							   P2A55_SOFTWARE_VERSION,
							   P2A55_CONTACT_NAME, P2A55_DATA_ID, 
							   &metadata) < 0) {
	if (verbose)
	  fprintf(stderr, "Failed to set l2 specific metadata.\n");
	TKreportWarning(W_2A55_WRITE_METADATA_FAILED);
	exit(FAILED_CODE4);
  }
  if (write_hdf_metadata_for_l2vos(&hdf_fd, &metadata, verbose) < 0) {
	if (verbose)
	  fprintf(stderr, "Failed to write metadata.\n");
	TKreportWarning(W_2A55_WRITE_METADATA_FAILED);
	exit(FAILED_CODE4);
  }
  /* Fill all members of the hdf product; 3D reflectivities, vertical profile, cfad. */

#ifdef VERBOSE
  for (k = 0; k < zdim; ++k) {
	sprintf(out, "slice%2.2d.dat", k);
	of = fopen(out,"w");
	for (j = 0; j < xydim; ++j) {
	  for (i = 0; i < xydim; ++i) {
		fprintf(of, " %.2f", grid3d[k][j][i]);
	  }
	  fprintf(of, "\n");
	}
	fclose(of);
  }
  
#endif
  /* 3D reflectivities */

  for (k = 0; k < zdim; ++k)
	for (j = 0; j < xydim; ++j)
	  for (i = 0; i < xydim; ++i) {
		hdf_product.threeDreflect[k][j][i] = grid3d[k][j][i];
		if (grid3d[k][j][i] == -99.0 ||
			grid3d[k][j][i] < -15.0 ||
			grid3d[k][j][i] > 70.0)
		  /* Convert to Toolkit's badvalue */
		  hdf_product.threeDreflect[k][j][i] = TK_DEFAULT;
	  }

  /* Vertical profile
   */
  for (k = 0; k < zdim; ++k)
	for (j = 0; j < 12; ++j) {
	  hdf_product.vertProfile[k][j] = vprofile[k][j];
	  if (vprofile[k][j] == -99.0)
		/* Convert to Toolkit's badvalue */
		hdf_product.vertProfile[k][j] = TK_DEFAULT;
	}

  /* CFAD
   */
  for (k = 0; k < zdim; ++k)
	for (i = 0; i < 86; ++i)
	  for (j = 0; j < 12; ++j) {
		hdf_product.cfadData[k][i][j] = cfad[k][i][j];
	  }

  /* WRITE the hdf product. */
  status = TKwriteGrid(&hdf_fd, &hdf_product);
  if (status != TK_SUCCESS) {
	fprintf(stderr, "TKwriteGrid failed.  Current file is <<%s>>.\n", hdffile);
	TKreportWarning(W_2A55_WRITE_OUTFILE_FAILED);
	exit(FAILED_CODE4);
  }
  if (TKclose(&hdf_fd) == TK_FAIL) {
	fprintf(stderr, "TKclose failed.  Current file is <<%s>>.\n", hdffile);
	TKreportWarning(W_2A55_CLOSE_OUTFILE_FAILED);
	exit(FAILED_CODE4);
  }

} 

