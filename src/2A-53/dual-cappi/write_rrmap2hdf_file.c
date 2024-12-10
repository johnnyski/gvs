/* 
 * write_rrmap2hdf_file.c - Writes 2D rain rate map to HDF file.
 *   
 *
 * By:    Thuy Nguyen
 *        NASA/TRMM Office
 *        International Database Systems
 *        Thuy.Nguyen@gsfc.nasa.gov
 *        May 10, 1996
 */
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <stdlib.h>
#include <IO_GV.h>
 
#include "gvs_metadata.h"
#include "2A53.h"

/* hdf' grid */
#define HDF_MAX_NROWS MAX_NROWS
#define HDF_MAX_NCOLS MAX_NCOLS


int write_rrmap2hdf(float rrmap[MAX_NROWS][MAX_NCOLS], 
					 int mm, int dd, int yy,
					 int hour, int min, int sec, 
					 int xdim, int ydim,
					 float xres, float yres,
					 float lat, float lon, float alt,
					 char *metadata_fname,
					 int verbose, int write_metadata,
					 char *hdf_filename)
{
  /* write rain rate map to file in hdf format. Set metadata fields for
   * 2A-53 if write_metadata is 1.
   * Return 1 for successful; -1, otherwise.
   */
  L2A_53_SINGLE_RADARGRID hdf_grid;
  IO_HANDLE               hdf_fd;
  int r, c;
  float                   rate;
  int                     dataType;
  char                    filemode;
  int                     status;
  gv_metadata_t           metadata;
  DATE_STR                vos_date;

  if (hdf_filename == NULL || strlen(hdf_filename) == 0) {
	fprintf(stderr, "Error: Output filename is not specified\n");
	return(-1);
  }

  memset(&metadata, '\0', sizeof(gv_metadata_t));
  dataType = TK_L2A_53S;
  filemode = TK_NEW_FILE;
  status = TKopen(hdf_filename, dataType, filemode, &hdf_fd);
  
  /* Check the Error Status */
  if (status != TK_SUCCESS) {
	/* handle error processing here */
	fprintf(stderr, "TKopen failed\n");
	return(-1);
  }

  memset(&hdf_grid, '\0', sizeof(L2A_53_SINGLE_RADARGRID));

  hdf_grid.tktime.tkhour = (int8)hour;
  hdf_grid.tktime.tkminute = (int8)min;
  hdf_grid.tktime.tksecond = (int8)sec;

  if (write_metadata) {
	/* Yes, write metadata fields */
	if (verbose) fprintf(stderr, "Getting metadata...\n");
	memset(&vos_date, '\0', sizeof(DATE_STR));
	vos_date.tkyear = (int16)yy;
	vos_date.tkmonth = (int16)mm;
	vos_date.tkday = (int16)dd;


	/* Fill all pertinent metadata items, 2A53 specific, into 'metadata'. */
	if (set_l2_specific_metadata(&vos_date, &(hdf_grid.tktime), xdim, ydim, 0, 
								 xres, yres, P2A53_ZRES, lat, lon, alt,
								 metadata_fname,
								 P2A53_MIN_UNIT, P2A53_MAX_UNIT,   /* Rain rates are in percentage */
								 P2A53_ALG_ID, P2A53_ALG_VERSION,
								 P2A53_SOFTWARE_VERSION,
								 P2A53_CONTACT_NAME, P2A53_DATA_ID, 
								 &metadata) < 0) {
	  if (verbose) 
		fprintf(stderr, "Failed to set l2 specific metadata.\n");
	  return(-1);
	}

	if (write_hdf_metadata_for_l2vos(&hdf_fd, &metadata, verbose) < 0) {
	  if (verbose)
		fprintf(stderr, "Failed to write metadata.\n");
	  return(-1);
	}
  }

  if (verbose) 
	fprintf(stderr, "Filling rain rate map \n");

  /* fill rain rate map to hdf's grid before writing to file */

  for (r = 0; r < HDF_MAX_NROWS; r++) {
	for (c = 0; c < HDF_MAX_NCOLS; c++) {
	  rate = rrmap[r][c];
	  if (rate == MISSING)
		rate = TK_DEFAULT; /* Convert to the toolkit's bad value */
	  hdf_grid.rainRate[r][c] =  rate;
	}
  }

  if (verbose) 
	fprintf(stderr, "Filling radar grid str\n");

  if (verbose) 
	fprintf(stderr, "Writing grid to HDF file.\n");

  status = TKwriteGrid(&hdf_fd, &hdf_grid);
  
  /* Check the Error Status */
  if (status != TK_SUCCESS) {
	/* handle error processing here */
	fprintf(stderr, "TKwriteGrid failed\n");
	return(-1);

  }

  if (TKclose(&hdf_fd) == TK_FAIL) {
	fprintf(stderr, "TKclose failed\n");
	return(-1);
  }
  
  return 1;
} /* write_rrmap2hdf */
 


