/* 
 * write_csmap2hdf.cc - Writes 2D convective/stratiform map to HDF file.
 *   
 *
 * By:    Thuy Nguyen
 *        NASA/TRMM Office
 *        Thuy.Nguyen@gsfc.nasa.gov
 *        May 13, 1996
 */
#include <stdio.h>
#include <string.h>
#include <unistd.h>


/* TSDIS toolkit include files*/
#include <IO.h>
#include <IO_GV.h>

#include "gvs_metadata.h"
#include "2A54.h"

/* hdf' grid */
#define HDF_MAX_NROWS 151
#define HDF_MAX_NCOLS 151


int write_csmap2hdf(int csmap[151][151], int year, int month, int day,
					int hour, int min, int sec,
					int xdim, int ydim, 
					float xres, float yres,
					float lat, float lon, float alt,
					char *metadata_fname,
					char *hdf_filename, int verbose)
{

  /* Write csmap to file in hdf format. Return 1 upon successful; -1, 
   * otherwise 
   */


  L2A_54_SINGLE_RADARGRID hdf_grid;
  IO_HANDLE               hdf_fd;
  int r, c;
  int                     dataType;
  char                    filemode;
  int                     status;
  gv_metadata_t           metadata;
  DATE_STR                tkdate;



  if (hdf_filename == NULL || strlen(hdf_filename) == 0) {
	fprintf(stderr, "Error: Output filename is not specified\n");
	return -1;
  }

  memset(&metadata, '\0', sizeof(gv_metadata_t));
  dataType = TK_L2A_54S;
  filemode = TK_NEW_FILE;
  memset(&hdf_fd, '\0', sizeof(IO_HANDLE));
  

  if (verbose) 
	fprintf(stderr, "Calling TKopen <%s>...\n", hdf_filename);
  status = TKopen(hdf_filename, dataType, filemode, &hdf_fd);

  /* Check the Error Status */
  if (status != TK_SUCCESS) {
	/* handle error processing here */
	fprintf(stderr, "TKopen failed\n");
	return -1;
  }
  /* fill rain rate map to hdf's grid before writing to file */
  memset(&hdf_grid, '\0', sizeof(L2A_54_SINGLE_RADARGRID));


  for (r = 0; r < HDF_MAX_NROWS; r++) {
	for (c = 0; c < HDF_MAX_NCOLS; c++) {
	  hdf_grid.convStratFlag[r][c] =  csmap[r][c];
	}
  }

  /* fill in time */
  hdf_grid.tktime.tkhour = hour;
  hdf_grid.tktime.tkminute = min;
  hdf_grid.tktime.tksecond = sec;

  tkdate.tkday = day;
  tkdate.tkmonth = month;
  tkdate.tkyear = year;

  if (set_l2_specific_metadata(&tkdate, &(hdf_grid.tktime), xdim, ydim,
							   P2A54_ZDIM,
							   xres, yres, P2A54_ZRES , lat, lon, alt,  
							   metadata_fname, P2A54_MIN_UNIT, P2A54_MAX_UNIT,
							   P2A54_ALG_ID, P2A54_ALG_VERSION,
							   P2A54_SOFTWARE_VERSION,
							   P2A54_CONTACT_NAME, P2A54_DATA_ID, 
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

  if (verbose) 
	fprintf(stderr, "Calling TKwriteGrid...\n");
  status = TKwriteGrid(&hdf_fd, &hdf_grid);
  
  /* Check the Error Status */
  if (status != TK_SUCCESS) {
	/* handle error processing here */
	fprintf(stderr, "TKwriteGrid failed\n");
	return -1;

  }

  if (verbose) 
	fprintf(stderr, "Calling TKclose...\n");

  if (TKclose(&hdf_fd) == TK_FAIL) {
	fprintf(stderr, "TKclose failed\n");
	return -1;
  }

  return 1;
} /* write_csmap2hdf */

