/* 
 * write_rrmap2hdf.c - Writes 2D rain rate map to HDF file.
 *   
 *
 * By:    Thuy Nguyen
 *        NASA/TRMM Office
 *        Thuy.Nguyen@gsfc.nasa.gov
 *        May 10, 1996
 */
#include <stdio.h>
#include <string.h>
#include <unistd.h>

/* TSDIS toolkit include files*/
#include "IO.h"
#include "IO_GV.h"


/* hdf' grid */
#define HDF_MAX_NROWS 151
#define HDF_MAX_NCOLS 151

void write_rrmap2hdf(float rrmap[155][155], int hour, int min, int sec, 
					 char *lat_str, char *lon_str, char *hdf_filename)
{
  /* write rain rate map to file in hdf format */
  L2A_53_SINGLE_RADARGRID hdf_grid;
  IO_HANDLE               hdf_fd;
  int r, c;
  int                     dataType;
  char                    filemode;
  int                     status;

  if (hdf_filename == NULL || strlen(hdf_filename) == 0) {
	fprintf(stderr, "Error: Output filename is not specified\n");
	exit(-1);
  }

  dataType = TK_L2A_53S;
  filemode = TK_NEW_FILE;
  status = TKopen(hdf_filename, dataType, filemode, &hdf_fd);
  
  /* Check the Error Status */
  if (status != TK_SUCCESS) {
	/* handle error processing here */
	fprintf(stderr, "TKopen failed\n");
	exit(-1);
  }

  /* fill rain rate map to hdf's grid before writing to file */
  memset(&hdf_grid, '\0', sizeof(L2A_53_SINGLE_RADARGRID));
  for (r = 1; r <= HDF_MAX_NROWS; r++) {
	for (c = 1; c <= HDF_MAX_NCOLS; c++) {
      /* grid's index starts at 0 */
	  hdf_grid.rainRate[r-1][c-1] =  rrmap[r][c];
	}
  }

  /* fill in time */
  hdf_grid.tktime.tkhour = hour;
  hdf_grid.tktime.tkminute = min;
  hdf_grid.tktime.tksecond = sec;

  status = TKwriteGrid(&hdf_fd, &hdf_grid);
  /* Check the Error Status */
  if (status != TK_SUCCESS) {
	/* handle error processing here */
	fprintf(stderr, "TKwriteGrid failed\n");
	exit(-1);

  }
  if (TKclose(&hdf_fd) == TK_FAIL) {
	fprintf(stderr, "TKclose failed\n");
	exit(-1);
  }

} /* write_rrmap2hdf */
 
