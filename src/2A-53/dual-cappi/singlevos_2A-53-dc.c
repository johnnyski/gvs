#include <stdio.h>
#include <math.h>
#include "2A53.h"

#include <rsl.h>
#include "gvs_metadata.h"

#include <gv_utils.h>

#define MAX_FILENAME_LEN 256
#define MAX_XDIM         151
#define MAX_YDIM         151
#define MAX_SMALL_STR_LEN 30
#define MAX_STR_LEN      MAX_FILENAME_LEN * 3 + 100

#define THRESHOLD_RADIUS 50
#define INNER_GRID_CAPPI 0
#define OUTER_GRID_CAPPI 1

/*
 * Dual-cappi 2A-53 Algorithm
 *   Modified from version 4 2A-53 algorithm.
 * 
 * Input is 2A-55 hdf, 2A-54 hdf, zr table.
 * No longer uses 1C-51 as input.  No call to sprint.
 *
 * 2A-53 uses the first cappi (1.5km) from 2A-55 for the inner disk
 * and the second cappi (3 km) for the outer disk.
 * The outer disk is every pixel beyond THRESHOLD_RADIUS 
 * pixels from the center.
 *
 * By:
 *  Galen J. Wilkerson
 *  Science Systems and Applications, Inc.
 *  Lanham, MD
 *  Galen.Wilkerson@gsfc.nasa.gov
 *  6/28/2000
 ****************************************************
 * Version 4 2A-53 algorithm.
 *
 * 1. Derived from 2A-53 pseudocode (28 October 1996 version), by Sandy Yuter.
 * 
 * By:
 *   John H. Merritt
 *   Applied Research Corp.
 *   Landover MD
 *   John.Merritt@gsfc.nasa.gov
 *   3/1/97
 *-----------------------------------------------------------------------------
 *
 * There are 4 major components to the rain rate algorithm:
 *
 * 1. Polar to cartesean using input section to 2A-54 processing: sprint.
 * 2. Create rainmap type map.
 * 3. Initialize rainmap.
 * 4. Find rain rate from ZR and range table.
 *
 * To compute the ZR, use PMM:
 * a. Similiar procedure as in rain rate algorithm above.
 * b. Input is 1C51, gauge location table, 2A54.
 * c. Input # categories.
 * d. Use # categories to detrermine type of algorithm.
 * e. Match w/ gauge data
 * f. Construct PMM table.
 */

int verbose = 0;

void usage(int argc, char **argv)
{
  fprintf(stderr, "Usage: %s (Version %s)\n", argv[0], PROG_VERSION);
  fprintf(stderr, "                [-v] [-m metadata_file]\n");
  fprintf(stderr, "                2A-55 2A-54 ZR-table Out.hdf\n\n");

  fprintf(stderr, "Where:\n");
  fprintf(stderr, "       -v             Verbose diagnostics during execution.\n");
  fprintf(stderr, "       -m             Specify filename for metadata file. \n");
  fprintf(stderr, "                      Default:$GVS_DATA_PATH/2A53_metadata.txt\n");
  fprintf(stderr, "       2A-55          Cartesian Reflectivity Data (single VOS 2A-55) in hdf\n");
  fprintf(stderr, "       2A-54          Cartesian_CS_map (single VOS 2A-54) in hdf\n");
  fprintf(stderr, "       ZR-table       Table(s) provided by site PIs corresponding to\n");
  fprintf(stderr, "                      precipitation types and different ranges.\n");
  fprintf(stderr, "       Out.hdf        Cartesian rainmap 2A-53 in HDF\n");
  TKreportWarning(W_2A53_BAD_USAGE);
  exit(-1);
}

#ifdef __sgi
  #include <getopt.h>
#endif

#include <stdlib.h>
#include <string.h>

void process_args(int argc, char **argv,
				  char **in_2A55, char **in_2a54, 
				  char **in_zr_table,
				  char **out_file, int *verbose,
				  char **metadata_fname)
{
  extern int optind;
  extern char *optarg;
  extern int optopt;

  int c;

  while ((c = getopt(argc, argv, "m:v")) != -1)
	switch (c) {
	case 'v': *verbose = 1; 
	  break;
	case 'm':
	  *metadata_fname= (char *) strdup(optarg); 
	  break;

	case '?': usage(argc, argv); break;
	default:  break;
	}

  /* must have 4 files listed */
  if (argc - optind != 4) {
	fprintf(stderr, "Need at least 4 arguments.\n");
	usage(argc, argv);
  }
  *in_2A55        = (char *) strdup(argv[optind]);
  *in_2a54        = (char *) strdup(argv[optind+1]);
  *in_zr_table    = (char *) strdup(argv[optind+2]);
  *out_file       = (char *) strdup(argv[optind+3]);
}

/***********************************************************************/
/*                                                                     */
/*                      initialize_rainmap                             */
/*                                                                     */
/***********************************************************************/
Cartesian_rainmap *initialize_rainmap(int xdim, int ydim, float xval)
{
  int x, y;
  Cartesian_rainmap *r;

  /* Allocate space. */
  r = (Cartesian_rainmap *) calloc(1, sizeof(Cartesian_rainmap));

  r->xdim = xdim;
  r->ydim = ydim;
  for (x=0; x<xdim; x++) {
	for (y=0; y<ydim; y++)
	  r->x[x][y] = xval;
  }
  return r;
}
/***********************************************************************/
/*                                                                     */
/*                      compute_range_table                            */
/*                                                                     */
/***********************************************************************/
Pixel_range *compute_range_table(int xdim, int ydim,
								 float xres, float yres,
								 int x0, int y0
								 )
{
  Pixel_range *p;
  int i, x, y, xd, yd;
  float r;

  /*
   * Input: xdim, ydim    -- Dimension.  x = longitude, y = latitude.
   *        xres, yres    -- Resolution.
   *        x0, y0        -- Origin of radar.
   *
   * Output: 2d array ranges.
   */

  p = (Pixel_range *)calloc(1, sizeof(Pixel_range));
  if (p == NULL) {
	perror("Allocated Pixel_range");
	return p;
  }

  p->xdim = xdim;
  p->ydim = ydim;
  p->xres = xres;
  p->yres = yres;
  p->xo = x0;
  p->yo = y0;

  p->x = (float**)calloc(xdim, sizeof(float *));
  for (i=0; i<xdim; i++)
	p->x[i] = (float*)calloc(ydim, sizeof(float));

  /* Now compute all ranges. */
  for (x=0; x<xdim; x++){
	xd = (x - x0)*p->xres;
	for (y=0; y<ydim; y++) {
	  yd = (y - y0)*p->yres;
	  r = (float)sqrt((double)xd*xd + (double)yd*yd);
	  p->x[x][y] = r;
	}
  }

  return p;
}

/***********************************************************************/
/*                                                                     */
/*                      print_range_table                              */
/*                                                                     */
/***********************************************************************/
void print_range_table(Pixel_range *p)
{
  int x, y;

  printf("%d\n", p->xdim);
  printf("%d\n", p->ydim);
  printf("%f\n", p->xres);
  printf("%f\n", p->yres);
  printf("%d\n", p->xo);
  printf("%d\n", p->yo);
  for (x=0; x<p->xdim; x++){
	for (y=0; y<p->ydim; y++) {
	  printf(" %f", p->x[x][y]);
	}
	printf("\n");
  }
}
  
  
/***********************************************************************/
/*                                                                     */
/*                           print_Z                                   */
/*                                                                     */
/***********************************************************************/
void print_Z(Cartesian_Z_map *z)
{
  int x, y;
  unsigned char c;
  float val;
  
  printf("P5\n");
  printf("%d\n", z->xdim);
  printf("%d\n", z->ydim);
  printf("255\n");
  for (x=0; x<z->xdim; x++){
	for (y=0; y<z->ydim; y++) {
	  val = z->x[x][y];
	  if (val < 0) val = 0;
	  c = val;
	  printf("%c", c);
	}
  }
}
/***********************************************************************/
/*                                                                     */
/*                           print_2a54                                */
/*                                                                     */
/***********************************************************************/
void print_2a54(Raintype_map *z)
{
  int x, y;
  unsigned char c;
  float val;

  printf("P5\n");
  printf("%d\n", z->xdim);
  printf("%d\n", z->ydim);
  printf("255\n");
  for (x=0; x<z->xdim; x++){
	for (y=0; y<z->ydim; y++) {
	  val = z->ix[x][y];
	  if (val < 0) val = 0;
	  c = val;
	  printf("%c", c);
	}
  }
}

/***********************************************************************/
/*                                                                     */
/*                     read_data_from_hdf_file                         */
/*                                                                     */
/***********************************************************************/
/* TSDIS toolkit include files*/
#include <IO.h>
#include <IO_GV.h>

/* Shamelessly stolen from gvs/format_conversion/2A-54_hdf2ascii.c */
int read_data_from_hdf_file(char *hdffile, L2A_54_SINGLE_RADARGRID *grid)
{
  int status;
  IO_HANDLE hdf_fd;

  if (grid == NULL || hdffile == NULL) return -1;

  memset(&hdf_fd, '\0', sizeof(IO_HANDLE));
  /* open hdf file */
  status = TKopen(hdffile, TK_L2A_54S, TK_READ_ONLY, &hdf_fd);
  
  /* Check the Error Status */
  if (status != TK_SUCCESS) {
	fprintf(stderr, "TKopen failed on file <%s>\n", hdffile);
	return (-1);
  }

  /* read grid from file */
  if (TKreadGrid(&hdf_fd, grid) != TK_SUCCESS) {
	fprintf(stderr, "TKreadGrid failed from file <%s>.\n", hdffile);
	return (-1);
  }	

  /* close file */
  if (TKclose(&hdf_fd) == TK_FAIL) {
	fprintf(stderr, "Can't close <%s>\n", hdffile);
	return(-1);  
  }
  return 1;
}


/***********************************************************************/
/*                                                                     */
/*                      read_raintype                                  */
/*                                                                     */
/***********************************************************************/
Raintype_map *read_raintype(char *file)
{
  /*
   * Read 2A54 product.
   */
  Raintype_map *rtype;
  L2A_54_SINGLE_RADARGRID l2A54Grid;
  int r, c;

  memset(&l2A54Grid, '\0', sizeof(L2A_54_SINGLE_RADARGRID));
  if (read_data_from_hdf_file(file, &l2A54Grid) < 0) {
	fprintf(stderr, "Failed to read data from file <%s>\n", file);
	TKreportWarning(W_2A53_2A54_READ_FAIL);
	exit(-1);
  }

  
  rtype = (Raintype_map *)calloc(1, sizeof(Raintype_map));
  rtype->ix = (int **)calloc(MAX_NROWS, sizeof(int *));
  rtype->xdim = MAX_NROWS;
  rtype->ydim = MAX_NCOLS;
  /* access grid */
  for (r = 0; r < MAX_NROWS; r++) {
	rtype->ix[r] = (int *)calloc(MAX_NCOLS, sizeof(int));
	for (c = 0; c < MAX_NCOLS; c++)
	  rtype->ix[r][c] = l2A54Grid.convStratFlag[r][c];
  }
  return rtype;
}





/***********************************************************************/
/*                                                                     */
/*                 read_data_from_2A_55_hdf_file                       */
/*                                                                     */
/***********************************************************************/
/* TSDIS toolkit include files*/

#include <IO.h>
#include <IO_GV.h>

/* Shamelessly stolen from read_date_from_hdf_file */
int read_data_from_2A55_hdf_file(char *hdffile, L2A_55_SINGLE_RADARGRID *grid)
{
  int status;
  IO_HANDLE hdf_fd;

  if (grid == NULL || hdffile == NULL) return -1;

  memset(&hdf_fd, '\0', sizeof(IO_HANDLE));
  /* open hdf file */
  status = TKopen(hdffile, TK_L2A_55S, TK_READ_ONLY, &hdf_fd);
  
  /* Check the Error Status */
  if (status != TK_SUCCESS) {
	fprintf(stderr, "TKopen failed on file <%s>\n", hdffile);
	return (-1);
  }

  /* read grid from file */
  if (TKreadGrid(&hdf_fd, grid) != TK_SUCCESS) {
	fprintf(stderr, "TKreadGrid failed from file <%s>.\n", hdffile);
	return (-1);
  }	

  /* close file */
  if (TKclose(&hdf_fd) == TK_FAIL) {
	fprintf(stderr, "Can't close <%s>\n", hdffile);
	return(-1);  
  }
  return 1;
}


/***********************************************************************/
/*                                                                     */
/*                 get_info_from_hdf_file                              */
/*                                                                     */
/***********************************************************************/
int get_info_from_hdf_file(char *hdf_fname, char *site_name,
						  int *mm, int *dd, int *yy
						  )
{
  int status;
  DATE_STR date;
  int data_type = TK_L2A_55S;
  IO_HANDLE fh;

  memset(&fh, '\0', sizeof(IO_HANDLE));
  
  status = TKopen(hdf_fname, data_type, TK_READ_ONLY, &fh);
  if (status != TK_SUCCESS) {
    TKclose(&fh);
    return -1;
  }

  TKreadMetadataChar(&fh, TK_RADAR_NAME, site_name);

  TKreadMetadataInt(&fh, TK_END_DATE, &date);

  *mm = date.tkmonth;
  *dd = date.tkday;
  *yy = date.tkyear;

  TKclose(&fh);

  return 1;
} /*get_info_from_hdf_file */

/***********************************************************************/
/*                                                                     */
/*                    read_gridded_reflectivity                        */
/*                                                                     */
/***********************************************************************/
/* TSDIS toolkit include files*/

#include <IO.h>

/* 
 *Read the gridded reflectivity -
 *      The inner circle - radius 100 km is from 2A-55 cappi at 1.5 km
 *      the outer circle - 100 to 150 km is from 2A-55 cappi at 3 km
 */
Cartesian_Z_map *read_gridded_reflectivity(char *l2A_55_filename) {

  Cartesian_Z_map *czmap;
  L2A_55_SINGLE_RADARGRID l2A55Grid;
  TIME_STR tktime;

  char site_name[MAX_SMALL_STR_LEN];
  int mm, dd, yy;
  int8 HH, MM, SS;
  float lat, lon;
  int i, j;
  
  if (l2A_55_filename == NULL) return (NULL);

  memset(&l2A55Grid, '\0', sizeof(L2A_55_SINGLE_RADARGRID));

 if (verbose)
   fprintf(stderr, "Reading metadata from 2A55 file...\n");
  
 /*
    Get data type and date from hdf file.
  */

  memset(site_name, '\0', MAX_SMALL_STR_LEN);

  if (get_info_from_hdf_file(l2A_55_filename, site_name,
							&mm,&dd,&yy)  < 0) {
    fprintf(stderr, "Couldn't get info_from_hdf_file\n");

    return NULL;
  }

  if (verbose) {
    fprintf(stderr,"filename: %s\n",l2A_55_filename);
    fprintf(stderr,"sitename: %s\n",site_name);
    fprintf(stderr,"month: %d day: %d year: %d\n",mm,dd,yy);
    
    fprintf(stderr,"Reading grid from 2A55 file... \n");
    fprintf(stderr,"%s\n",l2A_55_filename);
  }

  if (read_data_from_2A55_hdf_file(l2A_55_filename,&l2A55Grid) < 0) 
    {
    fprintf(stderr, "Failed to read data from file <%s>\n", l2A_55_filename);

    return NULL;
  }

  /* get the time from the radar grid */  
  tktime = l2A55Grid.tktime;
  HH = tktime.tkhour;
  MM = tktime.tkminute;
  SS = tktime.tksecond;

  if (verbose) {
    fprintf(stderr,"hour: %d, min: %d, sec: %d\n",HH,MM,SS);
    fprintf(stderr,"Reading lat/lon... \n");
  }

  /* Read lat/lon from $GVS_DATA_PATH/gv_radar_site_info.data */

  if (gv_utils_get_grid_origin_lat_lon(site_name, GV_UTILS_SINGLE_RADAR, &lat, &lon) < 0) {
    
    fprintf(stderr, "Failed to read lat/lon from file with site = <%s>\n", site_name);
    TKreportWarning(W_2A53_INVALID_SITE);
    
    return NULL;
  }

  czmap = (Cartesian_Z_map *) calloc (1,sizeof(Cartesian_Z_map));

  /*
   *  grab the values out of l2A55Grid
   */
  czmap->mm   = mm;
  czmap->dd   = dd;
  czmap->yy   = yy;
  czmap->hh   = (int) HH;
  czmap->min  = (int) MM;
  czmap->sec  = (float) SS;

  czmap->origin_lat = lat;
  czmap->origin_lon = lon;

  /* copy the reflectivities */
  czmap->xdim = MAX_XDIM;
  czmap->ydim = MAX_YDIM;
  
  /* 
   * use cappi 0 for the inner THRESHOLD_RADIUS km
   * use cappi 1 for the outer THRESHOLD_RADIUS to 300 km
   */

  if(verbose) {
    fprintf(stderr, "center: %d , thresh: %d\n", MAX_XDIM/2, THRESHOLD_RADIUS);
    fprintf(stderr, "inner CAPPI: %d, outer CAPPI: %d \n", INNER_GRID_CAPPI, OUTER_GRID_CAPPI);
  }

  for (i = 0; i < MAX_XDIM; i++) {
    for ( j = 0; j < MAX_YDIM; j++) 

      if ((pow(i - MAX_XDIM/2, 2) + pow(j - MAX_YDIM/2, 2)) < THRESHOLD_RADIUS * THRESHOLD_RADIUS)
	czmap->x[i][j]  = l2A55Grid.threeDreflect[INNER_GRID_CAPPI][i][j];
      else
	czmap->x[i][j]  = l2A55Grid.threeDreflect[OUTER_GRID_CAPPI][i][j];
  }
 
  return czmap;
}


/***********************************************************************/
/*                                                                     */
/*                      set_single_raintype                            */
/*                                                                     */
/***********************************************************************/
Raintype_map *set_single_raintype(void)
{
  /*
   * Uniform: For single ZR.
   */
  Raintype_map *rtype;
  int r, c;

  rtype = (Raintype_map *)calloc(1, sizeof(Raintype_map));
  rtype->ix = (int **)calloc(MAX_NROWS, sizeof(int *));
  rtype->xdim = MAX_NROWS;
  rtype->ydim = MAX_NCOLS;
  /* access grid */
  for (r = 0; r < MAX_NROWS; r++) {
	/* All 1's is a good rain-type index, because, the lookup
     * in applyZR subtracts 1. */
	rtype->ix[r] = (int *)calloc(MAX_NCOLS, sizeof(int));
	for (c = 0; c < MAX_NCOLS; c++)
	  rtype->ix[r][c] = 1;
  }
  return rtype;
}


#include <signal.h>
void handler(int sig)
{
  fprintf(stderr, "Got signal %d. Abort.\n", sig);
  kill(0, sig);
  if (sig == SIGINT || sig == SIGKILL || sig == SIGSTOP) 
	exit (-2);
  exit(-1);
}

void free_cartesian_z_map(Cartesian_Z_map *map)
{
  if (map == NULL) return;
  free(map);
}


/***********************************************************************/
/*                                                                     */
/*                        m  a  i  n                                   */
/*                                                                     */
/***********************************************************************/
int main(int argc, char **argv)
{
  char *in_1c51, *in_2a54, *in_zr_table;
  char *in_2A55;

  char *out_hdf;
  char tmpname[MAX_FILENAME_LEN];
  Cartesian_Z_map   *cartesian_Z_working_map;
  Cartesian_rainmap *cartesian_rainmap;
  Raintype_map      *raintype_map;
  Zr_table          *zr_table;
  Pixel_range       *pixel_range;
  int               rain_type;
  enum RAIN_classification    rclass;
  char              *sprint_script_fname = NULL;
  int i, j;
  char              *metadata_fname = NULL;
  
  /* Diag strings.  Keep these in sync w/ RAIN_classification. */
  /* Rain classes always have the missing and noecho classes, therefore
   * a single means 3 rain classes (missing, noecho, single).  And, 
   * a dual means 4 rain classes (missing, noecho, strat, conve)
   */
  char *class_str[6] = {"noclass",
						"missing", "noecho",
						"single", "dual", "multi"};  
  char *path = NULL;

  signal(SIGINT, handler);
  signal(SIGFPE, handler);
  signal(SIGKILL, handler);
  signal(SIGILL, handler);
  signal(SIGSTOP, handler);
  signal(SIGSEGV, handler);

  if (verbose)
    fprintf(stderr,"begin singlevos_2A-53-v4\n");


  process_args(argc, argv, &in_2A55, &in_2a54, &in_zr_table,
			   &out_hdf,
			   &verbose, 
			   &metadata_fname); 

   /* Set default */
  if (metadata_fname == NULL) {
	path = getenv("GVS_DATA_PATH");
	if (path == NULL)
	  path = "/usr/local/trmm/GVBOX/data";
	memset(tmpname, '\0', MAX_FILENAME_LEN);
	sprintf(tmpname, "%s/2A53_metadata.txt", path);
	metadata_fname = strdup(tmpname);
  }
  zr_table = read_ZR(in_zr_table);  /* Retrieves the ZR table. */

  if (zr_table == NULL) {
	fprintf(stderr, "Unable to read ZR file <%s>\n", in_zr_table);
	TKreportWarning(W_2A53_ZR_FORMAT);
	exit(-1);
  }

  if (zr_table->nrtype < 3) {
	fprintf(stderr, "ZR table requires at least 3 classes.\n");
    fprintf(stderr, "Classes MISSING, NOECHO are required classes\n");
	TKreportWarning(W_2A53_ZR_FORMAT);
	exit(-1);
  }

  if (zr_table->nrtype == 3) rclass = SINGLE;
  if (zr_table->nrtype == 4) rclass = DUAL;
  if (zr_table->nrtype >= 5) rclass = MULTI;

  if (verbose) fprintf(stderr, "ZR table rain classification method: %s\n",
					   class_str[rclass]);

  /*
    instead of re - interpolating 1C-51, read a horizontal slice from 2A-55
   */

  cartesian_Z_working_map = read_gridded_reflectivity(in_2A55); 

  if (cartesian_Z_working_map == NULL) {
	fprintf(stderr, "No Z working map\n");
	TKreportWarning(W_2A53_NO_Z);
	exit(-1);
  }
  /*  print_Z(cartesian_Z_working_map); */

  /* Initialize the rainmap to all missing values. */
  cartesian_rainmap = initialize_rainmap(cartesian_Z_working_map->xdim,
										 cartesian_Z_working_map->ydim,
										 0.0);
  
  /* It may be faster to compute the range table, due to fast CPU's and
   * it is by far, easier to handle internally than rely on the operator
   * to specify the correct file on the command line.  Additional
   * parameters needed are the location of the radar in the grid.
   * For now, I'll assume it is located at the center of the 151x151
   * grid; the '75, 75' parameters.
   */
  pixel_range = compute_range_table(P2A53_XDIM, P2A53_YDIM, P2A53_XRES, 
									P2A53_YRES, 75, 75);
  /*  print_range_table(pixel_range); */


  /* Create the raintypes map. Currently, only dual and single supported. */
  if (rclass == MULTI) {
    raintype_map = read_raintype(in_1c51);
  } else if (rclass == DUAL) {
    raintype_map = read_raintype(in_2a54);
  } else if (rclass == SINGLE) {
    raintype_map = set_single_raintype();
  }
  /*print_2a54(raintype_map); */
  
  for (i=0; i<cartesian_rainmap->xdim; i++) {
	for (j=0; j<cartesian_rainmap->ydim; j++) {
	  rain_type = raintype_map->ix[i][j];
	  /* Rain types that come from raintype_map are:
       *
       *    MISSING    == -99
       *    NOECHO     == 0
       *    STRATIFORM == 1
       *    CONVECTIVE == 2
       *
       * These values are present in the ZR table.  All conversion
       * of DBZ to rainmap values is done via the ZR table.  The
       * routine 'applyZRtable' does this.  By convension, the 
       * first two rain classes are MISSING and NOECHO.  The next
       * two are STRATIFORM and CONVECTIVE.  This means that a
       * dual ZR table is really composed of 4 classes: MISSING, NOECHO,
       * STRATIFORM and CONVECTIVE.
       *
       * Therefore, rain_type == 0 is MISSING, rain_type == 1 is NOECHO,
	   * rain_type == 2 is STRATIFORM and rain_type == 3 is CONVECTIVE.
       * (Only by convension)
       *
       * Range dependency may exist.  For instance, < 15 km is MISSING,
       * > 150 is MISSING, > 100 is MISSING (as of 8/10/89).  But, this
       * is all defined in the ZR table and not a concern here.
       */

	  /* Reassign ordinal to rain_type for ZR table lookup. */

	  if (rain_type == MISSING) {
		rain_type = 0;
	  } else if (rain_type == 0) { /* For some reason the execution of this
									* loop overwrites the symbolic variable
									* NOECHO.  So, here I test against its
									* value explicitly; why is this?  Bug?
									*/
		rain_type = 1;
      } else {
		rain_type += 1;  /* Stratiform and Convective */
	  }
	  
	  cartesian_rainmap->x[i][j] = applyZRtable(rain_type, 
												pixel_range->x[i][j],
												cartesian_Z_working_map->x[i][j],
												zr_table);
	}
  }

  /*
   * Output rainmap to HDF
   */
  if (verbose) fprintf(stderr, "output hdf <%s>\n", out_hdf);
  if (write_rrmap2hdf(cartesian_rainmap->x, 
				  cartesian_Z_working_map->mm,
				  cartesian_Z_working_map->dd,
				  cartesian_Z_working_map->yy,
				  cartesian_Z_working_map->hh,
				  cartesian_Z_working_map->min,
				  (int)(cartesian_Z_working_map->sec+0.5),
				  pixel_range->xdim, pixel_range->ydim,
				  pixel_range->xres, pixel_range->yres,
			      cartesian_Z_working_map->origin_lat, 
				  cartesian_Z_working_map->origin_lon,
				  cartesian_Z_working_map->origin_alt,
				  metadata_fname,
					  verbose, 1, out_hdf) < 0) {
	TKreportWarning(W_2A53_WRITE_METADATA_FAIL);
	exit(-1);
  }
  
  if (verbose) fprintf(stderr, "output hdf <%s> ... done\n", out_hdf);
  if (cartesian_Z_working_map)
	free_cartesian_z_map(cartesian_Z_working_map);

  exit(0);
}
