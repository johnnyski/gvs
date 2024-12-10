#ifndef __2A53_H__
#define  __2A53_H__ 1


#include <time.h>

/* Toolkit's definitions */
#include <IO_GV.h>
#include <TKerrHandle.h>
#include "TS_2A53_31.h"

#include "gvs_metadata.h"
#include "zr_table.h"

#define MAX_NROWS P2A53_YDIM
#define MAX_NCOLS P2A53_XDIM
#define MAX_FILENAME_LEN 256

typedef struct {
  int xdim, ydim;
  int mm, dd, yy;
  int hh, min;
  float sec;
  float origin_lat, origin_lon, origin_alt;
  float x[MAX_NROWS][MAX_NCOLS]; /* Dynamic allocate xdim x ydim */
} Cartesian_Z_map;

typedef struct {
  int xdim, ydim;
  float x[MAX_NROWS][MAX_NCOLS]; /* This is nicer for write_rrmap2hdf */
} Cartesian_rainmap;

typedef struct {
  TIME_STR  map_time;
  int xdim, ydim;
  int **ix; /* Dynamic allocate xdim x ydim */
} Raintype_map;

typedef struct {
  int xdim, ydim;     /* Dimension. */
  int xo, yo;         /* Radar origin. */
  float xres, yres;   /* Resolution in km. */
  float **x; /* Dynamic allocate xdim x ydim */
} Pixel_range;

/* Function prototypes. */
Cartesian_Z_map *interpolate_1c51(char *uf_fname, char *sprint_script_fname);
Cartesian_rainmap *initialize_rainmap(int xdim, int ydim, float xval);
Pixel_range *read_range_table(char *file);
Raintype_map *read_raintype(char *file);
void free_cartesian_z_map(Cartesian_Z_map *map);
void write_cart_map(Cartesian_Z_map *map, char *fname);

#endif








