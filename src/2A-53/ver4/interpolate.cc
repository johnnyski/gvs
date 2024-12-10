/* interpolate.cc
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <math.h>
#include <errno.h>

#include "mudras.h"


extern "C" 
{
  #include "2A53.h"
  #include <gv_utils.h>
  #include <rsl.h>
  extern int verbose;
}


#define MAX_FILENAME_LEN 256
#define MAX_XDIM         151
#define MAX_YDIM         151
#define MAX_SMALL_STR_LEN 30
#define MAX_STR_LEN      MAX_FILENAME_LEN * 3 + 100


//#define MISSING_VAL -99.0
#define MISSING_VAL MISSING
#define MAX_ARGC         12
extern int system_call(char *cmd);
extern int uf2mudras(char *uf_fname, 
			  char *sprint_script_fname, 
			  char *site_name,
			  char *date_str,
			  char *mudras_fname);

int get_info_from_uf_file(char *uf_fname, char *site_name, char *date_str,
						  int *mm, int *dd, int *yy,
						  int *HH, int *MM, float *SS);

Cartesian_Z_map *interpolate_1c51(char *uf_fname, 
								   char *sprint_script_fname)
{
  /* Inputs:
   *  uf_fname: filename of 1C-51 UF file.
   *  sprint_script_fname: filename of input script to 'sprint'. Specify NULL 
   *                       to use default ($GVS_DATA_PATH/sprint_base_scan.inp)
   * Output:
   *  It returns a pointer to a cartesian Z map if successful; NULL, otherwise.
   *  Caller should deallocate the cartesian Z map.
   *
   * Interpolate lowest level scan from 1C-51 to create
   * Cartesian_Z_working_map using SPRINT.  Same algo. as input to 2A-54.
   *
   * Note: Some of the code here were pulled directly from '2A54.cc'.
   *
   */
  char date_str[MAX_SMALL_STR_LEN];
  char *mudras_fname = NULL;
  MudFile m;
  Cartesian_Z_map *cart_map = NULL;
  char *path;
  char sscript_fname[MAX_FILENAME_LEN];
  FILE *fp;
  char site_name[MAX_SMALL_STR_LEN];
  int mm, dd, yy;
  int HH, MM;
  float SS;
  float lat, lon;

  if (uf_fname == NULL) return (NULL);

  //Get data type and date from uf file.
  memset(site_name, '\0', MAX_SMALL_STR_LEN);
  memset(date_str, '\0', MAX_SMALL_STR_LEN);
  if (get_info_from_uf_file(uf_fname, site_name, date_str,
							&mm,&dd,&yy,
							&HH,&MM,&SS)  < 0) {
	fprintf(stderr, "Couldn't get info_from_uf_file\n");
	TKreportWarning(W_2A53_UF_INFO_FAILED);
	return NULL;
  }

  if (sprint_script_fname == NULL) {
	/* Use default: $GVS_DATA_PATH/sprint_base_scan.inp */
	path = getenv("GVS_DATA_PATH");
	if (path == NULL) 
	  path = "/usr/local/trmm/GVBOX/data";
	memset(sscript_fname, '\0', MAX_FILENAME_LEN);
	sprintf(sscript_fname, "%s/sprint_base_scan.inp", path);
	sprint_script_fname = sscript_fname;
  }

  /* Create a tmp file name */
  if ((mudras_fname = tempnam("/tmp/", "2a53")) == NULL) {
	if ((mudras_fname = (char *) calloc(MAX_FILENAME_LEN, sizeof(char))) == NULL) {
	  perror("calloc mudras fname");
	  TKreportWarning(W_2A53_MEM_ALLOC_FAILED);
	  return (NULL);
	}
	sprintf(mudras_fname, "/tmp/2a53.%d", getpid());
  }

  if (verbose) 
	fprintf(stderr, "Start to convert uf to mudras...\n");
  /* Convert UF to mudras file */
  if (uf2mudras(uf_fname, sprint_script_fname, site_name, date_str, 
	mudras_fname) < 0) {
	fprintf(stderr, "Failed creating a mudras file for %s\n", uf_fname);
	if (mudras_fname) free(mudras_fname);
	TKreportWarning(W_2A53_FAILED_MUDRAS);
	return (NULL);
  }

  /* Open mudras file */
  if ((fp = fopen(mudras_fname, "r")) == NULL) {
	perror("fopen mudras_fname");
	perror(mudras_fname);
	goto DONE;
  }

  if ((cart_map = (Cartesian_Z_map *) calloc(1, sizeof(Cartesian_Z_map))) == NULL) {
	perror("calloc Cartesian_Z_map");
	goto DONE;
  }

  if (verbose) 
	fprintf(stderr, "Reading mudras file ...\n");

  m.readPureCedricVolume(fp);
  if (verbose) 
	fprintf(stderr, "Filling cartesian map ...\n");
  m.getField(0, (float *)cart_map->x, MAX_XDIM, MAX_YDIM, 1, (float) MISSING_VAL);

  // Read lat/lon from $GVS_DATA_PATH/gv_radar_site_info.data
  if (gv_utils_get_grid_origin_lat_lon(site_name, GV_UTILS_SINGLE_RADAR, &lat, &lon) < 0) {
	fprintf(stderr, "Failed to read lat/lon from file with site = <%s>\n", site_name);
	TKreportWarning(W_2A53_INVALID_SITE);
	free_cartesian_z_map(cart_map);
	cart_map = NULL;
	goto DONE;
  }
  cart_map->xdim = MAX_XDIM;
  cart_map->ydim = MAX_YDIM;
  cart_map->mm   = mm;
  cart_map->dd   = dd;
  cart_map->yy   = yy;
  cart_map->hh   = HH;
  cart_map->min  = MM;
  cart_map->sec  = SS;
  cart_map->origin_lat = lat;
  cart_map->origin_lon = lon;


DONE:
  if (fp)
	fclose(fp);
  remove(mudras_fname);
  if (mudras_fname) free(mudras_fname);
  return (cart_map);

}  /* interpolate_1c51 */

int get_info_from_uf_file(char *uf_fname, char *site_name, char *date_str,
						  int *mm, int *dd, int *yy,
						  int *HH, int *MM, float *SS)

{
  /* Get site name, date, time from the uf file. 
   * Return 1 upon successful; -1, otherwise.
   */
  Radar *radar;
  int rc = 1;

  if (site_name == NULL || date_str == NULL) return -1;
  if (verbose) RSL_radar_verbose_on();
  radar = RSL_anyformat_to_radar(uf_fname);
  if (radar == NULL) {
	fprintf(stderr, "Unable to read %s to radar structure.\n", uf_fname);
	return -1;
  }
  strcpy(site_name, radar->h.radar_name);
  sprintf(date_str, "%.2d%.2d%.2d", radar->h.year % 100, radar->h.month, radar->h.day);
DONE:
  *mm = radar->h.month;
  *dd = radar->h.day;
  *yy = radar->h.year;
  *HH = radar->h.hour;
  *MM = radar->h.minute;
  *SS = radar->h.sec;

  if (radar) RSL_free_radar(radar);
  return rc;
} /*get_info_from_uf_file */

int uf2mudras(char *uf_fname, 
			  char *sprint_script_fname, 
			  char *site_name,
			  char *date_str,
			  char *mudras_fname)
{
  /* Convert UF file to MUDRAS file using the given sprint's script
   * Inputs:
   *  uf_fname: filename of 1C-51 UF file.
   *  sprint_script_fname: filename of input script to 'sprint'. 
   *  site_name:           radar name.
   *  date_str:            YYMMDD of the UF file.  It can be NULL if 
   *                       YYMMDD is contained in uf_fname.
   *  mudras_fname:        filename for mudras file.
   *
   * Output:
   * Returns 1 upon successful; otherwise, -1.
   */
  char cmd[MAX_STR_LEN];
  char sprint_options[MAX_STR_LEN];
  if (uf_fname == NULL || sprint_script_fname == NULL ||
	  mudras_fname == NULL) return (-1);

  memset(cmd, '\0', MAX_STR_LEN);
  memset(sprint_options, '\0', MAX_STR_LEN);
  if (date_str != NULL) {
	strcat(sprint_options, "-d ");
	strcat(sprint_options, date_str);
  }
  if (verbose) 
	strcat(sprint_options, " -v");

  if (site_name != NULL) {
	strcat(sprint_options, " -S ");
	strcat(sprint_options, site_name);
  }
  if (sprint_script_fname != NULL) {
	strcat(sprint_options, " -s ");
	strcat(sprint_options, sprint_script_fname);
  }

  sprintf(cmd, "uf2mudras %s %s %s", sprint_options, uf_fname, mudras_fname);

  /* Don't want to call system() because it doesnot return the exit code
   * of the program.
   */
  if (verbose) 
	fprintf(stderr, "Executing %s...\n", cmd);

  if (system_call(cmd) > 0) 
	return (1);

  return (-1);
} /* uf2mudras */

int system_call(char *cmd)
{
  /* Returns 1 upon successful; otherwise, -1.  Exit with -2 if prog received 
   * interrupt signal.
   *
   * Don't want to call system() because it doesnot return the exit code 
   * of the program. Use execv() instead.
   */
  char *cmd_argvs[MAX_ARGC];
  int   stat;
  char *tok;
  char *prog;
  int i;
  pid_t chpid;
  /*
  extern int errno;
  */

  tok = strtok(cmd, " ");
  if (tok == NULL) return (-1);
  prog = tok;
  i = 0;
  cmd_argvs[i] = tok;
  i++;
  while ((tok = strtok(NULL, " ")) != NULL) {
	cmd_argvs[i] = tok;
	i++;
  }
  cmd_argvs[i] = NULL;

  /*
  for (j=0;j<i;j++) {
	if (cmd_argvs[j]) fprintf(stderr, "%s \n", cmd_argvs[j]);
  }
  */
  if ((chpid = fork()) == 0) {
	/* child */

	stat = execvp(prog, cmd_argvs);
	if (verbose)
	  fprintf(stderr, "Errno = %d, stat: %d\n", errno, stat);
	TKreportWarning(W_2A53_SYSTEM_FAILED);
	exit(stat);

  }

  /* parent */
  waitpid (chpid, &stat, 0); /* Wait until child is finished. **/
  if (stat != 0) {
	if (stat == -1) {
	  fprintf(stderr,"Failed executing %s. Stat: %d\n", prog, stat);
	  TKreportWarning(W_2A53_CHILD_FAILED);
	  return -1;
	}
	else if (stat == -2) {
	  fprintf(stderr,"Aborted executing %s. Stat: %d\n", prog, stat);
	  /* E_2A53_CHILD_ABORTED */
	  TKreportWarning(W_2A53_CHILD_ABORTED);
	  exit(-2);
	}
  }
  return (1);
} /* system_call */


void free_cartesian_z_map(Cartesian_Z_map *map)
{
  if (map == NULL) return;
  free(map);
}


void write_cart_map(Cartesian_Z_map *map, char *fname)
{
  /* Write the map to file or stdout if fname is NULL */
  int x,y;
  float *X;
  FILE *fp;

  if (map == NULL) return;

  if (fname == NULL) fp = stdout;

  if ((fp = fopen(fname, "w")) == NULL) fp = stdout;
  
  fprintf(stderr, "X,Y = %d, %d\n", map->xdim, map->ydim);
  X = (float *) map->x;
  for (y = map->ydim-1; y >=0; y--) {
	for (x = 0; x < map->xdim; x++) {

	  fprintf(fp, "%.2f ", X[x + map->xdim * y]);
	}
	fprintf(fp, "\n");
  }
  if (fp != stdout) fclose(fp);
}

