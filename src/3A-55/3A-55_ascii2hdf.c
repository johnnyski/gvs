#include <stdio.h>
#include <string.h>
/*
 * By: John Merritt
 *     Space Applications Corporation
 *     Vienna, VA
 *     John.H.Merritt@gsfc.nasa.gov
 */

#include <gv_utils.h>
/* TSDIS toolkit include files*/
/* setenv TSDISTK /usr/local/toolkit */
#include "IO_GV.h"
#include <TKerrHandle.h>
#include <TS_3A55_43.h>
#include "gvs_metadata.h"
#include "3A55.h"

/* From volume.h */
#define MAX_X_DIM       160               /* maximum x-array dimension */
#define MAX_Y_DIM       160               /* maximum y-array dimension */
#define MAX_Z_DIM        17               /* maximum z-array dimension */

/*
	typedef struct
	{
	  int16				vertProfile_scale[13][12];
	  float32			vertProfile[13][12];
	  int32				cfadData[13][86][12];
	} L3A_55_RADARGRID;
	*/
int write_hdf_metadata_for_l3granule(IO_HANDLE *hdf_fd,
									 gv_metadata_t *metadata,
									 int verbose);

void read_metadata(char *filename, char *hdf_outfile, gv_metadata_t *metadata)
{
  FILE *fp;

  fp = fopen(filename, "r");
  if (fp == NULL) {
	perror(filename);
	return;
  }

  fread(metadata, sizeof(gv_metadata_t), 1, fp);

  /* Override any default w/ these. */
  strcpy(metadata->data_id, P3A55_DATA_ID);
  strcpy(metadata->contact, P3A55_CONTACT_NAME);
  strcpy(metadata->alg_id, P3A55_ALG_ID);             /* This prog. */
  strcpy(metadata->alg_version, P3A55_ALG_VERSION);   /* This prog. */
  /*
  strcpy(metadata->software_version, P3A55_SOFTWARE_VERSION);
  */
  /* This prog. */
  strcpy(metadata->anomaly_flag, "NOT EMPTY");
  if (strrchr(hdf_outfile, '/') != NULL)
	strcpy(metadata->granule_id, strrchr(hdf_outfile, '/') + 1);
  else
	strcpy(metadata->granule_id, hdf_outfile);
  fclose(fp);
}

int main(int argc, char **argv)
{
  int i, j, k;
  L3A_55_RADARGRID        hdf_product;
  IO_HANDLE               hdf_fd;
  int                     dataType;
  char                    filemode;
  int                     status;
  char *outhdf;
  char *in_cfad;
  char *in_vprofile;
  char *in_metadata;
  int zdim=13;
  FILE *fp;
  gv_metadata_t           metadata;
  int verbose = 0;
  int create_empty_granule = 0;

  if (argc != 5) {
	fprintf(stderr,"Usage (%s)\n", PROG_VERSION);
	fprintf(stderr, "  %s in_cfad.ascii in_vprofile.ascii in_metadata.bin out_3A-55.hdf\n", argv[0]);
	fprintf(stderr,"\n  where:\n");
	fprintf(stderr,"     in_cfad.ascii is a concatination of all 12 cfads.\n");
	fprintf(stderr,"     in_vprofile.ascii is the vertical profile file.\n");
	fprintf(stderr,"     in_metadata.bin contains all metadata in binary form.\n");
	fprintf(stderr, "    Note: This program will create empty granule product if the specified cfad file does not exist.\n");
	TKreportWarning(W_3A55_BAD_USAGE);
	exit(-1);
  }

  in_cfad     = strdup(argv[1]);
  in_vprofile = strdup(argv[2]);
  in_metadata = strdup(argv[3]);
  outhdf      = strdup(argv[4]);

  dataType = TK_L3A_55;
  filemode = TK_NEW_FILE;

  status = TKopen(outhdf, dataType, filemode, &hdf_fd);
  
  if (status != TK_SUCCESS) {
	fprintf(stderr, "TKopen failed for file <<%s>>.\n", outhdf);
	TKreportWarning(W_3A55_TKOPEN_FAIL);
	exit(-1);
  }

  memset(&metadata, '\0', sizeof(gv_metadata_t));
  read_metadata(in_metadata, outhdf, &metadata);

  if (!file_exist(in_cfad)) {
	create_empty_granule = 1;
	strcpy(metadata.anomaly_flag, "EMPTY: REASON UNKNOWN");
  }

  if (write_hdf_metadata_for_l3granule(&hdf_fd, &metadata, verbose) < 0) {
	if (verbose)
	  fprintf(stderr, "Failed to write metadata. HDF file is <<%s>>.\n", outhdf);
	TKreportWarning(W_3A55_WRITE_METADATA_FAIL);
	exit(-1);
  }
  /* Fill all members of the hdf product; 3D reflectivities, vertical profile, cfad. */


  if (create_empty_granule)
	goto CLOSE_HDF;

  /* CFAD */
  fp = fopen(in_cfad,"r");
  if (fp == NULL) {
	TKreportWarning(W_3A55_CFAD_OPEN_FAIL);
	exit(-1);
  }
  for (j = 0; j < 12; ++j) {
	for (i = 0; i < 86; ++i) {
	  for (k = 0; k < zdim; ++k)
		fscanf(fp, "%6d", &hdf_product.cfadData[k][i][j]);
	  fscanf(fp,"\n");
	}
  }
  fclose(fp);

  /* Vertical Profile */
  fp = fopen(in_vprofile,"r");
  if (fp == NULL) {
	TKreportWarning(W_3A55_VPROF_OPEN_FAIL);
	exit(-1);
  }
  for (j = 0; j < 12; ++j) {
	for (k = 0; k < zdim; ++k) {
	  fscanf(fp, " %f", &hdf_product.vertProfile[k][j]);
	  if (hdf_product.vertProfile[k][j] <= -99.0) {
		hdf_product.vertProfile[k][j] = TK_DEFAULT;
	  }
	}
	fscanf(fp,"\n");
  }
  fclose(fp);

  /* Now output to HDF */

  status = TKwriteGrid(&hdf_fd, &hdf_product);
  if (status != TK_SUCCESS) {
	fprintf(stderr, "TKwriteGrid failed.  HDF file is <<%s>>.\n", outhdf);
	TKreportWarning(W_3A55_TKWRITEGRID_FAILED);
	exit(-1);
  }

  CLOSE_HDF:
  if (TKclose(&hdf_fd) == TK_FAIL) {
	fprintf(stderr, "TKclose failed.  HDF file is <<%s>>.\n", outhdf);
	TKreportWarning(W_3A55_TKCLOSE_FAILED);
	exit(-1);
  }

  exit(0);
} 


  
