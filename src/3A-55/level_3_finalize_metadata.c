#include <stdio.h>
/*
 * Finalize the metadata information for Level 3 3A-55 products.
 *
 * Do this by reading a binary metadata file and a series of 
 * command line options (each for a specific metadata item) and
 * output a modified binary metadata file (to stdout).
 *
 * By: John Merritt
 *     Space Applications Corporation
 *     Vienna, VA
 *     NASA/GSFC Code 910.1
 *     John.H.Merritt@gsfc.nasa.gov
 *
 *
 */

#include "gvs_metadata.h"
#include "gv_utils.h"

#include <stdio.h>
#ifdef __sgi
  #include <getopt.h>
#endif

#include <stdlib.h>
#include <string.h>
extern char *optarg;
extern int optind, opterr, optopt;


void usage(char **argv)
{
  fprintf(stderr,"Usage (%s)\n", PROG_VERSION);
  fprintf(stderr, " %s [-a alg_id] [-v alg_version]\n",argv[0]);
  fprintf(stderr, "          [-P product_version] [qa_param_value]\n");
  fprintf(stderr, "          [-R reprocess_status] [-c data_center_source]\n");
  fprintf(stderr, "          [-C contact] [-d data_id]\n");
  fprintf(stderr, "          [-B begin_date] [-b begin_time]\n");
  fprintf(stderr, "          [-E end_date] [-e end_time]\n");
  fprintf(stderr, "          in_metadata.bin\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "Where: (possible value shown)\n");
  fprintf(stderr, "     alg_id             = \"3A55\"\n");
  fprintf(stderr, "     alg_version        = \"1\"\n");
  fprintf(stderr, "     product_version    = \"2\"\n");
  fprintf(stderr, "     qa_param_value     = \"NOT BEING INVESTIGATED\"\n");
  fprintf(stderr, "     reprocess_status   = \"Yes\"\n");
  fprintf(stderr, "     data_center_source = \"TSDIS\"\n");
  fprintf(stderr, "     contact            = \"John H. Merritt NASA/GSFC Code 910.1\"\n");
  fprintf(stderr, "     data_id            = \"3A55\"\n");
  fprintf(stderr, "     begin_date         = \"mm/dd/yyyy\"\n");
  fprintf(stderr, "     begin_time         = \"hh/mm/ss\"\n");
  fprintf(stderr, "     end_date           = \"mm/dd/yyyy\"\n");
  fprintf(stderr, "     end_time           = \"hh/mm/ss\"\n");
}

void process_args(int argc, char **argv, gv_metadata_t *metadata, char **infile)
{
  int c;
  
  while ((c = getopt(argc, argv, "a:v:P:Q:R:c:C:d:B:b:E:e:")) != -1)
	switch (c) {
	case 'a': strcpy(metadata->alg_id     , optarg); break;
	case 'v': strcpy(metadata->alg_version, optarg); break;
	case 'P': metadata->product_version = atoi(optarg); break;
	case 'Q': strcpy(metadata->qa_param_value, optarg); break;
	case 'R': strcpy(metadata->repro_stat, optarg); break;
	case 'c': strcpy(metadata->data_center_src, optarg); break;
	case 'C': strcpy(metadata->contact, optarg); break;
	case 'd': strcpy(metadata->data_id, optarg); break;
	case 'B': date_time_strs2date_and_time(optarg, NULL, &metadata->begin_date, NULL); break;
	case 'b': date_time_strs2date_and_time(NULL, optarg, NULL, &metadata->begin_time); break;
	case 'E': date_time_strs2date_and_time(optarg, NULL, &metadata->end_date, NULL); break;
	case 'e': date_time_strs2date_and_time(NULL, optarg, NULL, &metadata->end_time); break;
	case '?': usage(argv); exit(-1); break;
	default:  break;
	}

/* must have 1 files listed */
  if (argc - optind != 1) {
	usage(argv);
	exit(-1);
  }

/* Can use strdup, if desired */
/* strdup allocates memory */
  *infile = strdup(argv[optind]);
}


int main(int argc, char **argv)
{
  gv_metadata_t metadata;
  char          *infile;
  FILE *fp;

  process_args(argc, argv, &metadata, &infile);

  fp = fopen(infile, "r");
  if (fp == NULL) {
	perror(infile);
	exit(-1);
  }
  fread(&metadata, sizeof(metadata), 1, fp);
  fclose(fp);
#if __sgi
  optind =  1;
#else
  optind = 0;
#endif

  process_args(argc, argv, &metadata, &infile); /* Replace w/ command line. */

  fwrite(&metadata, sizeof(metadata), 1, stdout);

  exit(0);
} 


  
