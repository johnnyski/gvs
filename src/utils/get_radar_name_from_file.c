/*
 *
 *  get_radar_name_from_file:
 *      Read the radar name from any data file (recognized by the RSL lib)
 *      or 1C51 HDF file and echo it to STDOUT. Non-hdf file may be compressed.
 *
 * By:  Thuy Nguyen
 *      Science Systems and Applications, Inc. (SSAI)
 *      NASA/TRMM Office
 *      nguyen@trmm.gsfc.nasa.gov
 *      August 1, 1997.
 */ 


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef __sgi
  #include <getopt.h>
#endif
#include <signal.h>
#include <IO.h>
#include <IO_GV.h>
#include <rsl.h>


typedef enum { HDF, NON_HDF} file_type_t;
#define MAX_NAME_LEN    30

static char *this_prog;
static int verbose = 0;
static int radar_verbose_flag = 0;

void get_radar_name_from_HDF(char *infile, char *radar_name);
void get_radar_name_from_NON_HDF(char *infile, char *callid_first_file, 
							char *radar_name);
void handler(int sig);
void process_argvs(int argc, char **argv, 
				  char **infile, char **callid_first_file, file_type_t *file_type);

int main (int argc, char **argv)
{
  char *infile = NULL;
  file_type_t file_type;
  char *callid_first_file = NULL;
  char radar_name[MAX_NAME_LEN];

  this_prog = argv[0];

  /* Set signals' handler */
  signal(SIGINT, handler);
  signal(SIGFPE, handler);
  signal(SIGKILL, handler);
  signal(SIGSTOP, handler);
  signal(SIGILL, handler);
  signal(SIGSEGV, handler);

  process_argvs(argc, argv, &infile, &callid_first_file,&file_type);
  
  memset(radar_name, '\0', MAX_NAME_LEN);
  if (verbose) {
	fprintf(stderr, "infile: %s\n",infile);
	fprintf(stderr, "firstfile: %s\n", callid_first_file);
	fprintf(stderr, "filetype: %d\n", file_type);
  }

  if (file_type == NON_HDF) 
	get_radar_name_from_NON_HDF(infile, callid_first_file, radar_name);

  else
  	get_radar_name_from_HDF(infile, radar_name);
  
  if (strlen(radar_name) < 1)
	strcpy(radar_name, "unknown");


  fprintf(stdout, "%s", radar_name);  
} /* main */

void handler(int sig)
{
  fprintf(stderr, "%s: Got signal %d. Abort.\n", this_prog, sig);
  kill(0, sig);
  if (sig == SIGINT || sig == SIGKILL || sig == SIGSTOP) {
	exit (-2);
  }
  exit(-1);
}

void usage(char *prog)
{
  fprintf(stderr, "Get Radar Name from any data file recognized by RSL or 1C-51 HDF file.\n");
  fprintf(stderr, "Usage (%s)\n", PROG_VERSION);
  fprintf(stderr, "  %s [-v] [-f first_file_or_callid] NON_HDF|HDF infile\n", prog);
  fprintf(stderr, "\n  Note: NON_HDF file may be compressed. \n");
  exit(-1);
}

void process_argvs(int argc, char **argv, 
				  char **infile, char **callid_first_file, 
				  file_type_t *file_type)
{
  extern int optind;
  extern char *optarg;
  extern int optopt;
  int c;

  char *tmp_str;

  while ((c = getopt(argc, argv, ":f:v")) != -1) {
	switch (c) {
	case 'f': *callid_first_file = (char *) optarg; break;
	case 'v': verbose = TRUE; radar_verbose_flag = 1; break;
    case '?': fprintf(stderr, "option -%c is undefined\n", optopt);
	  usage(this_prog);
    case ':': fprintf(stderr, "option -%c requires an argument\n",optopt);
	  usage(this_prog);
    default: break;
    }
  }

  /* must have 2 things listed.  */
  if (argc - optind < 2) usage(this_prog);

  tmp_str = argv[optind++];
  if (strcasecmp(tmp_str, "NON_HDF") == 0) *file_type = NON_HDF;
  else if (strcasecmp(tmp_str, "HDF") == 0) *file_type = HDF;
  else {
	fprintf(stderr, "Error: Program recognizes NON_HDF or HDF only.\n");
	exit(-1);
  }
  *infile = (char *)strdup(argv[optind++]);
} /* process_argvs */


void get_radar_name_from_NON_HDF(char *infile, char *callid_first_file, 
							char *radar_name)
{
  Radar *radar;

  if (infile == NULL || radar_name == NULL) return;
  if (callid_first_file)
	radar = RSL_anyformat_to_radar(infile, callid_first_file);
  else 
	radar = RSL_anyformat_to_radar(infile);
  if (radar == NULL) {
	if (verbose) fprintf(stderr, "radar is NULL\n");
	return;
  }

  strcpy(radar_name, radar->h.radar_name);
  /* Change to TSDIS' site naming convention */
  if (strcasecmp(radar_name, "KMLB") == 0)
	  strcpy(radar_name, "MELB");
  else if (strcasecmp(radar_name, "KHGX") == 0)
	strcpy(radar_name, "HSTN");
  if (strcasecmp(radar_name, "KTBW") == 0)
	  strcpy(radar_name, "TAMP");
  if (strcasecmp(radar_name, "KMIA") == 0)
	strcpy(radar_name, "MIAM");
  if (strcasecmp(radar_name, "KLCH") == 0)
	strcpy(radar_name, "LKCH");
  if (strcasecmp(radar_name, "KCRP") == 0)
	  strcpy(radar_name, "CORC");

  RSL_free_radar(radar);
} /* get_radar_name_from_NON_HDF */


void get_radar_name_from_HDF(char *infile, char *radar_name)
{
  int status;
  IO_HANDLE fh;

  if (infile == NULL || radar_name == NULL) return;

  memset(&fh, '\0', sizeof(IO_HANDLE));
  status = TKopen(infile, TK_L1C_GV, TK_READ_ONLY, &fh);
  if (status != TK_SUCCESS)
	return;
  TKreadMetadataChar(&fh, TK_RADAR_NAME, radar_name);
  TKclose(&fh);

} /* get_radar_name_from_HDF */
