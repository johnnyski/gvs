/*
 *
 *  get_info_from_file:
 *      Read info from any file (recognized by the RSL lib)
 *      or from level 2 HDF file and echo info to STDOUT. Input file may be 
 *      compressed.
 *
 *-------------------------------------------------------------------
 *
 * By:  Ngoc-Thuy Nguyen
 *      Science Systems and Applications, Inc. (SSAI)
 *      NASA/TRMM Office
 *      nguyen@trmm.gsfc.nasa.gov
 *      January 15, 1998
 *
 ***********************************************************************/ 


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
#include <gv_utils.h>


typedef enum { HDF, NON_HDF} file_type_t;
/* Define info_type_t so can be used with bitwise operations */
typedef enum {INFO_ZERO=0, END_DATE_TIME_INFO=1, RADAR_NAME_INFO=2} info_type_t;

#define END_DATE_TIME_STR "End Date Time: "
#define RADAR_NAME_STR    "Radar Name:    "

#define MAX_NAME_LEN    30
#define MAX_STR_LEN     100

static char *this_prog;
static int verbose = 0;
extern int radar_verbose_flag;
void get_info_from_HDF(char *infile, product_type_t ptype, info_type_t info_type, FILE *fp);
void get_info_from_NON_HDF(char *infile, char *callid_first_file, 
						info_type_t info_type, FILE *fp);
static void handler(int sig);
void process_argvs(int argc, char **argv, product_type_t *ptype,
				   info_type_t *info_type, char **infile, 
				   char **callid_first_file, file_type_t *file_type);

int main (int argc, char **argv)
{
  char *infile = NULL;
  file_type_t file_type;
  char *callid_first_file = NULL;
  char radar_name[MAX_NAME_LEN];
  product_type_t ptype;
  info_type_t info_type=INFO_ZERO;
  FILE *fp;

  this_prog = argv[0];

  /* Set signals' handler */
  set_signal_handlers();

  process_argvs(argc, argv, &ptype, &info_type, 
				&infile, &callid_first_file,&file_type);
  
  memset(radar_name, '\0', MAX_NAME_LEN);
  if (verbose) {
	fprintf(stderr, "infile: %s\n",infile);
	fprintf(stderr, "firstfile: %s\n", callid_first_file);
	fprintf(stderr, "filetype: %d\n", file_type);
  }

  fp = stdout;
  if (file_type == NON_HDF) 
	get_info_from_NON_HDF(infile, callid_first_file, info_type, fp);

  else
  	get_info_from_HDF(infile, ptype, info_type, fp);
  exit(0);
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
  fprintf(stderr, "Get information from any data file recognized by RSL or level 2 HDF file.\n");
  fprintf(stderr, "Usage (%s)\n", PROG_VERSION);
  fprintf(stderr, "  %s [-v] [-e] [-n] {[-f first_file_or_callid] NON_HDF} | {HDF 2A53|2A54|2A55} infile\n", prog);
  fprintf(stderr, "     where,\n"
                  "       -e: Get the end date and time from file. Output's prefix: %s\n", END_DATE_TIME_STR);
  fprintf(stderr, "       -n: Get the radar name from file. Output's prefix: %s\n", RADAR_NAME_STR);
  fprintf(stderr, "\n  Note: Input file may be compressed. \n");
  fprintf(stderr, "  The data will be outputed to stdout.\n");    
  exit(-1);
}

void process_argvs(int argc, char **argv, product_type_t *ptype, 
				   info_type_t *info_type, char **infile, 
				   char **callid_first_file, file_type_t *file_type)
{
  extern int optind;
  extern char *optarg;
  extern int optopt;
  int c;

  char *tmp_str;

  while ((c = getopt(argc, argv, ":f:nev")) != -1) {
	switch (c) {
	case 'f': *callid_first_file = (char *) optarg; break;
	case 'v': verbose = TRUE; radar_verbose_flag = 1; break;
	case 'e': *info_type |= END_DATE_TIME_INFO; break;
	case 'n': *info_type |= RADAR_NAME_INFO; break;
    case '?': fprintf(stderr, "option -%c is undefined\n", optopt);
	  usage(this_prog);
    case ':': fprintf(stderr, "option -%c requires an argument\n",optopt);
	  usage(this_prog);
    default: break;
    }
  }

  /* must have atl least 2 things listed.  */
  if (argc - optind < 2) usage(this_prog);

  tmp_str = argv[optind++];
  if (strcasecmp(tmp_str, "NON_HDF") == 0) *file_type = NON_HDF;
  else if (strcasecmp(tmp_str, "HDF") == 0) {
	*file_type = HDF;
	tmp_str = argv[optind++];
	if (strcasecmp(tmp_str, "2A53") == 0)
	  *ptype = P2A53;
	else if (strcasecmp(tmp_str, "2A54") == 0)
	  *ptype = P2A54;
	else if (strcasecmp(tmp_str, "2A55") == 0)
	  *ptype = P2A55;
	else {
	  fprintf(stderr, "Error: Program does not recognize %s\n", tmp_str);
	  exit(-1);
	}
  }
  else {
	fprintf(stderr, "Error: Program recognizes NON_HDF or HDF only.\n");
	exit(-1);
  }
  *infile = (char *)strdup(argv[optind++]);
} /* process_argvs */


void get_info_from_NON_HDF(char *infile, char *callid_first_file, 
						   info_type_t info_type, FILE *fp)
{
  Radar *radar;
  Radar_header *h;
  char info_str[MAX_STR_LEN];

  if (infile == NULL) return;
  if (callid_first_file)
	radar = RSL_anyformat_to_radar(infile, callid_first_file);
  else 
	radar = RSL_anyformat_to_radar(infile);
  if (radar == NULL) {
	if (verbose) fprintf(stderr, "radar is NULL\n");
	return;
  }
  h = &(radar->h);
  if (h == NULL) return;

  if (info_type & RADAR_NAME_INFO) {
	strcpy(info_str, h->radar_name);
	/* Change to TSDIS' site naming convention */
	if (strcasecmp(info_str, "KMLB") == 0)
	  strcpy(info_str, "MELB");
	else if (strcasecmp(info_str, "KHGX") == 0)
	  strcpy(info_str, "HSTN");
	if (strcasecmp(info_str, "KTBW") == 0)
	  strcpy(info_str, "TAMP");
	if (strcasecmp(info_str, "KMIA") == 0)
	  strcpy(info_str, "MIAM");
	if (strcasecmp(info_str, "KLCH") == 0)
	  strcpy(info_str, "LKCH");
	if (strcasecmp(info_str, "KCRP") == 0)
	  strcpy(info_str, "CORC");
	fprintf(fp, "%s %s\n", RADAR_NAME_STR, info_str);
  }
  if (info_type & END_DATE_TIME_INFO) {
	sprintf(info_str, "%2.2d/%2.2d/%4.4d %2.2d:%2.2d:%f", h->month, h->day, 
			h->year, h->hour, h->minute, h->sec);
	fprintf(fp, "%s %s\n", END_DATE_TIME_STR, info_str);
  }

  RSL_free_radar(radar);
} /* get_info_from_NON_HDF */


void get_info_from_HDF(char *infile, product_type_t ptype,
					   info_type_t info_type, FILE *fp)
{
  int status;
  IO_HANDLE fh;
  char info_str[MAX_STR_LEN];
  DATE_STR date;
  TIME_STR time;
  int data_type;
  char *tmp_infile = NULL;

  if (infile == NULL || info_str == NULL) return;

  switch (ptype) {
  case P2A53:
	data_type = TK_L2A_53S;
	break;
  case P2A54:
	data_type = TK_L2A_54S;
	break;
  case P2A55:
	data_type = TK_L2A_55S;
	break;
  default:
	return;
  }
  /* Uncompress infile to a tmp file if applicable */
  tmp_infile = (char *) calloc(1, strlen(infile)+5);
  if (tmp_infile == NULL) {
	perror("calloc tmp_infile");
	return;
  }
  create_tmp_filename(infile, "", tmp_infile);
  uncompress_file(infile, tmp_infile);
  infile = tmp_infile;
  memset(&fh, '\0', sizeof(IO_HANDLE));
  status = TKopen(infile, data_type, TK_READ_ONLY, &fh);
  if (status != TK_SUCCESS)
	goto DONE;
  if (info_type & RADAR_NAME_INFO) {
	TKreadMetadataChar(&fh, TK_RADAR_NAME, info_str);
	fprintf(fp, "%s %s\n", RADAR_NAME_STR, info_str);
  }
  if (info_type & END_DATE_TIME_INFO) {
	TKreadMetadataInt(&fh, TK_END_DATE, &date);
	TKreadMetadataInt(&fh, TK_END_TIME, &time);
	sprintf(info_str, "%2.2d/%2.2d/%4.4d %2.2d:%2.2d:%2.2d", date.tkmonth, 
			date.tkday, date.tkyear, time.tkhour, time.tkminute, time.tksecond);
	fprintf(fp, "%s %s\n", END_DATE_TIME_STR, info_str);
  }

  TKclose(&fh);
DONE:
  remove (tmp_infile);

} /* get_info_from_HDF */


