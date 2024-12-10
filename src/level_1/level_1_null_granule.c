/******************************************************************

	   Mainline driver and assorted functions for GV null_granule
		 1C-51.HDF file generation.

  -----------------------------------------------------------------
		mike.kolander@trmm.gsfc.nasa.gov
		(301) 286-1540
*******************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
/* TSDIS toolkit function and structure definitions. */
#include "IO.h"
#include "IO_GV.h"
/* RSL function and structure definitions. */
#include "rsl.h"
/* Parameter definitions for 1B-51 and 1C-51 HDF
	 file handling applications using the TSDIS toolkit. */
#include "toolkit_1BC-51_appl.h"

#include <gv_site.h>

#define MAX_RADAR_SITES  15   /* 15 current radar sites. */

static char *this_prog = "";

/*************************************************************/
/*                                                           */
/*                Function Prototypes                        */
/*                                                           */
/*************************************************************/
int parse_HDFfilename(Radar *radar, char *HDF_1C_filename);
Radar *create_radar(char *HDF_1C_filename);
void usage(void);



/*************************************************************/
/*                                                           */
/*                    parse_HDFfilename                      */
/*                                                           */
/*************************************************************/
int parse_HDFfilename(Radar *radar, char *HDF_1C_filename)
{
	/* Extract date, time, and siteName from the HDF filename, and
		 store in the radar->header.

		 Returns: OK
		          ABORT, if error.
*/
	char siteName[6];
	int year, mon, day, hour;
	
	/* Example filename: 1C51.921002.13.MELB.1.HDF */
	if (sscanf(HDF_1C_filename, "1C51.%2d%2d%2d.%2d.%4s.%*d.HDF",
						 &year, &mon, &day, &hour, siteName) != 5)
	{
		fprintf(stderr, "Error parsing HDF filename.\n");
		return(ABORT);
	}
	
	if (year > 80) radar->h.year = year + 1900;
	else radar->h.year = year + 2000;
	radar->h.month = mon;
	radar->h.day = day;
	radar->h.hour = hour - 1;
	radar->h.minute = 0;
	strncpy(radar->h.radar_name, siteName, 4);
	return(OK);
}

/*************************************************************/
/*                                                           */
/*                       create_radar                        */
/*                                                           */
/*************************************************************/
Radar *create_radar(char *HDF_1C_filename)
{
	/* 
	 * Make a radar structure, and fill the radar->header using values
	 * extracted from the HDF_filename, and from the site database file
	 * 'gv_radar_site_info.data'.
	 *
	 * Returns: radar, if success.
	 *          NULL, else.
	 */
	gv_utils_radar_site_info_t *siteInfo;
	Radar *radar;
	
	
	/* Create a Radar structure. */
	radar = (Radar *)RSL_new_radar(1);
	if (radar == NULL) 
	{
		perror("Error allocating radar structure.\n");
		return(NULL);
	}

	/* Extract date, time, and siteName from the HDF filename, and store
	   in the radar->header. */
	if (parse_HDFfilename(radar, HDF_1C_filename) < 0)
	{
		RSL_free_radar(radar);
		return(NULL);
	}

	/* Get radar site info from file 'gv_radar_site_info.data'. */
	siteInfo = gv_utils_get_radar_site_info(GV_UTILS_TSDIS_NAME,
																					radar->h.radar_name);
	if (siteInfo == NULL)
	{
		fprintf(stderr, "Couldn't find site >%s< in site database file\n\n",
						radar->h.radar_name);
		RSL_free_radar(radar);
		return(NULL);
	}
	/*
	 * OK, found the radar site in the database, so load radar->header
	 * values.
	 */
	strncpy(radar->h.name, siteInfo->fields.tsdis_name, 7);
	strncpy(radar->h.city, siteInfo->fields.city, 14);
	strncpy(radar->h.state, siteInfo->fields.state, 2);
	strncpy(radar->h.country, siteInfo->fields.country, 14);
	radar->h.nvolumes = 0; /* This radar structure contains no volumes.*/
	/* Free gv_utils memory */
	gv_utils_free_radar_site_info(siteInfo);
	return(radar);
}

/*************************************************************/
/*                                                           */
/*                        usage                              */
/*                                                           */
/*************************************************************/
void usage(void)
{

    fprintf(stderr, "Usage (%s):\n", CURR_PROG_VERSION);
	fprintf(stderr,"%s [-v] [-c 1C-51-HDF_output_dir] 1C51_HDFfile\n\n", this_prog);
	fprintf(stderr,"\t-v                     : verbose\n");
	fprintf(stderr,"\t-c 1C51_HDF_output_dir : Default: Writes 1C51_HDF into ./\n");
	fprintf(stderr, "\t1C51_HDFfile format    : 1C51.yymmdd.n.site.v.HDF\n\n");
	exit(ABORT);
}

extern int getopt(int argc, char * const argv[], const char *optstring);
/*************************************************************/
/*                                                           */
/*                         main                              */
/*                                                           */
/*************************************************************/
int main(int argc, char **argv)
{
/* 
	 Checks content and validity of the command line which invoked this
	 function. If valid, initiates processing required to generate a
	 null 1C-51 HDF granule.

	 Returns:
        OK     0  : Nominal termination with product produced.
        ABORT -1  : PANIC exit -> No product produced.
        QUIT  -3  : Anomalous_condition exit -> No product produced.
*/
	char HDF_1C_filename[TK_MAX_FILENAME];
	char outpath_1C51[TK_MAX_FILENAME];
	int c, status;
	Radar *radar;
	extern char *optarg;
	extern int optind;

	this_prog = argv[0];
	if (argc < 2) usage();

	strcpy(outpath_1C51, "./");   /* Default 1C-51.HDF file path */

	/* Read options from command line. */
	while ((c=getopt(argc, argv, "vc:")) != EOF)
	{
		switch (c)
		{
		case 'v':
			RSL_radar_verbose_on();
			break;
		case 'c':           
			strncpy(outpath_1C51, optarg, TK_MAX_FILENAME-25);
			strcat(outpath_1C51, "/");
			break;
		default:
			fprintf(stderr, "Option <%c> is not valid\n", c);
			usage();
		}     /* end switch */
	}  /* end while */

	/* Check for one remaining item (1C51_HDFfile) in the command line. */
	if ((argc - optind) != 1)
	{
		fprintf(stderr, "\nMust specify a 1C-51 outfile.\n\n");
		exit(ABORT);
	}

	/* Check validity of 1C-51 HDF filename. */
	strcpy(HDF_1C_filename, argv[optind]);
	if ((strlen(HDF_1C_filename) < 24) || (strlen(HDF_1C_filename) > 25) ||
			(strncmp(HDF_1C_filename, "1C51.", 5) != 0))
	{
		fprintf(stderr, "\nInvalid 1C-51 HDF filename: %s\n", HDF_1C_filename);
		fprintf(stderr, "1C51_HDFfile format: 1C51.yymmdd.n.site.v.HDF\n\n");
		usage();
	}
	/* 
	 * Make a radar structure, and fill the radar->header using values
	 * extracted from the HDF_filename. They'll be used to fill HDF
	 * metadata fields.
	 */
	radar = create_radar(HDF_1C_filename);
	if (radar == NULL) exit(ABORT);
	
	strcat(outpath_1C51, HDF_1C_filename);
	status = RSL_radar_to_hdf(radar, outpath_1C51);
	exit(status);
}
