
/******************************************************************
	   Mainline driver and assorted functions for level_1 HDF/UF/GIF/PGM
		 file generation.

	   Moves all VOSs from a radar data disk file into one or more
		 of the following files:
		   1B-51 HDF
			 1C-51 HDF
			 1C-51 UF.
		 Does QC processing of reflectivity data (Z and Zdr) for 1C-51
		 products.
		 Creates GIF/PGM images of user-selected VOS sweeps.

		 Input file format may be any of the common radar data formats:
		 WSR-88D, SIGMET, UF, HDF.  An HDF file may contain up to 12 VOSs.
		 All other formats support only 1 VOS per file.

  -----------------------------------------------------------------
  mike.kolander@trmm.gsfc.nasa.gov
  wolff@trmm.gsfc.nasa.gov
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
/* GV stuff */
#include <gv_time.h>
#include <gvs_sat_coin.h>
#include <gv_site.h>

/*****************************************************************
                 PRODUCT / ALGORITHM versions
Change to reflect updates.
******************************************************************/
#define B_PRODUCT_VERSION  "2"
#define B_ALGORITHM_VERSION "2.0"

#define C_PRODUCT_VERSION  "4"
#define C_ALGORITHM_VERSION "3.0"
/******************************************************************/

#define SITENAME 0
#define CALLID   1
#define CITY     2
#define STATE    3
#define COUNTRY  4
#define HDR_NAME 5

#define QC_TABLE_SIZE 15000       /* lines */
#define QC_TABLE_ENTRY_SIZE 200  /* chars */

/* Flags for product generation */
#define B_PRODUCT    1   /* 1B-51     */
#define C_PRODUCT    2   /* 1C-51     */
#define UF_PRODUCT   4   /* 1C-UF     */
#define GIF_PRODUCT  8   /* Image.gif */
#define PGM_PRODUCT 16   /* Image.pgm */

#define MAX_RADAR_SITES  15   /* 15 current radar sites. */
#undef MINIMUM_SWEEPS
#define MINIMUM_SWEEPS 4   /* Min sweeps needed to do QC processing.*/



/*************************************************************/
/*                                                           */
/*                Function Prototypes                        */
/*                                                           */
/*************************************************************/
int nonNULLobjects(void **object, int nobjects);
int topOfHour(int minute);
int bottomOfHour(int minute);
int hdfFilePeek(char *hdfFileName, int productID, int *numVos,
								char *metaDataString);
int vosTimeCheck(Radar *radar, int numVos, char *metaDataString);
int satelliteCheck(Radar *radar, char *siteName, char *satFileName);
int qcDateTimeItemMatch(int radarItem, char *qcLineItem);
int qcDateTimeMatch(Radar *radar, char *qcLine);
int qcParmInit(float *qcParm, char *qcFileName, Radar *radar);
int hdf1B51Create(Radar *radar, char *dirpath, char *siteName);
int hdf1C51Create(Radar *radar, char *outPath, char *siteName,
				  float *qcParm, char *satFilePath, int doSatCheck,
				  int min_sweeps, int vrMask, char *clutter_map_fname);
int ufCreate(Radar *radar, char *dirpath, char *siteName, float *qcParm,
			 int min_sweeps, int vrMask, char *clutter_map_fname);
int ImgCreate(Radar *radar, char *outPath_gif, char *outPath_pgm, 
	                 char *siteName, float *qcParm, int nimages, int min_sweeps, 
	                 int vrMask, int img_product, char *clutter_map_fname);
void radarSite_tutorial(char *radarName1, char *radarName2);
int radarSiteCheck(Radar *radar, char *siteName);
int vosValidityCheck(Radar *radar);
void usage(void);

extern int RSL_radar_to_hdf(Radar *radar, char *outfile);
extern void RSL_set_hdf_qc_parameters(float *qcParm);
extern void RSL_set_tkMetaDataString(char *string, int param);
extern Radar *qualityControl(Radar *radar, float *qcParm, int vrMask, char *clutter_map_fname);
extern Volume *truncate_volume(Volume *v, float max_range, float min_dbz);
extern void RSL_select_fields(char *field_type, ...);
extern enum File_type RSL_filetype(char *infile);


static int verbose=0;
int qc_done=0;
char *thisprog = "";
char HDF_1B_filename[TK_MAX_FILENAME];
char HDF_1C_filename[TK_MAX_FILENAME];
char UF_1C_filename[TK_MAX_FILENAME];
int q_monitor_products = 0;
char *B_product_version_str;
char *C_product_version_str;

/*************************************************************/
/*                                                           */
/*                     nonNULLobjects                        */
/*                                                           */
/*************************************************************/
int nonNULLobjects(void **object, int nobjects)
{
	/* Count and return the no. of non-null objects in a linear
		 array of length 'nobjects'.
  */
	int dim, j;

	dim = 0;
	for (j=0; j<nobjects; j++)
	  if (object[j] != NULL) dim++;
	return(dim);
}

/*************************************************************/
/*                                                           */
/*                          topOfHour                        */
/*                                                           */
/*************************************************************/
int topOfHour(int minute)
{
/* Returns:
	   1, if minute indicates top_of_the_hour.
		 0, else.
*/
	if ((minute >= 0) && (minute <= 29))
	  return(1);
	else
	  return(0);
}

/*************************************************************/
/*                                                           */
/*                       bottomOfHour                        */
/*                                                           */
/*************************************************************/
int bottomOfHour(int minute)
{
/* Returns:
	   1, if minute indicates bottom_of_the_hour.
		 0, else.
*/
	if ((minute >= 30) && (minute <= 59))
	  return(1);
	else
	  return(0);
}

/*************************************************************/
/*                                                           */
/*                          hdfFilePeek                      */
/*                                                           */
/*************************************************************/
int hdfFilePeek(char *hdfFileName, int productID, int *numVos,
								char *metaDataString)
{
	/* Open the HDF file, if it exists, to:
		 1. find how many VOSs it contains.
		 2. Read the metaDataString 'GEN_DATE_INPUT_FILES', if 1C-51 file.
		    If 1B-51 file, or if the file doesn't exist, then we don't
				care about the metaDataString.
		 Close the HDF file.

		 Returns: OK
		          ABORT, if error.
  */
	int status;
	struct stat buf;
	IO_HANDLE granuleHandle;

	/* If this HDF file exists, open it. */
	if (stat(hdfFileName, &buf) == 0)
	{
		status = TKopen(hdfFileName, productID, TK_READ_ONLY, &granuleHandle); 
		if (status != TK_SUCCESS)
		{
			if (verbose)
		    fprintf(stderr, "\nhdfFilePeek: *** TKopen() error\n");
			return(ABORT);
		}
		if (productID == TK_L1C_GV)  /* 1C-51 file? */
		{
			/* Read metadata field from the file, and copy it to a global
				 RSL variable. It indicates which of the two time slots the
				 VOSs in the HDF file occupy, and will be used by level_1
				 function 'vosTimeCheck()' to decide whether to write this VOS
				 into the 1C-51 file.
				 If this VOS is added to the 1C-51 file, then this metadata
				 field will also be needed, and altered, by RSL function
				 'metaDataWrite()' in file 'radar_to_hdf_1.c'.
			*/
			TKreadMetadataChar(&granuleHandle, TK_GEN_DATE_INPUT_FILES, metaDataString);
			RSL_set_tkMetaDataString(metaDataString, TK_GEN_DATE_INPUT_FILES);
		} /* end if (productID == TK_L1C_GV) */
		*numVos = TKgetNvos(&granuleHandle);
		TKclose(&granuleHandle);
	} /* end if (stat(hdfFileName, &buf) */
	else /* HDF file does not exist. */
	{
		*numVos = 0;
		strcpy(metaDataString, "");
	}
	return(OK);
}

/*************************************************************/
/*                                                           */
/*                       vosTimeCheck                        */
/*                                                           */
/*************************************************************/
int vosTimeCheck(Radar *radar, int numVos, char *metaDataString)
{	
	/* This function is invoked only if the following are true:
	   1: The desired product is 1C-51.
	   2: No satellite pass coincides with the VOS.
	 So, here we check if this VOS is required for inclusion into the
	 1C-51 HDF file.

	 At this point, exactly one of the following must be true:
		 1. The HDF file does not exist; ie, the file contains no VOSs.
		    We will check the time of this new VOS, and write it into the 
				file if the time falls at either the top of the hour,
				or at the bottom of the hour.
		 2. The HDF file already contains one or more VOSs. In this case, we
		    we check if this new VOS falls into a free time slot. If so,
				we need this VOS.
			

	 Returns:  1 if this VOS is acceptable for inclusion into the 1C-51 
	             HDF file.
	           0 if this VOS is not needed for the HDF file.
	*/

	if (numVos == 0)  /* No HDF file exists for the hour of this VOS. */
	{
		if ((topOfHour(radar->h.minute)) || (bottomOfHour(radar->h.minute)))
		  return(1);
		else   /* This VOS is not needed. */
		  return(0);
	}

	/* If this is a top_of_the_hour VOS, and the file needs one...
	   Note metaDataString contains some variant of the char string
		 'unKNOWN'. The first char is lower_case if the file contains
		 no top_of_the_hour VOS. Similiarly, the 2nd char is lower_case
		 if the file lacks a bottom_of_the_hour VOS.*/
	if ( (topOfHour(radar->h.minute)) && (metaDataString[0] == 'u') )
	  return(1);
	/* If this is a bottom_of_the_hour VOS, and the file needs one... */
	if ( (bottomOfHour(radar->h.minute)) && (metaDataString[1] == 'n') )
	  return(1);

	return(0); /* This VOS is not needed. */
}

/*************************************************************/
/*                                                           */
/*                      satelliteCheck                       */
/*                                                           */
/*************************************************************/
int satelliteCheck(Radar *radar, char *siteName, char *satFilePath)
{
/* Determines whether or not a satellite overpass coincides with the
	 site/date/time of the VOS in the radar structure. 

	 Checks for a satellite overpass within a +-30 minute window around
	 the time of the VOS.

	 Returns:  1 if a coincident satellite pass exists.
	           0 if not.
						-1 if error.
*/
	char ca_date_str[32], ca_time_str[32];
	int hit, status;
	float ca_distance;
	DATE_STR tkdate;
	TIME_STR tktime;

	tkdate.tkyear = radar->h.year;
	tkdate.tkmonth = radar->h.month;
	tkdate.tkday = radar->h.day;
	tktime.tkhour = radar->h.hour;
	tktime.tkminute = radar->h.minute;
	tktime.tksecond = radar->h.sec;
/*
	status = get_sat_coinc_info(site_name2id(siteName), &tkdate, &tktime, 
								verbose, satFilePath, ca_date_str, ca_time_str,
								&ca_distance, &hit);
*/
	status = get_sat_coinc_info(site_name2id(siteName), &tkdate, &tktime, 
								0, satFilePath, ca_date_str, ca_time_str,
								&ca_distance, &hit);

	if (status < 0) return(status);  /* Error occurred. */
	else if (hit == 1) return(1);
	else return(0);
}

/*************************************************************/
/*                                                           */
/*                    qcDateTimeItemMatch                    */
/*                                                           */
/*************************************************************/
int qcDateTimeItemMatch(int radarItem, char *qcLineItem)
{
	/* Attempts to match a date/time item from a radar VOS against
		 a date/time item from a qcFile line.
		 qcFile notation: '*' is wildcard character.
		                  '-' denotes a range of dates or times.
		 Returns: 1, if match.
		          0, if no match.
  */
	int qcLineItem1, qcLineItem2;
	
	if (strchr(qcLineItem, '*') == NULL)
	  if (strchr(qcLineItem, '-') == NULL)
		{
		  if (atoi(qcLineItem) != radarItem) return(0);
		}
		else
		{
			if (sscanf(qcLineItem, "%d - %d", &qcLineItem1, &qcLineItem2) != 2)
			  return(0);
			if ((radarItem < qcLineItem1) ||
					(radarItem > qcLineItem2)) return(0);
		}
	return(1);
}

/*************************************************************/
/*                                                           */
/*                     qcDateTimeMatch                       */
/*                                                           */
/*************************************************************/
int qcDateTimeMatch(Radar *radar, char *qcLine)
{
	/* Compares the date/time of the radar VOS with the date/time
		 of the qcFile line entry.
		 Returns: 1, if match.
		          0, if no match.
	*/
	char syear[8], smonth[8], sday[8], shour[8], sminute[8];

	/* Parse the date/time components out of the qcFile line. */
	if (sscanf(qcLine, "%s %s %s %s %s", sminute, shour, sday, smonth,
						 syear) != 5) return(0);
	if (!qcDateTimeItemMatch(radar->h.year-1900, syear) && 
	    !qcDateTimeItemMatch(radar->h.year % 100, syear) &&
	    !qcDateTimeItemMatch(radar->h.year, syear)) return(0);
	if (!qcDateTimeItemMatch(radar->h.month, smonth)) return(0);
	if (!qcDateTimeItemMatch(radar->h.day, sday)) return(0);
	if (!qcDateTimeItemMatch(radar->h.hour, shour)) return(0);
	if (!qcDateTimeItemMatch(radar->h.minute, sminute)) return(0);
	return(1);
}

/*************************************************************/
/*                                                           */
/*                        qcParmInit                         */
/*                                                           */
/*************************************************************/
int qcParmInit(float *qcParm, char *qcFileName, Radar *radar)
{
/* Opens the operator-supplied QC parameters file, and loads the qcParm
	 array with the QC parameters which will be used for subsequent QC
	 processing of the VOS contained in the RSL structure.

	 Returns:  OK (0)
	           ABORT (-1) if fails.
*/
	FILE *qcFile;
	char buf[256];
	char **tableEntry;
	int j, nline;
	float parm[NUMBER_QC_PARAMS];

	qcFile = (FILE *)fopen(qcFileName, "r");
	if (qcFile == NULL)
	{
		fprintf(stderr, "\nqcParmInit(): Error opening qcFile: %s\n",
						qcFileName);
		return(ABORT);
	}
	/* Skip past comment lines until we get to the first table entry. */
	while (1)
	{
		if (fgets(buf, 256, qcFile) == NULL)
		{
			fprintf(stderr, "qcParmInit(): Error reading qcFile %s\n",
							qcFileName);
			fclose(qcFile);
			return(ABORT);
		}
		if (strlen(buf) > 30)
		  if (strncmp(buf, "#", 1) != 0)
			  break;  /* We've found the 1st table entry. */
	} /* end while (1) */

	/* Set up a QC table array for 256 entries, */
	tableEntry = (char **)calloc(QC_TABLE_SIZE, sizeof(char *));
	/* Create the 1st table entry from buf. */
	tableEntry[0] = (char *)calloc(QC_TABLE_ENTRY_SIZE, sizeof(char));
	strncpy(tableEntry[0], buf, QC_TABLE_ENTRY_SIZE-1);
	nline = 1;
	/* Read each table entry from the file into the array 'tableEntry'. */
	for (j=1; j<QC_TABLE_SIZE; j++)
	{
		tableEntry[j] = (char *)calloc(QC_TABLE_ENTRY_SIZE, sizeof(char));
		if (fgets(tableEntry[j], QC_TABLE_ENTRY_SIZE, qcFile) == NULL) break;
		nline++;
	}
	if (!feof(qcFile))
	{
		fprintf(stderr, "qcParmInit(): qcFile too long.\n");
		goto fail_exit;
	}
	/* Scan backwards from the last line, looking for a date/time match
	   with this radar volume scan. */
	for (j=nline-1; j>=0; j--)
	{
		if (strncmp(tableEntry[j], "#", 1) == 0) continue; /*Ignore comment lines.*/
		if (strlen(tableEntry[j]) <= 3) continue;  /* Ignore empty lines. */
		if (qcDateTimeMatch(radar, tableEntry[j]))
		{
			/* Found a time match. Pick the QC parameters out of the file line.*/
			if (sscanf(tableEntry[j], "%*s %*s %*s %*s %*s %f %f %f %f %f %f %f %f %f %f",
								 &parm[0], &parm[1], &parm[2],
								 &parm[3], &parm[4], &parm[5],
								 &parm[6], &parm[7], &parm[8], &parm[9]) != NUMBER_QC_PARAMS)
			{
				fprintf(stderr, "qcParmInit(): Error reading file %s\n",
								qcFileName);
				goto fail_exit;
			}
			/* We've now successfully read all the appropriate qc parms.
				 Load each qcParm slot with the value retrieved from the qcFile
				 only if the operator hasn't already filled the slot via command
				 line option.
			*/
			for (j=0; j<NUMBER_QC_PARAMS; j++)
			  if (qcParm[j] <= NOVAL_FLOAT)
				  qcParm[j] = parm[j];

			for (j=0; j<nline; j++) free(tableEntry[j]);
			fclose(qcFile);
			return(OK);
		} /* end if (qcDateTimeMatch...*/
	} /* end for (j=nline-1;...*/

	/* At this point, we've checked all table entries and couldn't find
		 a date/time match. The operator has managed to screw up the table,
		 so bail out. The qcFile top table date/time entry should be all
		 wildcard chars, which will match any VOS date/time.
	*/
	if (verbose)
	  fprintf(stderr, "\nNo VOS date/time match with any entry from qcFile: %s\n",
						qcFileName);
 fail_exit:
	fclose(qcFile);
	for (j=0; j<nline; j++) free(tableEntry[j]);
	return(ABORT);
}

/*************************************************************/
/*                                                           */
/*                         hdf1B51Create                     */
/*                                                           */
/*************************************************************/
int hdf1B51Create(Radar *radar, char *dirpath, char *siteName)
{
/* Writes a VOS into a 1B-51 HDF file .

	 Returns:
	   'OK'  if success.
		 < 0   if failure.
*/
	char granuleID[TK_MAX_FILENAME];
	char metaDataString[128];
	int numVos, status, year;

	/* The VOS must have at least 1 sweep. */
	if (radar->v[DZ_INDEX]->h.nsweeps < 1)
	{
		if (verbose)
		  fprintf(stderr, "hdf1B51Create(): VOS nsweeps:%d < minimum_sweeps:1\n",
							radar->v[DZ_INDEX]->h.nsweeps);
		return(QUIT);
	}

	if (radar->v[CZ_INDEX] != NULL)
	{
		if (verbose)
		fprintf(stderr, "hdf1B51Create(): VOS contains QC'ed, range-truncated data.\n");
		fprintf(stderr, "                 Cannot create a 1B-51 file from a 1C-51 file.\n");
		return(ABORT);
	}

  /* Create a HDFfilename using the date/site of the VOS. */
	if (radar->h.year >= 2000) year = radar->h.year - 2000;
	else year = radar->h.year - 1900;
	sprintf(granuleID, "%s1B51.%.2d%.2d%.2d.%d.%s.%s.HDF", 
					dirpath, year, radar->h.month, 
					radar->h.day, radar->h.hour+1, siteName, B_product_version_str);
	strcpy(HDF_1B_filename, granuleID);

	/* Find how many VOSs already exist in the HDF file. */
	status = hdfFilePeek(granuleID, TK_L1B_GV, &numVos, metaDataString);
	if (status < 0) return(status);
	if (numVos >= MAX_VOS)
	{
		if (verbose)
		  fprintf(stderr, "\n\nhdf1B51Create(): Can't exceed %d VOSs per granule\n",
					 MAX_VOS);
		return(QUIT);
	}

	RSL_set_tkMetaDataString(B_product_version_str, TK_PRODUCT_VERSION);
	RSL_set_tkMetaDataString(B_ALGORITHM_VERSION, TK_ALGORITHM_VERSION);
	RSL_set_tkMetaDataString(CURR_PROG_VERSION, TK_SOFTWARE_VERSION);
	/* Write the VOS data from the RSL radar structure into the HDF file. */
	status = RSL_radar_to_hdf(radar, granuleID);
	return(status);
}

/*************************************************************/
/*                                                           */
/*                         hdf1C51Create                     */
/*                                                           */
/*************************************************************/
int hdf1C51Create(Radar *radar, char *outPath, char *siteName,
				  float *qcParm, char *satFilePath, int doSatCheck,
				  int min_sweeps, int vrMask, char *clutter_map_fname)
{
/* Writes a VOS into a 1C-51 HDF file, if required.

	 The number of VOSs written into a 1C-51 HDF file is dependent
	 upon satellite location. If no satellite pass coincides with
	 site/date/hour of the VOS in the radar structure, only
	 two VOS's are kept in the 1C-51 HDF file. All VOSs within
	 +- 30 minutes of a satellite overpass are retained.

	 Returns:
	   'OK'  if success.
		 < 0   if failure.
*/
	char granuleID[TK_MAX_FILENAME];
	char metaDataString[128];
	int numVos, satPass, status, year;
	
	/* The VOS must have at least 'min_sweeps'. */
	if (radar->v[DZ_INDEX]->h.nsweeps < min_sweeps)
	{
		if (verbose)
		  fprintf(stderr,"hdf1C51Create(): VOS nsweeps:%d < minimum_sweeps:%d\n",
							radar->v[DZ_INDEX]->h.nsweeps, min_sweeps);
		return(QUIT);
	}

  /* Create a HDFfilename using the date/site of the VOS. */
	if (radar->h.year >= 2000) year = radar->h.year - 2000;
	else year = radar->h.year - 1900;
	sprintf(granuleID, "%s1C51.%.2d%.2d%.2d.%d.%s.%s.HDF",
					outPath, year, radar->h.month, 
					radar->h.day, radar->h.hour+1, siteName, C_product_version_str);
	strcpy(HDF_1C_filename, granuleID);

	/* Find out how many VOSs already exist in the HDF file, and read the 
		 metaDataString we need. */
	memset(metaDataString, '0', sizeof(metaDataString));
	status = hdfFilePeek(granuleID, TK_L1C_GV, &numVos, metaDataString);
	if (status < 0) return(status);
	if (numVos >= MAX_VOS)
	{
		if (verbose)
		  fprintf(stderr, "\n\nhdf1C51Create(): Can't exceed %d VOSs per granule\n",
					 MAX_VOS);
		return(QUIT);
	}

	if (doSatCheck)  /* Satellite coincidence checks enabled? */
	{
		/* Check if a satellite pass coincides with the site/date/time 
			 of the VOS in the radar structure. If yes, we will later write
			 this VOS into HDF file. If no, usually only 2 VOSs from each hour
			 are written into the HDF file. */
		satPass = satelliteCheck(radar, siteName, satFilePath);
		if (satPass == 1)  /* Coincidental satellite pass */
		{
			if (verbose)
			{
				fprintf(stderr, "\nA satellite overpass coincides with this VOS.\n");
				fprintf(stderr, "   ==> VOS scheduled for inclusion into 1C51-HDF file.\n");
			}
		}
		else if (satPass == 0)  /* No coincidental satellite pass */
		{ 
			if (verbose)
			{
		    fprintf(stderr, "\nNo satellite overpass coincides with this VOS.\n");
			  fprintf(stderr, "\nChecking if this VOS needed for 1C51-HDF file ...\n");
			}
			/* Check if this VOS is needed for the 1C-51 HDF file. */
			if (vosTimeCheck(radar, numVos, metaDataString))
			{
				if (verbose)
			    fprintf(stderr, "   ==> VOS scheduled for inclusion into 1C51-HDF file.\n\n");
			}
			else /* VOS not needed in the HDF file. */
			{
				if (verbose)
			    fprintf(stderr, "   ==> VOS not scheduled for inclusion into 1C51-HDF file.\n\n");
				return(OK);
			}
		} /* end else if (satPass == 0) */
		else   /* Error reading satellite file */
		{
			fprintf(stderr, "\nError finding/reading satellite overpass coincidence file.\n");
			fprintf(stderr, "TERMINATING ...\n\n");
			return(SHUTDOWN);
		}
	} /* end if (doSatCheck) */
	else  /* Satellite coincidence checks are disabled. */
	{
		if (verbose)
		{
		  fprintf(stderr, "Satellite overpass coincidence checks are **DISABLED**.\n");
			fprintf(stderr, "   ==> VOS scheduled for inclusion into 1C51-HDF file.\n");
		}
	}
	
	/* QC'd reflectivity volume(s) already exist at this point if we're
		 reprocessing. Get rid of all such QC'd volumes.
		 We want to QC the data from scratch using a new set of QC parameters.
  */
	if (radar->v[CZ_INDEX] != NULL)
	  RSL_free_volume(radar->v[CZ_INDEX]);
	if (radar->v[CD_INDEX] != NULL)
	  RSL_free_volume(radar->v[CD_INDEX]);
	
	if (verbose)
	{
	  fprintf(stderr, "\n****** Starting QC processing ...\n");
	  fprintf(stderr, "Using QC parameters:\n");
	  fprintf(stderr, "  hThresh1: %.2f  hThresh2: %.2f  hThresh3: %.2f\n",
			  qcParm[HTHRESH1], qcParm[HTHRESH2], qcParm[HTHRESH3]);
	  fprintf(stderr, "  zThresh0: %.2f  zThresh1: %.2f  zThresh2: %.2f  zThresh3: %.2f\n",
			  qcParm[ZTHRESH0], qcParm[ZTHRESH1], qcParm[ZTHRESH2], qcParm[ZTHRESH3]);
	  fprintf(stderr, "  hFreeze: %.2f  dbzNoise: %.2f  zCal: %.2f\n",
			  qcParm[HFREEZE], qcParm[DBZNOISE], qcParm[ZCAL]);
	}
	/* Create a CZ volume. If differential data exists, then 
		 also create a CD volume. */
	radar = qualityControl(radar, qcParm, vrMask, clutter_map_fname);
	if (radar->v[CZ_INDEX] == NULL)
	{
		fprintf(stderr, "****** Failed QC processing.\n\n");
		return(ABORT);
	}
	if (verbose)
	  fprintf(stderr, "****** Finished QC processing.\n\n");
	qc_done = 1;
	
	RSL_set_hdf_qc_parameters(qcParm);
	RSL_set_tkMetaDataString(C_product_version_str, TK_PRODUCT_VERSION);
	RSL_set_tkMetaDataString(C_ALGORITHM_VERSION, TK_ALGORITHM_VERSION);
	RSL_set_tkMetaDataString(CURR_PROG_VERSION, TK_SOFTWARE_VERSION);
	/* Write the VOS data from the RSL radar structure into the HDF file. */
	status = RSL_radar_to_hdf(radar, granuleID);
	return(status);
}

/*************************************************************/
/*                                                           */
/*                         ufCreate                          */
/*                                                           */
/*************************************************************/
int ufCreate(Radar *radar, char *outPath, char *siteName, float *qcParm,
			 int min_sweeps, int vrMask, char *clutter_map_fname)
{
/* Writes a VOS into a 1C-UF file.

 	 Returns:
	   'OK' if success.
		  < 0 if failure.
*/
	char granuleID[TK_MAX_FILENAME];
	int j, year;
	Volume *vr_volume;

	/* The VOS must have at least 'min_sweeps'. */
	if (radar->v[DZ_INDEX]->h.nsweeps < min_sweeps)
	{
		if (verbose)
		  fprintf(stderr, "ufCreate(): VOS nsweeps:%d < minimum_sweeps:%d\n",
							radar->v[DZ_INDEX]->h.nsweeps, min_sweeps);
		return(QUIT);
	}

  /* Create a UF filename using the date/site of the VOS. */
	if (radar->h.year >= 2000) year = radar->h.year - 2000;
	else year = radar->h.year - 1900;
	sprintf(granuleID,"%s%.2d%.2d%.2d.%d.%s.%s.%.2d%.2d.uf.gz", 
					outPath, year, radar->h.month, radar->h.day, 
					radar->h.hour+1, siteName, C_product_version_str, radar->h.hour, 
					radar->h.minute);
	strcpy(UF_1C_filename, granuleID);

	/* Remove unneeded volumes. */
	for (j=0; j<radar->h.nvolumes; j++)
	{
		if (radar->v[j] == NULL) continue;
		if ((j == DZ_INDEX) || (j == ZD_INDEX) || (j == VR_INDEX))
		  continue;
		/* If QC volumes exist, and were created by this (currently executing)
			 process, keep them. */
		if ((j == CZ_INDEX) || (j == CD_INDEX))
		  if (qc_done) continue;
		RSL_free_volume(radar->v[j]);
		radar->v[j] = NULL;
	}  /* end for (j=0; ... */

	/* If QC processing has not been done, do it now. */
	if (!qc_done)
	{
	  if (verbose)
		{
		  fprintf(stderr, "\n****** Starting QC processing ...\n");
		  fprintf(stderr, "Using QC parameters:\n");
		  fprintf(stderr, "  hThresh1: %.2f  hThresh2: %.2f  hThresh3: %.2f\n",
				  qcParm[HTHRESH1], qcParm[HTHRESH2], qcParm[HTHRESH3]);
		  fprintf(stderr, "  zThresh0: %.2f  zThresh1: %.2f  zThresh2: %.2f  zThresh3: %.2f\n",
				  qcParm[ZTHRESH0], qcParm[ZTHRESH1], qcParm[ZTHRESH2], qcParm[ZTHRESH3]);
		  fprintf(stderr, "  hFreeze: %.2f  dbzNoise: %.2f  zCal: %.2f\n",
				  qcParm[HFREEZE], qcParm[DBZNOISE], qcParm[ZCAL]);
		}
		/* Create a CZ volume. If differential data exists, then 
			 also create a CD volume. */
		radar = qualityControl(radar, qcParm, vrMask, clutter_map_fname);
		if (radar->v[CZ_INDEX] == NULL)
		{
			fprintf(stderr, "****** Failed QC processing.\n\n");
			return(ABORT);
		}
		if (verbose)
	    fprintf(stderr, "****** Finished QC processing.\n\n");
		qc_done = 1;
	} /* end if (!qc_done) */
	
	/* For each volume, truncate range at 200.0 km, and set 
		 dbz_threshold at -100.0 ,ie, no threshold. External function 
		 'truncate_volume()' written by Thuy?? */
	RSL_radar_verbose_off();
	for (j=0; j<radar->h.nvolumes; j++)
	{
		if (radar->v[j] != NULL)
		{
		  radar->v[j] = truncate_volume(radar->v[j], MAX_RANGE_1C51+0.5, -100.0);
			if (radar->v[j] == NULL)
			{
				if (verbose)
			    fprintf(stderr, "ufCreate(): Error truncating RSL volume.\n");
				return(ABORT);
			}
		}
	}

	/*
	 * OK, we're ready to write the UF file.
	 *
	 * Want velocity data in the UF file only if vrmask was applied.
	 * If no vrmask, null the velocity volume pointer. We'll restore
	 * it after writing the UF file.
	 */
	if (vrMask == 0) {
	  vr_volume = radar->v[VR_INDEX];  /* Save the velocity volume ptr. */
	  radar->v[VR_INDEX] = NULL;
	}

	/* Write the VOS data from the RSL radar structure into the UF file. */
	if (verbose)
	  fprintf(stderr, "\n****** Writing UF file: %s ...\n", granuleID);
	RSL_radar_to_uf_gzip(radar, granuleID);

	if (vrMask == 0)
	  radar->v[VR_INDEX] = vr_volume;  /* Restore the vr_volume pointer. */

	if (verbose) RSL_radar_verbose_on();
	return(OK);
}

/*************************************************************/
/*                                                           */
/*                         ImgCreate                         */
/*                                                           */
/*************************************************************/
int ImgCreate(Radar *radar, char *outPath_gif, char *outPath_pgm, 
			  char *siteName, float *qcParm, int nimages,
			  int min_sweeps, int vrMask, int img_product, char *clutter_map_fname)
{
/* Create color images of 'nimages' sweeps,
   and write into GIF/PGM files.

 	 Returns:
	   'OK' if success.
		  < 0 if failure.
*/
  char    filename[TK_MAX_FILENAME];
  char    date[8], time[8], hh[16], zz[16];
  int     i, h1, h2, h3, z0, z1, z2, z3, year;
  Sweep   *sweep;
  char    cmd[1000]; /* System command; send_product ... */
	
  memset(filename, '\0', TK_MAX_FILENAME);

  /* The VOS must have at least 'min_sweeps'. */
  if (radar->v[DZ_INDEX]->h.nsweeps < min_sweeps)
  {
	if (verbose)
	  fprintf(stderr, "ImgCreate(): VOS nsweeps:%d < minimum_sweeps:%d\n",
			  radar->v[DZ_INDEX]->h.nsweeps, min_sweeps);
	return(QUIT);
  }
  /* The VOS must have at least 'nimages' sweeps. */
  if (radar->v[DZ_INDEX]->h.nsweeps < nimages)
  {
	if (verbose)
	  fprintf(stderr, "ImgCreate(): VOS nsweeps:%d < requested # of images:%d\n",
			  radar->v[DZ_INDEX]->h.nsweeps, nimages);
	return(QUIT);
  }
  /* If QC processing has not been done, do it now. */
  if (!qc_done)
  {
	if (verbose)
	{
	  fprintf(stderr, "\n****** Starting QC processing ...\n");
	  fprintf(stderr, "Using QC parameters:\n");
	  fprintf(stderr, "  hThresh1: %.2f  hThresh2: %.2f  hThresh3: %.2f\n",
			  qcParm[HTHRESH1], qcParm[HTHRESH2], qcParm[HTHRESH3]);
	  fprintf(stderr, "  zThresh0: %.2f  zThresh1: %.2f  zThresh2: %.2f  zThresh3: %.2f\n",
			  qcParm[ZTHRESH0], qcParm[ZTHRESH1], qcParm[ZTHRESH2], qcParm[ZTHRESH3]);
	  fprintf(stderr, "  hFreeze: %.2f  dbzNoise: %.2f  zCal: %.2f\n",
			  qcParm[HFREEZE], qcParm[DBZNOISE], qcParm[ZCAL]);
	}
	/* Create a CZ volume. If differential data exists, then 
	   also create a CD volume. */
	radar = qualityControl(radar, qcParm, vrMask, clutter_map_fname);
	if (radar->v[CZ_INDEX] == NULL)
	{
	  fprintf(stderr, "****** Failed QC processing.\n\n");
	  return(ABORT);
	}
	if (verbose)
	  fprintf(stderr, "****** Finished QC processing.\n\n");
	qc_done = 1;
  } /* end if (!qc_done) */

  h1 = qcParm[HTHRESH1];
  h2 = qcParm[HTHRESH2];
  h3 = qcParm[HTHRESH3];
  z0 = qcParm[ZTHRESH0];
  z1 = qcParm[ZTHRESH1];
  z2 = qcParm[ZTHRESH2];
  z3 = qcParm[ZTHRESH3];

  /* Create a gif/pgm filename using the date/site of the VOS. */
  if (radar->h.year >= 2000) year = radar->h.year - 2000;
  else year = radar->h.year - 1900;
  sprintf(date,"%.2d%.2d%.2d", 
		  year, radar->h.month, radar->h.day);
  sprintf(time,"%.2d%.2d", radar->h.hour, radar->h.minute);
  sprintf(hh,"%.2d%.2d%.2d", h1,h2,h3);
  sprintf(zz,"%.2d%.2d%.2d%.2d", z0,z1,z2,z3);

  /*
   * Reflectivity Z
   */
  RSL_load_refl_color_table();
  if (radar->v[DZ_INDEX] != NULL) {
	for(i=0; i<nimages; i++) {
	  sweep = radar->v[DZ_INDEX]->sweep[i];
	  if(sweep != NULL) {
		if (img_product & GIF_PRODUCT)
		{
		  sprintf(filename,"%s%s_%s_%s.%s.%s.%s.%2.2d.gif",
				  outPath_gif, siteName, "DZ", date, time, hh, zz, i);
		  RSL_sweep_to_gif(sweep, filename, 400, 400, 200.0);
		  if(verbose)
			fprintf(stderr, "Z gif image:        %s\n", filename);
		} /* end if GIF_PRODUCT */
		if (img_product & PGM_PRODUCT)
		{
		  sprintf(filename,"%s%s_%s_%s.%s.%s.%s.%2.2d.pgm",
				  outPath_pgm, siteName, "DZ", date, time, hh, zz, i);
		  RSL_sweep_to_pgm(sweep, filename, 400, 400, 200.0);
		  if(verbose)
			fprintf(stderr, "Z pgm image:        %s\n", filename);
		} /* end if PGM_PRODUCT */
		if (q_monitor_products) {
		  sprintf(cmd,"send_product 1B-51 unknown 0 %s %s %s",
				  date, time, filename);
		  system(cmd);
		  unlink(filename);  /* Unless -g ... I'll do this later :-) */
		}
	  }
	}
  }
  /*
   * QC'd Reflectivity
   */
  if (radar->v[CZ_INDEX] != NULL) {
	for(i=0; i<nimages; i++) {
	  sweep = radar->v[CZ_INDEX]->sweep[i];
	  if(sweep != NULL) {
		if (img_product & GIF_PRODUCT)
		{
		  sprintf(filename,"%s%s_%s_%s.%s.%s.%s.%2.2d.gif",
				  outPath_gif, siteName, "CZ", date, time, hh, zz, i);
		  RSL_sweep_to_gif(sweep, filename, 400, 400, 200.0);
		  if(verbose)
			fprintf(stderr, "QC'd Z gif image:   %s\n", filename);
		} /* end if GIF_PRODUCT */
		if (img_product & PGM_PRODUCT)
		{
		  sprintf(filename,"%s%s_%s_%s.%s.%s.%s.%2.2d.pgm",
				  outPath_pgm, siteName, "CZ", date, time, hh, zz, i);
		  RSL_sweep_to_pgm(sweep, filename, 400, 400, 200.0);
		  if(verbose)
			fprintf(stderr, "QC'd Z pgm image:   %s\n", filename);
		} /* end if PGM_PRODUCT */
		if (q_monitor_products) {
		  sprintf(cmd,"send_product 1C-51 unknown 0 %s %s %s",
				  date, time, filename);
		  system(cmd);
		  unlink(filename);  /* Unless -g ... I'll do this later :-) */
		}
	  }
	}
  }
  /*
   * Velocity VR
   */
  if (radar->v[VR_INDEX] != NULL) {
	RSL_load_vel_color_table();
	for(i=0; i<nimages; i++) {
	  sweep = radar->v[VR_INDEX]->sweep[i];
	  if(sweep != NULL) {
		/* Map VR values to color table indices. */
		RSL_rebin_velocity_sweep(sweep);
		if (img_product & GIF_PRODUCT)
		{
		  sprintf(filename,"%s%s_%s_%s.%s.%s.%s.%2.2d.gif",
				  outPath_gif, siteName, "VR", date, time, hh, zz, i);
		  RSL_sweep_to_gif(sweep, filename, 400, 400, 200.0);
		  if(verbose)
			fprintf(stderr, "Velocity gif image: %s\n", filename);
		} /* end if GIF_PRODUCT */
		if (img_product & PGM_PRODUCT)
		{
		  sprintf(filename,"%s%s_%s_%s.%s.%s.%s.%2.2d.pgm",
				  outPath_pgm, siteName, "VR", date, time, hh, zz, i);
		  RSL_sweep_to_pgm(sweep, filename, 400, 400, 200.0);
		  if(verbose)
			fprintf(stderr, "Velocity pgm image: %s\n", filename);
		} /* end if PGM_PRODUCT */
	  }
	}
  }
  /*
   * Differential reflectivity ZDR
   */
  if (radar->v[ZD_INDEX] || radar->v[CD_INDEX])
	RSL_load_zdr_color_table();

  if (radar->v[ZD_INDEX] != NULL) {
	for(i=0; i<nimages; i++) {
	  sweep = radar->v[ZD_INDEX]->sweep[i];
	  if(sweep != NULL) {
		/* Map zdr values to color table indices. */
		RSL_rebin_zdr_sweep(sweep);
		if (img_product & GIF_PRODUCT)
		{
		  sprintf(filename,"%s%s_%s_%s.%s.%s.%s.%2.2d.gif",
				  outPath_gif, siteName, "ZD", date, time, hh, zz, i);
		  RSL_sweep_to_gif(sweep, filename, 400, 400, 200.0);
		  if(verbose)
			fprintf(stderr, "ZDR gif image:      %s\n", filename);
		} /* end if GIF_PRODUCT */
		if (img_product & PGM_PRODUCT)
		{
		  sprintf(filename,"%s%s_%s_%s.%s.%s.%s.%2.2d.pgm",
				  outPath_pgm, siteName, "ZD", date, time, hh, zz, i);
		  RSL_sweep_to_pgm(sweep, filename, 400, 400, 200.0);
		  if(verbose)
			fprintf(stderr, "ZDR pgm image:      %s\n", filename);
		} /* end if PGM_PRODUCT */
	  }
	}
  }
  /*
   * QC'd Differential reflectivity
   */
  if (radar->v[CD_INDEX] != NULL) {
	for(i=0; i<nimages; i++) {
	  sweep = radar->v[CD_INDEX]->sweep[i];
	  if(sweep != NULL) {
		/* Map zdr values to color table indices. */
		RSL_rebin_zdr_sweep(sweep);
		if (img_product & GIF_PRODUCT)
		{
		  sprintf(filename,"%s%s_%s_%s.%s.%s.%s.%2.2d.gif",
				  outPath_gif, siteName, "CD", date, time, hh, zz, i);
		  RSL_sweep_to_gif(sweep, filename, 400, 400, 200.0);
		  if(verbose)
			fprintf(stderr, "QC'd ZDR gif image: %s\n", filename);
		} /* end if GIF_PRODUCT */
		if (img_product & PGM_PRODUCT)
		{
		  sprintf(filename,"%s%s_%s_%s.%s.%s.%s.%2.2d.pgm",
				  outPath_pgm, siteName, "CD", date, time, hh, zz, i);
		  RSL_sweep_to_pgm(sweep, filename, 400, 400, 200.0);
		  if(verbose)
			fprintf(stderr, "QC'd ZDR pgm image: %s\n", filename);
		} /* end if PGM_PRODUCT */
	  }
	}
  }
  return(OK);
}

/*************************************************************/
/*                                                           */
/*                   radarSite_tutorial                      */
/*                                                           */
/*************************************************************/
void radarSite_tutorial(char *radarName1, char *radarName2)
{
	char name1[9], name2[9];

	memset(name1, '\0', sizeof(name1));
	strncpy(name1, radarName1, 8);
	memset(name2, '\0', sizeof(name2));
	strncpy(name2, radarName2, 8);
	
	fprintf(stderr, "\nIf you're attempting to read radar data files from a\n");
	fprintf(stderr, "new radar site not recognized by 'level_1', then ...\n");
	fprintf(stderr, "\n    -- Radar Sites Tutorial --\n\n");
	fprintf(stderr, "'level_1' identifies the radar site origin of a data\n");
	fprintf(stderr, "file using the database file 'gv_radar_site_info.data'.\n");
	fprintf(stderr, "Locate and examine the contents of this file. There must be\n");
	fprintf(stderr, "one table entry dedicated to your radar site.\n\n");
	fprintf(stderr, "In the table column labelled 'Radar Name', insert the\n");
	fprintf(stderr, "first 6 or 7 chars of one of the following strings.\n");
	fprintf(stderr, "Use the string most descriptive of the radar.\n\n");
	fprintf(stderr, "String1: %s\n", name1);
	fprintf(stderr, "String2: %s\n\n", name2);
	fprintf(stderr, "Reprocess the data file. If no luck, contact the TRMM Office.\n\n");
}

/*************************************************************/
/*                                                           */
/*                      radarSiteCheck                       */
/*                                                           */
/*************************************************************/
int radarSiteCheck(Radar *radar, char *siteName)
{
	/* Identify the source of the data in the rsl radar structure.
		 To do this:

		 Compare each table entry from file 'gv_radar_site_info.data'
		 with the 'radar->h' fields. If a match is found, fill various
		 'radar->h' fields using strings from the file's table entry.

		 Returns: OK, if certain about the radar site.
		          ABORT, if confused.
	*/
	gv_utils_radar_site_info_t *siteInfo, *siteInfoList;

	/* Read radar site info from file 'gv_radar_site_info.data'
		 into a linked list. */
	siteInfoList = gv_utils_get_radar_site_info(GV_UTILS_ALL, "");
	if (siteInfoList == NULL) return(ABORT);

	/*
	 * Identify the radar site: Chase down the siteInfo linked list,
	 * comparing name & ID strings from the VOS against those in the
	 * linked list.
	 */
	siteInfo = siteInfoList;
	while (siteInfo != NULL)
	{
		if ((strncmp(radar->h.radar_name, siteInfo->fields.tsdis_name, 4) == 0) ||
		    (strncmp(radar->h.radar_name, siteInfo->fields.wsr88d_id, 4) == 0) ||
		    (strncmp(radar->h.radar_name, siteInfo->fields.radar_name, 4) == 0) ||
		    (strncmp(radar->h.name,       siteInfo->fields.radar_name, 4) == 0))
		{
			/* Found the radar site. Fill various 'radar->h' fields. */
			strncpy(radar->h.name, siteInfo->fields.tsdis_name, 7);
			strncpy(radar->h.radar_name, siteInfo->fields.tsdis_name, 7);
			strncpy(radar->h.city, siteInfo->fields.city, 14);
			strncpy(radar->h.state, siteInfo->fields.state, 2);
			strncpy(radar->h.country, siteInfo->fields.country, 14);
			strncpy(siteName, siteInfo->fields.tsdis_name, 4);
			/* Free the linked list. */
			gv_utils_free_radar_site_info(siteInfoList);
			return(OK);
		}
		siteInfo = siteInfo->next;  /* Get next list entry. */
	} /* end while (siteInfo != NULL) */

	/* 
	 * Can't find a radar site in the linked list matching this VOS.
	 * Free the linked list, display a user tutorial, and abort.
	 */
	fprintf(stderr, "radarSiteCheck(): Can't determine radar site.\n");
	gv_utils_free_radar_site_info(siteInfoList);
	radarSite_tutorial(radar->h.name, radar->h.radar_name);
	return(ABORT);
}

/*************************************************************/
/*                                                           */
/*                      vosValidityCheck                     */
/*                                                           */
/*************************************************************/
int vosValidityCheck(Radar *radar)
{	
/* Checks suitability of VOS for level_1 processing:
	 1. A 'DZ' or 'ZT' volume must exist.
	 2. Checks that each volume has fewer than 'MAX_SWEEP' sweeps,.
	 3. Checks that each sweep has fewer than 'MAX_RAY' rays.

	 Returns:
	   'OK'   if success.
		 'QUIT' if failure.
*/
	int nrays, nsweeps, sindex, vindex;
	Volume *v;

	/* I here assume that 'DZ' or 'ZT' will be in every VOS. */
	if ( (radar->v[DZ_INDEX] == NULL) && (radar->v[ZT_INDEX] == NULL) )
	{
		if (verbose)
		  fprintf(stderr, "vosValidityCheck(): No reflectivity in VOS.\n");
		return(QUIT);
	}

	/* Check structural integrity of VOS. */
	for (vindex=0; vindex<radar->h.nvolumes; vindex++)
	{
		if (radar->v[vindex] == NULL) continue;
		v = radar->v[vindex];
		/* Find no. of non-NULL sweeps in this volume. */
		nsweeps = nonNULLobjects((void **)v->sweep, v->h.nsweeps);
		if (nsweeps > MAX_SWEEP)
		{
			if (verbose)
			  fprintf(stderr, "vosValidityCheck(): Too many sweeps: %d in volume[%d]\n",
								nsweeps, vindex);
			return(QUIT);
		}
		for (sindex=0; sindex<v->h.nsweeps; sindex++)
		{
			if (v->sweep[sindex] == NULL) continue;
			/* Find no. of non-NULL rays in this sweep. */
			nrays = nonNULLobjects((void **)v->sweep[sindex]->ray,
															 v->sweep[sindex]->h.nrays);
			if (nrays > MAX_RAY)
			{
				if (verbose)
				  fprintf(stderr, "vosValidityCheck(): Too many rays: %d in sweep[%d] of volume[%d]\n",
									nrays, sindex, vindex);
				return(QUIT);
			}
		} /* end for (sindex=0;... */
	} /* end for (vindex=0;... */

	return(OK);
}

/*************************************************************/
/*                                                           */
/*                        usage                              */
/*                                                           */
/*************************************************************/
void usage(void)
{
	fprintf(stderr, "Usage (%s):\n", CURR_PROG_VERSION);
	fprintf(stderr, "\nTO BUILD ANY OR ALL OF THE FOLLOWING PRODUCTS:\n");
	fprintf(stderr, "  1B-51.HDF, 1C-51.HDF, 1C-51.UF, IMAGE.GIF\n");
	fprintf(stderr, "---------------------------------------------------------\n");
	fprintf(stderr,"level_1 [-options] infile\n\n\n");
	fprintf(stderr,"OPTIONS:\n");
	fprintf(stderr,"  1B-51.HDF & 1C-51.HDF products are created by default. See -B, -C\n");
	fprintf(stderr,"  1C-51.UF & IMAGE.GIF products created only upon request. See -u, -g\n");
	fprintf(stderr, "------------------------------------\n");
	fprintf(stderr,"\t-v                     : verbose\n");
	fprintf(stderr,"\t-P 1B51_product_version: 1B51 product version number. Default: \"%s\"\n", B_PRODUCT_VERSION);
	fprintf(stderr,"\t-Q 1C51_product_version: 1C51 product version number. Default: \"%s\"\n", C_PRODUCT_VERSION);
	fprintf(stderr,"\t-B                     : No 1B-51.HDF product\n");
	fprintf(stderr,"\t-b 1B-51-HDF_output_dir: Default: Writes 1B-51.HDF into ./\n");
	fprintf(stderr,"\t-C                     : No 1C-51.HDF product\n");
	fprintf(stderr,"\t-c 1C-51-HDF_output_dir: Default: Writes 1C-51.HDF into ./\n");
	fprintf(stderr,"\t-u 1C-UF_output_dir    : Default: No 1C-51.UF product\n");
	fprintf(stderr,"\t-f firstfile      : Tape header filename. Required for WSR88D data.\n");
	fprintf(stderr,"\t-k                     : Apply clutter map from\n");
	fprintf(stderr,"\t                         $GVS_DATA_PATH/<site>.cluttermap\n");
	fprintf(stderr,"\t                         Applied to qc volume during quality control.\n");
	fprintf(stderr,"\t-m minimum_sweeps : Only VOSs having >= minimum_sweeps are\n");
	fprintf(stderr,"\t                    stored into 1C-51 files. Default: 4\n");
	fprintf(stderr,"\t-q qc_params_file : Default: $GVS_DATA_PATH/qc_params_$(site)\n");
	fprintf(stderr,"\t-S                : Assume coincident satellite overpass ,ie,\n");
	fprintf(stderr,"\t                    write VOS into 1C-51.HDF w/o sat_file check.\n");
	fprintf(stderr,"\t-t satellite_files_dir: Default: $GVS_DATA_PATH . Location of\n");
	fprintf(stderr,"\t                        daily satellite coincidence tables.\n");
	fprintf(stderr,"\t-----------------------------------------------------\n");
	fprintf(stderr,"\tIMAGE CREATION\n");
	fprintf(stderr,"\t-g gif_image_output_dir : Default: No IMAGE.GIF product\n");
	fprintf(stderr,"\t-p pgm_image_output_dir : Default: No IMAGE.PGM product\n");
	fprintf(stderr,"\t-n Number_of_sweeps to make images of: Default: 1\n");
	fprintf(stderr,"\t-M Send images to 'monitorps' via 'send_product'\n");
	fprintf(stderr,"\t   You must use '-g .' for this to work.\n");
	fprintf(stderr,"\t-----------------------------------------------------\n");
	fprintf(stderr,"\tQC USING VR (VELOCITY) MASK\n");
	fprintf(stderr,"\t-V {1 or 2} :    Default: No VR masking\n");
	fprintf(stderr,"\t    1: Mask Z values where VR values are invalid\n");
	fprintf(stderr,"\t    2: Mask Z values where VR values are invalid or zero\n");
	fprintf(stderr,"\t-----------------------------------------------------\n");
	fprintf(stderr,"\tTHE FOLLOWING OPTIONS OVERRIDE DEFAULT QC PARAMETERS\n");
	fprintf(stderr,"\tFOR 1C-51 AND 1C-UF PRODUCTION. (Rosenfeld algorithm)\n");
	fprintf(stderr,"\t-----------------------------------------------------\n");
	fprintf(stderr,"\t-0 hThresh1 : Cappi height (km)\n");
	fprintf(stderr,"\t-1 hThresh2 : Minimum cloud height (km)\n");
	fprintf(stderr,"\t-2 hThresh3 : Max height QC search (km)\n");
	fprintf(stderr,"\t-3 zThresh0 : Min Zmax @ 1.5 km (dBz)\n");
	fprintf(stderr,"\t-4 zThresh1 : Min Zmax @ 3.0 km (dBz)\n");
	fprintf(stderr,"\t-5 zThresh2 : Min Z @ lowest tilt (dBZ)\n");
	fprintf(stderr,"\t-6 zThresh3 : Min Zmax @ hThresh1 (dBZ)\n");
	fprintf(stderr,"\t-7 hFreeze  : Freezing height (km)\n");
	fprintf(stderr,"\t-8 dbzNoise : dBz noise level\n");
	fprintf(stderr,"\t-9 zCalibr  : Reflectivity Calibr constant\n\n");
	exit(ABORT);
}

/*************************************************************/
/*                                                           */
/*                       clean_up                            */
/*                                                           */
/*************************************************************/
void clean_up()
{
  if (strlen(HDF_1B_filename) > 0) remove (HDF_1B_filename);
  if (strlen(HDF_1C_filename) > 0) remove (HDF_1C_filename);
  if (strlen(UF_1C_filename) > 0) remove (UF_1C_filename);
}

/*************************************************************/
/*                                                           */
/*                       doexit_handler                      */
/*                                                           */
/*************************************************************/
void doexit_handler(int sig)
{
/*
  pid_t grpid;
  int i;
*/
  /* Restore STDERR and STDOUT.  This is required since both stderr and stdout
   * were redirected in the calling program, all_tape_level_1n2.
   */
  close(1);
  close(2);
  fopen("/dev/tty", "w");
  fopen("/dev/tty", "w");
  clean_up();
  fprintf(stderr, "%s: Got signal <%d>. Bye.\n", thisprog, sig);
  fflush(0);
  if (sig == SIGINT || sig == SIGKILL || sig == SIGSTOP) exit(INTER);
  else exit(ABORT);
}

extern int getopt(int argc, char * const argv[], const char *optstring);
/*************************************************************/
/*                                                           */
/*                         main                              */
/*                                                           */
/*************************************************************/
int
#if defined(__linux) || defined (__sgi) || defined(__hpux)
 main
#else
__main
#endif
(int argc, char **argv)
{
/* 
	 Checks content and validity of the command line which invoked this
	 function. If valid, initiates processing required to generate a
	 1B-51 HDF file, 1C-51 HDF file, and/or a UF file.

	 Returns:
        OK     0  : Nominal termination with product produced.
        ABORT -1  : PANIC exit -> No product produced.
        QUIT  -3  : Anomalous_condition exit -> No product produced.
*/
	char outpath_1B51[TK_MAX_FILENAME], outpath_1C51[TK_MAX_FILENAME];
	char outpath_uf[TK_MAX_FILENAME];   /* uf file path */
	char outpath_gif[TK_MAX_FILENAME];  /* gif image path */
	char outpath_pgm[TK_MAX_FILENAME];  /* pgm image path */
	char qcFile[TK_MAX_FILENAME], satFilePath[TK_MAX_FILENAME];
	char *clutter_map_fname = NULL;
	char callid_or_firstfile[TK_MAX_FILENAME];
	char siteName[8];
	char *gvs_env, *infile, *inProduct;
	int c, filetype, minimumSweeps;
	int nvos, vosNum;
	int status=-1;
	int product=0;
	int nsweeps=1;
	int doSatCheck=1;  /* default: do a satellite coincidence check for 1C-51.HDF */
	int use_default_qcfile = 1;
	int use_default_satfile = 1;
	int use_clutter_map = 0;
	int vrMask = 0;   /* 0: Do not QC using velocity mask */
	                  /* 1: Mask bins where velocity is invalid */
	                  /* 2: Mask bins where velocity is invalid or zero */
	float qcParm[NUMBER_QC_PARAMS];
	Radar *radar;
	extern char *optarg;
	extern int optind;

	if (argc < 2) usage();

	thisprog = argv[0];
	memset(HDF_1B_filename, '\0', TK_MAX_FILENAME);
	memset(HDF_1C_filename, '\0', TK_MAX_FILENAME);
	memset(UF_1C_filename, '\0', TK_MAX_FILENAME);
	memset(siteName, '\0', sizeof(siteName));
	
	/* Set signals to trap */
	signal(SIGINT, doexit_handler);
	signal(SIGSTOP, doexit_handler);
	signal(SIGKILL, doexit_handler);
	signal(SIGFPE, doexit_handler);
	signal(SIGILL, doexit_handler);
	signal(SIGSEGV, doexit_handler);

	/* Get default directory pathname for qc_files and satellite_pass_files.*/
	gvs_env = (char *)getenv("GVS_DATA_PATH");
	if (gvs_env == NULL)
	{
		fprintf(stderr, "\nUninitialized Environment Variable 'GVS_DATA_PATH'\n\n");
		exit(ABORT);
	}

	/* Initialize qcParm table. */
	for (c=0; c<NUMBER_QC_PARAMS; c++)
	  qcParm[c] = NOVAL_FLOAT;

	/* Set up defaults . Overridden by arguments from the command line. */
	RSL_radar_verbose_off();
	product = product | (B_PRODUCT | C_PRODUCT); /* Do 1B-51 & 1C-51 files.*/
	strcpy(outpath_1B51, "./");          /* 1B-51.HDF file path */
	strcpy(outpath_1C51, "./");          /* 1C-51.HDF file path */
	strcpy(outpath_uf, "./");            /* 1C-51.UF file path */
	strcpy(outpath_gif, "./");           /* gif image file path */
	strcpy(outpath_pgm, "./");           /* pgm image file path */
	strcpy(callid_or_firstfile, "");     /* Tape header file for wsr88d */
	minimumSweeps = MINIMUM_SWEEPS; /* Min sweeps needed to do 1C-51. */
	q_monitor_products = 0;
	B_product_version_str = (char *)strdup(B_PRODUCT_VERSION);
	C_product_version_str = (char *)strdup(C_PRODUCT_VERSION);

	/* Read options from command line. */
	while ((c=getopt(argc, argv, "MvkP:Q:u:b:Bc:Cq:St:m:n:g:p:f:V:0:1:2:3:4:5:6:7:8:9:")) != EOF)
	{
		switch (c)
		{
		case 'v':
			verbose = 1;
			RSL_radar_verbose_on();
			break;
		case 'P':
		  B_product_version_str = (char *)strdup(optarg);
		  break;
		case 'Q':
		  C_product_version_str = (char *)strdup(optarg);
		  break;
		case 'M': q_monitor_products = 1;
			break;
		case 'm':
			minimumSweeps = atoi(optarg);
			break;
		case 'b':
			strncpy(outpath_1B51, optarg, TK_MAX_FILENAME-25);
			strcat(outpath_1B51, "/");
			break;
		case 'B':           /* No 1B-51 (HDF) product desired. */
			product = product & ~B_PRODUCT;
			break;
		case 'c':           
			strncpy(outpath_1C51, optarg, TK_MAX_FILENAME-25);
			strcat(outpath_1C51, "/");
			break;
		case 'C':           /* No 1C-51 (HDF) product desired. */
			product = product & ~C_PRODUCT;
			break;
		case 'u':           /* 1C-51 UF product desired. */
			product = product | UF_PRODUCT;
			strncpy(outpath_uf, optarg, TK_MAX_FILENAME-25);
			strcat(outpath_uf, "/");
			break;
		case 'g':           /* gif image product desired. */
			product = product | GIF_PRODUCT;
			strncpy(outpath_gif, optarg, TK_MAX_FILENAME-25);
			strcat(outpath_gif, "/");
			break;
		case 'k': /* University of Washington cluttermap */
		        use_clutter_map = 1;
		        break;
		case 'p':           /* pgm image product desired. */
			product = product | PGM_PRODUCT;
			strncpy(outpath_pgm, optarg, TK_MAX_FILENAME-25);
			strcat(outpath_pgm, "/");
			break;
		case 'n':
			nsweeps = atoi(optarg);  /* Number_of_sweeps to make images of. */
			break;
		case 'q':
			use_default_qcfile = 0;
			strncpy(qcFile, optarg, TK_MAX_FILENAME-1);
			break;
		case 'S':
			doSatCheck = 0;  /* No satellite coincidence check for 1C-51.HDF */
			break;
		case 't':
			use_default_satfile = 0;
			strncpy(satFilePath, optarg, TK_MAX_FILENAME-1);
			break;
		case 'f':
			strncpy(callid_or_firstfile, optarg, TK_MAX_FILENAME-1);
			break;
		case 'V':
		  vrMask = atoi(optarg);
		  if (vrMask < 1  ||  vrMask > 2)
		  {
			fprintf(stderr, "\nInvalid specification for option -V\n\n");
			usage();
		  }
		  break;
		case '0':
			qcParm[HTHRESH1] = atof(optarg);  break;
		case '1':
			qcParm[HTHRESH2] = atof(optarg);  break;
		case '2':
			qcParm[HTHRESH3] = atof(optarg);  break;
		case '3':
			qcParm[ZTHRESH0] = atof(optarg);  break;
		case '4':
			qcParm[ZTHRESH1] = atof(optarg);  break;
		case '5':
			qcParm[ZTHRESH2] = atof(optarg);  break;
		case '6':
			qcParm[ZTHRESH3] = atof(optarg);  break;
		case '7':
			qcParm[HFREEZE] = atof(optarg);   break;
		case '8':
			qcParm[DBZNOISE] = atof(optarg);  break;
		case '9':
			qcParm[ZCAL] = atof(optarg);      break;
		default:
			fprintf(stderr, "Option <%c> is not valid\n", c);
			usage();
		}     /* end switch */
	}  /* end while */

	/* Check for one remaining item ('infile') in the command line. */
	if ((argc - optind) != 1)
	{
		fprintf(stderr, "\nMust specify an input data file.\n\n");
		exit(ABORT);
	}
	/* Check that a product type was specified. */
	if (product == 0)
	{
		fprintf(stderr, "\nMust specify a desired output data product.\n\n");
	  exit(ABORT);
	}
	infile = argv[optind];

	/* Read from the data file only those data fields required for
		 1B-51/1C-51 products. */
	RSL_radar_verbose_off();
	RSL_select_fields("DZ", "ZT", "CZ", "ZD", "DR", "CD", "VR", NULL);
	if (verbose) RSL_radar_verbose_on();

	/* Peek at the contents of the input file to determine its format. */
	filetype = (int) RSL_filetype(infile);
	if (filetype == WSR88D_FILE)
	  if (strlen(callid_or_firstfile) == 0)
		{
			fprintf(stderr, "\nMust include '-f' option for WSR-88D files.\n\n");
			exit(ABORT);
		}
	if (filetype == HDF_FILE)
	{
		nvos = MAX_VOS;  /* HDF has up to 'MAX_VOS' VOSs per file. */
		if (doSatCheck)
		{
			/* If the inFile is 1C-51.HDF, we're reprocessing. Satellite
				 coincidence checks were done when the infile was created, so don't
				 repeat them here.
				 Get the first 4 chars of the inFilename. */
			inProduct = strrchr(infile, '/');
			if (inProduct == NULL) inProduct = infile;
			else inProduct = (char *)(inProduct + 1);
			if (strncmp(inProduct, "1C51", 4) == 0) doSatCheck = 0;
		} /* end if (doSatCheck) */
	} /* end if (filetype == HDF_FILE) */
	else
	{
		nvos = 1;  /* Non-HDF formats support only 1 VOS per file. */
	}

	/* The following for-loop reads all the VOSs out of the infile.
		 There may exist up to 'MAX_VOS' VOSs in an HDF file. */
	radar = (Radar *)NULL;
	for (vosNum=0; vosNum<nvos; vosNum++)
	{
		if (radar != NULL) RSL_free_radar(radar);
		qc_done = 0;
		if (filetype == HDF_FILE)
		{
	    radar = (Radar *)RSL_hdf_to_radar(infile);
			if (radar == NULL) exit(OK);
		}
		else /* Not HDF file. */
		{
			if (verbose)
			{
				fprintf(stderr, "\ninFile = %s\n", infile);
				fprintf(stderr, "****** Moving VOS from disk file -> RSL structure ...\n");
			}
			/* Ingest routines are too verbose for purposes here. */
			RSL_radar_verbose_off();
			radar = (Radar *)RSL_anyformat_to_radar(infile, callid_or_firstfile);
			/* SIGMET data may have a file header (that loaded into Radar->h) 
			 * containing time information that is slightly earlier than the
			 * time recorded for the first ray -- upwards of 20 seconds.
			 * In other words, the minute may be different on occasion.
			 * So, use an undocumented rsl routine to load the time information
			 * from the first ray.  Maybe other file formats have this
			 * problem too.
			 */
			if (verbose) {
			  fprintf(stderr, "Fix radar header time with time from the first ray.\n");
			  fprintf(stderr, "radar->h.minute before = %d\n", radar->h.minute);
			}
			radar_load_date_time(radar);
			if (verbose) {
			  fprintf(stderr, "radar->h.minute  after = %d\n", radar->h.minute);
			}
			if (verbose) RSL_radar_verbose_on();
			if (radar == NULL)
			{
			  if (verbose)
				  fprintf(stderr, "%s: ***** Error loading RSL structure\n", argv[0]);
				exit(ABORT);
			}
		} /* end else Not HDF file */
		
		status = vosValidityCheck(radar);
		if (status < 0) continue;

		/* If ZT field exists, replace the DZ field with ZT. 
		   ZT is the preferred raw reflectivity field. */
		if (radar->v[ZT_INDEX] != NULL)
		{
			/* If DZ field exists, get rid of it. */
			if (radar->v[DZ_INDEX] != NULL)
			  RSL_free_volume(radar->v[DZ_INDEX]);
			radar->v[DZ_INDEX] = radar->v[ZT_INDEX];
			radar->v[ZT_INDEX] = NULL;
		}

		/* Differential Reflectivity.
			 Use ZD as the preferred differential reflectivity field.
			 If no ZD field, but DR field exists, rename DR as ZD. */
		if (radar->v[DR_INDEX] != NULL)
		{
			if (radar->v[ZD_INDEX] == NULL)
				radar->v[ZD_INDEX] = radar->v[DR_INDEX];
			else
			  RSL_free_volume(radar->v[DR_INDEX]);
			radar->v[DR_INDEX] = NULL;
		}

		/* Identify the radar site. */
		status = radarSiteCheck(radar, siteName);
		if (status < 0) continue;

		if (use_default_satfile) strcpy(satFilePath, gvs_env);
		if (use_default_qcfile)
		{
			strcpy(qcFile, gvs_env);
			strcat(qcFile, "/qc_params_");
			strcat(qcFile, siteName);
		}

		if (use_clutter_map) {
		  clutter_map_fname = (char*) calloc(TK_MAX_FILENAME, sizeof(char));

		  strcpy(clutter_map_fname, gvs_env);
		  strcat(clutter_map_fname, "/");
		  strcat(clutter_map_fname, siteName);
		  strcat(clutter_map_fname, ".cluttermap");
		}

		/* If 1B-51.HDF product is required, create it. */
		if (product & B_PRODUCT)
		{
			if (verbose)
			{
				fprintf(stderr, "\n\n**********************************************************\n");
				fprintf(stderr, "**********************************************************\n");
				fprintf(stderr, "      1B-51 (HDF) Processing  ...\n");
				fprintf(stderr, "**********************************************************\n");
				fprintf(stderr, "**********************************************************\n");
			}
			status = hdf1B51Create(radar, outpath_1B51, siteName);
			if (status < 0) continue;
		} /* end if (product & B_PRODUCT) */

		/* If a 1C-51.HDF, UF, GIF, or PGM product is desired, set up QC parameters. */
		if (product & (C_PRODUCT | UF_PRODUCT | GIF_PRODUCT | PGM_PRODUCT))
		{
			/* Initialize the array of QC parameters based on radar site and
				 VOS date/time. Those QC parameters not set via command line 
				 options are taken from the QC defaults table in file 'qcFile'. */
			status = qcParmInit(qcParm, qcFile, radar);
			if (status < 0) goto quit;
			/* Store the reflectivity calibration constant into appropriate rsl
				 volume headers. */
			if (radar->v[DZ_INDEX] != NULL)
	      radar->v[DZ_INDEX]->h.calibr_const = qcParm[ZCAL];
			if (radar->v[CZ_INDEX] != NULL)
	      radar->v[CZ_INDEX]->h.calibr_const = qcParm[ZCAL];
		} /* end if (product & (C_PRODUCT | ...)) */
		
		/* If 1C-51.HDF product is required, create it. */
		if (product & C_PRODUCT)
		{
			if (verbose)
			{
				fprintf(stderr, "\n\n**********************************************************\n");
				fprintf(stderr, "**********************************************************\n");
				fprintf(stderr, "       1C-51 (HDF) Processing  ...\n");
				fprintf(stderr, "**********************************************************\n");
				fprintf(stderr, "**********************************************************\n\n");
			}
			status = hdf1C51Create(radar, outpath_1C51, siteName, qcParm, satFilePath,
								   doSatCheck, minimumSweeps, vrMask, clutter_map_fname);
			if (status < 0) continue;
		} /* end if (product & C_PRODUCT) */

		/* If 1C-UF product is required, create it.
			 Frees all volumes but reflectivity and velocity. Also truncates range at 200km.
		*/
		if (product & UF_PRODUCT)
		{
		  if (verbose)
			{
				fprintf(stderr, "\n\n**********************************************************\n");
				fprintf(stderr, "**********************************************************\n");
				fprintf(stderr, "       UF Processing  ...\n");
				fprintf(stderr, "**********************************************************\n");
				fprintf(stderr, "**********************************************************\n\n");
			}
			status = ufCreate(radar, outpath_uf, siteName, qcParm, minimumSweeps, vrMask, clutter_map_fname);
			if (status < 0) continue;
		} /* end if (product & UF_PRODUCT) */

		/* 
			 Create some images. Use the values Z1, Z2, Z3, H1, H2, H3 passed
			 on the command line or read from the param file. The output gif
			 filenames will be named as: SITEID.YYMMDD.HHMM.Z1Z2Z3.H1.H2.H3.gif
			 DESTROYS (ie, rebins) data values.
		*/
		if ( (product & GIF_PRODUCT) || (product & PGM_PRODUCT) )
		{
			if (verbose) 
			{
				fprintf(stderr, 
						"\n\n**********************************************************\n");
				fprintf(stderr, 
						"**********************************************************\n");
				fprintf(stderr, 
						"       Image Processing  ...\n");
				fprintf(stderr, 
						"**********************************************************\n");
				fprintf(stderr, 
						"**********************************************************\n\n");
			}
			status = ImgCreate(radar, outpath_gif, outpath_pgm, siteName,
							   qcParm, nsweeps, minimumSweeps, vrMask, product, clutter_map_fname);
			if (status < 0) continue;
		} /* end if ( (product & GIF_PRODUCT)... */
	} /* end for (vosNum=0; vosNum<nvos; vosNum++) */
	

 quit:
	if (verbose) {
	  if (status == ABORT)
		fprintf(stderr, "%s: PANIC exit -> No product produced.\n\n", argv[0]);
	  else if (status == QUIT) 
		fprintf(stderr, "%s: Anomalous_condition exit -> No product produced.\n\n", argv[0]);
	  else if (status == INTER) 
		fprintf(stderr, "%s: Abort execution. Received signal INT, KILL, or STOP.\n", argv[0]);
		else if (status == SHUTDOWN)
		fprintf(stderr, "%s: Total shutdown -> No product produced.\n\n", argv[0]);
	  else
		fprintf(stderr, "%s: Successful.\n\n", argv[0]);
	}

	exit(status);
}


 
#ifdef __hpux
int __main(int ac, char **av)
{
/* Do nothing. GCC on HP had a hard time and produced the symbol
 * __main for this file alone.  Why?
 */
  return 0;
}
#endif

