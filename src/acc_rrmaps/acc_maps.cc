// Driver for AIW2 accumulation of maps
// given a list of files and a dimension will
// output an array that is the sum of all those files
// handles missing data inside addABArrays
//
// SEY 21 August 1996

// Exit Code:
//     -1: Failure
//      0: Successful
//     -2: Interrupted

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include "defs.h"


#ifdef SGI53
void handler(...)
#else
void handler(int sig)
#endif

{

#ifdef SGI53
  fprintf(stderr, "Got signal. Abort.\n");
#else

  fprintf(stderr, "Got signal %d. Abort.\n", sig);
#endif
  kill(0, SIGKILL);
  exit(-1);
}

#ifdef SGI53
void abort_handler(...)
#else
void abort_handler(int sig)
#endif

{
#ifdef SGI53
  fprintf(stderr, "Interrupted. Abort.\n");
#else
  fprintf(stderr, "Got signal %d. Abort.\n", sig);
#endif
  kill(0, SIGINT);
  exit(-2);
}
int main(int argc, char **argv)
{
int verbose = 0;
char filelistfn[MAX_LINE];
char pathprefix[MAX_LINE+100];
char fullpath[MAX_LINE+100];
char outputfn[MAX_LINE];
char map_fn[MAX_LINE];
int arsize, afile, i;
double acc_multfactor;
FILE* fpmap; // reused pointer to a mapfile 

signal(SIGINT, abort_handler);
signal(SIGFPE, handler);
signal(SIGKILL, abort_handler);
signal(SIGSTOP, abort_handler);
signal(SIGILL, handler);
signal(SIGSEGV, handler);

if (argc < 6) {
USAGE:
  fprintf(stderr,
	    "usage: [-v] acc_maps filelistfilename outputfilename arraysize multfactor prefixpath\n");
    exit(-1);
}
i = 1;
if (strcmp(argv[i], "-v") == 0) {
  verbose = 1;
  if (argc != 7)
	goto USAGE;
  i++;
}
strcpy(filelistfn,argv[i++]);
strcpy(outputfn,argv[i++]);
arsize = atoi(argv[i++]);
acc_multfactor = atof(argv[i++]);
strcpy(pathprefix,argv[i++]);
if (verbose)
  fprintf(stderr,"arraysize is %d,multfactor is %f\n",
		  arsize,acc_multfactor);

// declare the array, but has to be 1d
float* maparray = new float[arsize * arsize];
float* acc_array = new float[arsize * arsize];
float* multmap_array = new float[arsize * arsize];


FILE *fplist = fopen(filelistfn, "r");
FILE *fpout  = fopen(outputfn, "w");

if (!fplist) {
    fprintf(stderr,"Error opening file %s\n",filelistfn);
	fclose(fpout);
	exit(-1);
} 

// initialize acc_array, treat as 1d for this purpose
    for (i=0;i<arsize*arsize;i++){
        acc_array[i]=0.0;
		maparray[i] = 0.0;
		acc_array[i] = 0.0;
    }
for (afile=1;afile<MAX_FILES;afile++){
    memset(map_fn,0,sizeof(map_fn));
    memset(fullpath,0,sizeof(fullpath));
    if (fscanf(fplist,"%s",map_fn) !=EOF) {

	if (verbose)
	    fprintf(stderr,"%dth file is %s\n",afile,map_fn);
	strcpy(fullpath,pathprefix);
	strcat(fullpath,map_fn);
	if (strlen(pathprefix)!=0) {
	    if (verbose)
	        fprintf(stderr,"with PREFIX %dth file is %s\n",afile,fullpath);
	}
	fpmap = fopen(fullpath, "r");// was map_fn
	if (fpmap) {
	readInArray(fpmap,maparray,arsize);
	scalarMultArray(maparray,multmap_array,arsize,acc_multfactor); // do all the time
	//writeOutArray(multmap_array,arsize);
	addABArrays(multmap_array,acc_array,acc_array,arsize);
	fclose(fpmap);
    }
	else {
	    if (verbose)
	       fprintf(stderr,"Error opening file %s\n",map_fn);
	    FILE* fpdiag = fopen(MASTER_DIAG_FILE,"a+");
	    char timestr[13];
		memset(timestr, '\0', sizeof(timestr));
	    getCurrentGMTDate(timestr);
	    fprintf(fpdiag,"%s in ACC_MAPS Error opening file %s\n",
		    timestr,map_fn);
	    fclose(fpdiag);
	    //return(1);
	}
    }}

outputTRMMArray(fpout,acc_array,arsize); 

fclose(fplist);
fclose(fpout);
delete maparray;
delete multmap_array;
delete acc_array;
exit(0);
} // end main 
