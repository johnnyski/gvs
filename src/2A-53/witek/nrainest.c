#include "rainest.h"

char   rrmap_fname[256];
char   input_fname[256];
int    icounter,iray;
int    latd, latm, lats, lond, lonm, lons;
int    yearp,monthp,dayp,minp,hourp, secp;
int    verbose;
int    day5d,y5d,mon5d;
int    daytot,ytot,montot;
int    iflag;
int    ipass;
int    countday;
int    counttot;
int    indexmp; 
int    indexcl;
int    radar_index;    
int    shiftmin;
int    categ[360][221]; 
int    map[CLASS_NX][CLASS_NY];
int    elvmx;
int    wn_search,wn_steps,wflag;
double wt1[151][151],wt2[151][151],wt3[151][151];
float  Zthre,As,Bs,Ass,Bss,ac,bc,Acc,Bcc;
float  htopc,htops,hbots,hbotc;
float  arc,brc,ars,brs;
float  value[2000],range_res;
float  Rp[360][221],Rpm[360][221],Rph[152][152],Rpd[152][152];
float  Rp5d[152][152],Rpt[152][152];
float  Npm[360][221],Nph[152][152];
float  Rc[155][155];
char   carttopol_tbl[100],poltocart_tbl[100],height_tbl[100];

sweep_header_t sweep;
ray_header_t   ray; 

#include  <unistd.h>

void usage(char *prog)
{
  fprintf(stderr, "Usage: %s [-v] [-P pathname_for_lookup_tables] param_file infile outfile\n", prog);
  exit(-1);
}

void main(int argc, char **argv)
/* 
   This program produces rainfall-rate maps in cartesian coordinates based on
   volume radar reflectivity data.
   It is based on The University of Iowa team's radar-rainfall estimation algorithm.
   Author: Emmanouil N. Anagnostou, Iowa Institute of Hydraulic Research,
           The University of Iowa, 1994.
*/
{
  extern void trap_signals(void (*handler)(int));
  extern void handler(int sig);
  FILE  *fpp;
  char  data_path[150],dummy[100];
  char  param_fname[256];
  int   c;
  extern char *optarg;
  extern int optind,optopt;

  /* trap signals to exit with the right code */
  trap_signals(handler);

  strcpy(data_path, "./");
  while ((c = getopt(argc,argv,"P:v"))!=-1){
	switch (c) {
	case 'v': verbose = 1; break;
	case 'P': strcpy(data_path, optarg); break;
    case '?': fprintf(stderr, "option -%c is undefined\n", optopt);
          usage(argv[0]);
    case ':': fprintf(stderr, "option -%c requires an argument\n",optopt);
          usage(argv[0]);
    default: break;
    }

  }

  if ( argc - optind != 3)  
    usage(argv[0]);
  strcpy(param_fname, argv[optind++]);
  strcpy(input_fname, argv[optind++]);
  strcpy(rrmap_fname, argv[optind++]);

  if (verbose) {
	fprintf(stderr, "%s: param_fname = <%s>\n", argv[0], param_fname);
	fprintf(stderr, "%s: input_fname = <%s>\n", argv[0], input_fname);
	fprintf(stderr, "%s: rrmap_fname = <%s>\n", argv[0], rrmap_fname);
	fprintf(stderr, "%s: data_path = <%s>\n", argv[0], data_path);
  }

  /* Open the parameter file */
  if ((fpp=fopen(param_fname,"r"))==NULL)
    {
	  fprintf(stderr,"ERROR: could not open <%s>\n", param_fname);
	  exit(-1);
    }
  fscanf(fpp,"%s",dummy);                              /* Radar site month and year                       */
  fscanf(fpp,"%f %s %s",&Zthre,dummy,dummy);           /* Reflectivity threshold (dBZ)                    */
  fscanf(fpp,"%d %s %s",&shiftmin,dummy,dummy);        /* Time shift with raingages (minutes)             */
  fscanf(fpp,"%d %s %s",&indexmp,dummy,dummy);         /* inst. map production index:
                                                          1: only instantaneus (5-minutes) maps
                                                          2: instantaneus/hour/day/5days/month maps
	                                                  3: hour/day/5days/month maps                    */
  fscanf(fpp,"%d %s %s",&indexcl,dummy,dummy);         /* inst. map classification index (on:1/off:2)     */
  fscanf(fpp,"%d %s %s",&wflag,dummy,dummy);           /* advection correction index     (on:1/off:0)     */
  fscanf(fpp,"%d %s %s",&wn_search,dummy,dummy);       /* advection correction parameter (common value 4) */
  fscanf(fpp,"%d %s %s",&wn_steps,dummy,dummy);        /* advection correction parameter (common value 5) */
  fscanf(fpp,"%f %s %s",&As,dummy,dummy);              /* Stratiform Z-R parameter A                      */
  fscanf(fpp,"%f %s %s",&Bs,dummy,dummy);              /* Stratiform Z-R parameter B                      */
  fscanf(fpp,"%f %s %s",&ars,dummy,dummy);             /* range dependent coefficient for As              */
  fscanf(fpp,"%f %s %s",&brs,dummy,dummy);             /* range dependent coefficient for Bs              */
  fscanf(fpp,"%f %s %s",&ac,dummy,dummy);              /* Convective Ac parameter multiplier: Ac=ac*As    */
  fscanf(fpp,"%f %s %s",&bc,dummy,dummy);              /* Convective Bc parameter multiplier: Bc=bc*Bs    */
  fscanf(fpp,"%f %s %s",&arc,dummy,dummy);             /* range dependent coefficient for Ac              */
  fscanf(fpp,"%f %s %s",&brc,dummy,dummy);             /* range dependent coefficient for Bc              */
  fscanf(fpp,"%f %s %s",&htopc,dummy,dummy);           /* Upper integration limit for convective cells    */
  fscanf(fpp,"%f %s %s",&htops,dummy,dummy);           /* Upper integration limit for stratiform cells    */
  fscanf(fpp,"%f %s %s",&hbotc,dummy,dummy);           /* Lower integration limit for convective cells    */
  fscanf(fpp,"%f %s %s",&hbots,dummy,dummy);           /* Lower integration limit for stratiform cells    */
  fscanf(fpp,"%d %s %s",&elvmx,dummy,dummy);           /* Maximum number of sweeps                        */
  fscanf(fpp,"%f %s %s",&range_res,dummy,dummy);       /* Radar bins range discretization (km): Darwin    */
                                                       /* (0.3 km), Melbourne Florida (1.0 km)            */
  fscanf(fpp,"%d %s %s",&radar_index,dummy,dummy);     /* Radar site: (1) Melbourne (FL), (2) Darwin      */
  fclose(fpp);

  if(radar_index == 1)
    { 
    sprintf(carttopol_tbl,"%s/carttopol.FL", data_path);
    sprintf(poltocart_tbl,"%s/poltocart.FL", data_path);
    }
  else
    {
    sprintf(carttopol_tbl,"%s/carttopol.DR", data_path);
    sprintf(poltocart_tbl,"%s/poltocart.DR", data_path);
    }

  settozero();                       /* Set to zero the matrices  */

  products();                        /* Create the instantaneus map */

  /* End of program */
  exit(0);
}



