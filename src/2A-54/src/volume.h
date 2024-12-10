#ifndef VOLUME_H
#define VOLUME_H

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <mudras.h>

// The missing value.
#define MISSINGV        -99

// Maximum rainrate allowed [mm/h].
#define MAX_RAINRATE 1000.0

// Dimensions for Cartesian-gridded, three-dimensional radar volume:
#define MAX_X_DIM       160               // maximum x-array dimension
#define MAX_Y_DIM       160               // maximum y-array dimension
#define MAX_Z_DIM        17               // maximum z-array dimension

// Dimensions used for reading the raingauge site information:
#define MAX_LINE        132               // maximum line length
#define MAX_SITES       200               // number of raingauge sites
#define MAX_NAME         20               // maximum length of site name

// Dimension for geographical map:
#define MAP_GRID        241               // grid dimension for map

// Dimension for Z-R conversion file:
#define MAX_FILE        500               // maximum lines of WPMM Z-R file

// Number of domain categories (land, ocean, etc.) including anvil
#define NUM_CATEGORIES   12

// Number of domain categories (land, ocean, etc.) excluding anvil
#define NUM_RAIN_CATEGORIES    9

#define MAX_CLASS          90             // Maximum number of CFAD classes.
#define RANGE_CLASS         8             // Number of range intervals
#define ZMIN_BIN        -15.0             // minimum refl for histogram
#define ZMAX_BIN         70.0             // maximum refl for histogram
#define RANGE_RES        20.0             // range bin size

// Analysis indices.
#define ANAL_ECHO_AREA      0
#define ANAL_MEAN_PROF      1
#define ANAL_HIST_CFAD      2
#define ANAL_CFAD_RANGE     3
#define ANAL_RAIN_MAP       4
#define ANAL_GAUGESITES     5
#define ANAL_AREAL_RAIN     6
#define ANAL_CONV_STRA      7
#define NUM_ANAL_INDICES    8             // array size for analysis flags

// Single-volume data structures.
class EchoArea
{
public:
  int conv_pixel_land;                  // counts conv pixels over land
  int conv_pixel_sea;                   // counts conv pixels over ocean
  int stra_pixel_land;                  // counts stra pixels over land
  int stra_pixel_sea;                   // counts stra pixels over ocean
  int dry_pixel_land;                   // counts no-rain pixels over land
  int dry_pixel_sea;                    // counts no-rain pixels over ocean
  int total_pixel;                      // total pixel number in domain
  int total_pixel_land;                 // total pixel number over land
  int total_pixel_sea;                  // total pixel number over ocean
  int total_pixel_conv;                 // total conv pixel number
  int total_pixel_stra;                 // total stra pixel number
  int total_pixel_dry;                  // total no-rain pixel number
};

class ArealRain
{
public:
  int count_Rint_total;                 // counts pixels, entire area      
  int count_Rint_land;                  // counts pixels, land             
  int count_Rint_sea;                   // counts pixels, ocean            
  int count_Rint_conv;                  // counts pixels, conv area        
  int count_Rint_conv_land;             // counts pixels, conv area land   
  int count_Rint_conv_sea;              // counts pixels, conv area ocean  
  int count_Rint_stra;                  // counts pixels, stra area        
  int count_Rint_stra_land;             // counts pixels, stra area land   
  int count_Rint_stra_sea;              // counts pixels, stra area ocean  
  float aver_Rint_total;                // mean rain rate, entire area     
  float aver_Rint_land;                 // mean rain rate, land            
  float aver_Rint_sea;                  // mean rain rate, ocean           
  float aver_Rint_conv;                 // mean rain rate, conv area       
  float aver_Rint_conv_land;            // mean rain rate, conv area land  
  float aver_Rint_conv_sea;             // mean rain rate, conv area ocean 
  float aver_Rint_stra;                 // mean rain rate, stra area       
  float aver_Rint_stra_land;            // mean rain rate, stra area land  
  float aver_Rint_stra_sea;             // mean rain rate, stra area ocean 
};

class CFAD
{
public:
  int max_Zindex;

  int   count_Zhist_tot[MAX_Z_DIM][MAX_CLASS];      
  int   count_Zhist_land[MAX_Z_DIM][MAX_CLASS];     
  int   count_Zhist_sea[MAX_Z_DIM][MAX_CLASS];      
  int   count_Zhist_conv[MAX_Z_DIM][MAX_CLASS];     
  int   count_Zhist_conv_land[MAX_Z_DIM][MAX_CLASS];
  int   count_Zhist_conv_sea[MAX_Z_DIM][MAX_CLASS];
  int   count_Zhist_stra[MAX_Z_DIM][MAX_CLASS];   
  int   count_Zhist_stra_land[MAX_Z_DIM][MAX_CLASS];
  int   count_Zhist_stra_sea[MAX_Z_DIM][MAX_CLASS];
  int   count_Zhist_anvil[MAX_Z_DIM][MAX_CLASS];
  int   count_Zhist_anvil_land[MAX_Z_DIM][MAX_CLASS];
  int   count_Zhist_anvil_sea[MAX_Z_DIM][MAX_CLASS];

  // CFAD for each range bin.
  int   count_Zhist_range[RANGE_CLASS][MAX_Z_DIM][MAX_CLASS];
};

class MeanProfile
{
public:
  int   count_Zprof_tot[MAX_Z_DIM];        // counts Z-profile, total      
  int   count_Zprof_land[MAX_Z_DIM];       // counts Z-profile, land       
  int   count_Zprof_sea[MAX_Z_DIM];        // counts Z-profile, sea        
  int   count_Zprof_conv[MAX_Z_DIM];       // counts Z-profile, conv       
  int   count_Zprof_conv_land[MAX_Z_DIM];  // counts Z-profile, conv-land 
  int   count_Zprof_conv_sea[MAX_Z_DIM];   // counts Z-profile, conv-sea
  int   count_Zprof_stra[MAX_Z_DIM];       // counts Z-profile, stra       
  int   count_Zprof_stra_land[MAX_Z_DIM];  // counts Z-profile, stra-land  
  int   count_Zprof_stra_sea[MAX_Z_DIM];   // counts Z-profile, stra-sea
  int   count_Zprof_anvil[MAX_Z_DIM];      // counts Z-profile, anvil      
  int   count_Zprof_anvil_land[MAX_Z_DIM]; // counts Z-profile, anvil-land
  int   count_Zprof_anvil_sea[MAX_Z_DIM];  // counts Z-profile, anvil-sea 
  float mean_Zprof_tot[MAX_Z_DIM];         // mean Z-profile, total        
  float mean_Zprof_land[MAX_Z_DIM];        // mean Z-profile, land         
  float mean_Zprof_sea[MAX_Z_DIM];         // mean Z-profile, sea          
  float mean_Zprof_conv[MAX_Z_DIM];        // mean Z-profile, conv         
  float mean_Zprof_conv_land[MAX_Z_DIM];   // mean Z-profile, conv-land 
  float mean_Zprof_conv_sea[MAX_Z_DIM];    // mean Z-profile, conv-sea  
  float mean_Zprof_stra[MAX_Z_DIM];        // mean Z-profile, stra         
  float mean_Zprof_stra_land[MAX_Z_DIM];   // mean Z-profile, stra-land 
  float mean_Zprof_stra_sea[MAX_Z_DIM];    // mean Z-profile, stra-sea  
  float mean_Zprof_anvil[MAX_Z_DIM];       // mean Z-profile, anvil        
  float mean_Zprof_anvil_land[MAX_Z_DIM];  // mean Z-profile, anvil-land
  float mean_Zprof_anvil_sea[MAX_Z_DIM];   // mean Z-profile, anvil-sea 
};

class Site
{
public:
  float x;
  float y;
  int   dir;
  int   dist;
  char  name[MAX_NAME+1];
  char  shortName[5+1];
  float rain;
  float reflin;
  float Rint;
  int   rain_type;
};

class Grids
{
public:
  // 2-d maps.
  float  reflmap[MAX_Y_DIM][MAX_X_DIM];    // working map: refl [dBZ]      
  float  reflinmap[MAX_Y_DIM][MAX_X_DIM];  // working map: refl [mm6/m3]   
  float  Rint_map[MAX_Y_DIM][MAX_X_DIM];   // rain intensity map [mm/h]    
  int    geomap[MAP_GRID][MAP_GRID];       // geographical land/ocean mask    
  int    range_mask[MAX_Y_DIM][MAX_X_DIM]; // mask indicating "valid data"    
  int    rain_type[MAX_Y_DIM][MAX_X_DIM];  // map defining precipitation type
  float  bg_Zint[MAX_Y_DIM][MAX_X_DIM];    // background reflectivity [dBZ]

  // 3-d radar volume.
  float  reflog[MAX_Z_DIM][MAX_Y_DIM][MAX_X_DIM]; // 3D radar volume

  // reflog flag: 0 -> no reflog, 1 -> valid reflog
  int validReflog;
};

// Added by Joe Tenerelli Dec. 27, 1994.  This was not added to the Params
// class in order to preserve compatibility with previous version of
// software based upon classes defined in this include file.
extern char workingMapFilename[MAX_LINE];

class Params
{
public:
  int    isite;                            // number of raingauge sites  
  int    ZRtype;                           // type of Z-R conversion          
  int    ZR_WPMMdim;                       // number of (Z,R)-pairs for WPMM  
  float  ZR_a;                             // Z-R multiplicative factor       
  float  ZR_b;                             // Z-R power factor                
  float  ZR_a_conv;                        // Z-R multiplicative factor, conv 
  float  ZR_b_conv;                        // Z-R power factor, conv          
  float  ZR_a_stra;                        // Z-R multiplicative factor, stra 
  float  ZR_b_stra;                        // Z-R power factor, stra
  int    maxtype;                          // type of surface rain estimation 
  int    radar_gauge_type;                 // radar estimates at gauge site   
  float  max_radius;                       // radius of influence at gauge    
  float  Zvalue[MAX_FILE];                 // WPMM Z-R relation               
  float  Rvalue[MAX_FILE];                 // WPMM Z-R relation
  FILE  *fpout1;                           // general output file
  float  xymin;                            // minimum of grid (horizontal)    
  float  xymax;                            // maximum of grid (horizontal)    
  float  xyres;                            // resolution of grid (horizontal) 
  float  zmin;                             // minimum of grid (vertical)      
  float  zmax;                             // maximum of grid (vertical)      
  float  zres;                             // resolution of grid (vertical)
  int    xydim;                            // horizontal dimension of grid    
  int    zdim;                             // vertical dimension of grid      
  int    level;                            // level of data analysis
  float  min_range;                        // min range of valid radar data
  float  max_range;                        // max range of valid radar data
  int    year;
  int    month;
  int    day;
  int    time;
  int    hour;
  int    minute;
  float  lat, lon;                         // Lat/lon (deg*10000+min*100+sec).

  int    anal_ind[NUM_ANAL_INDICES];             // analysis flags
  int    anal_done[NUM_ANAL_INDICES];            // analysis flags

  // Convective/stratiform separation parameters.

  // Threshold, in dBZ, below which the rain type is stratiform.  Otherwise
  // the rain type is convective unless the reflectivity is missing, in
  // which case the rain type is no echo.
  float csSimpleSimonThreshold;

  // 0 = CH84 or SH94 scheme depending upon the resolution of the working map.
  // 1 = Simple Simon scheme.
  int   csAlgorithm;

  char   siteName[MAX_LINE];
  char   dateString[MAX_LINE];
  char   timeString[MAX_LINE];
  char   outputPath[MAX_LINE]; // Not used/loaded as of 6/10/96 - JHM.
  char   outputHDFfile[MAX_LINE]; // Used instead of 'outputPath'

  // Input filenames.
  char inputVolumeFn[MAX_LINE];
  char inputRainGaugeSitesFn[MAX_LINE];
  char inputGeoMapFn[MAX_LINE];
  char inputZRTranFn[MAX_LINE];
  char inputValidRangeFn[MAX_LINE];
  char inputThresholdFn[MAX_LINE];        // added 960125 SRB for 2A55 code

  // Output filenames.
  char outputGeneralFn[MAX_LINE];
  char outputConvStraMapFn[MAX_LINE];
  char outputMeanVertProfFn[MAX_LINE];
  char outputCFADFn[MAX_LINE];
  char outputCFADRangeFn[MAX_LINE];
  char outputRainIntFn[MAX_LINE];

  // format of radar data file used
  // 0 == CSU Mudras
  // 1 == CEDRIC Tape
  // 2 == CEDRIC Disk
  // 3 == Pure CEDRIC
  int radarFormat;

  // The number (starting from 0) of the volume in the radar volume file
  // to use in the analysis.  This is only meaningful when
  // radarFormat == 3.
  int nVolume;

  // Format to use for writing out results.
  // 0 == Bell format.
  // 1 == AIW format.
  int ASCIIformat;

  // Format to use for reading the map database.
  // 0 == ASCII format.
  // 1 == binary format.
  int mapFormat;
};

class Volume
{
public:
  // Lifecycle functions.
  Volume(void);
  Volume(int argc, char **argv);
  Volume(const char *fn);
  virtual ~Volume(void);

  // Analysis functions.
  int analyze(void);
  int ANALecho_area(void);
  int ANALmean_prof(void);
  int ANALhist_cfad(void);
  int ANALcfad_range(void);
  int ANALconv_stra(void);
  int ANALconv_straRandom(void);
  int ANALconv_straSimpleSimon(void);
  int ANALconv_straCH84orSH94(void);
  int ANALareal_rain(void);
  int ANALgaugesite(void);
  int ANALrain_map(void);

  // Preparation unctions.
  int PREPvalid_range(void);
  int PREPworking_map(void);

  // Input functions.
  int READparam(void);
  int READgeogr(void);
  int READgeogrNew(void);
  int READgauge(void);

  // Output functions.
  int writeASCII(void);
  int writeASCII2a53(void);
  int writeASCII2a54(void);
  int writeASCII2a55(void);
  int writeBinary(void);
  int writeBinaryAIW(FILE *fp);
  void output_hdf(char *qa_param_name, char *qa_param_value,
				  char *repro_stat, char *browse_name,
				  char *data_center_src, int verbose);

  // Input functions.
  int readRadarVolume(const int nfield = 0);
  int readBinary(const char *fn);
  int readBinaryAIW(FILE *fp);
  int readASCII(void);
  int readASCIIandFlip(void);

public:
  // General variables.
  Params     params;

  // Mean profile.
  MeanProfile prof;

  // CFAD profile.
  CFAD cfad;

  // Grids.
  Grids grids;

  // Echo area statistics.
  EchoArea echoArea;

  // Areal rainfall averages and counts.
  ArealRain arealRain;

  // Sites.    
  Site site[MAX_SITES];

  // Mudras object.
  MudFile mudfile;

  // Threshold filename.
  char thres_fn[MAX_LINE];
};


#endif







