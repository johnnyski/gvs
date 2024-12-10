#ifndef MUDHOUSE_H
#define MUDHOUSE_H

#include <stdio.h>

// housekeeping for a mudras file.
// Written by Dean Churchill, 12 May 87
// Variables that begin with letter U and are followed by a number
// refer to an unused word that is indexed by the given number. e.g.
// U95 indicates that word 95 of the header is unused.

typedef struct time
{
  short int Year;
  short int Month;
  short int Day;
  short int Hour;
  short int Minute;
  short int Second;
} TIME;

typedef struct point
{
  short int Deg;		// degrees of latitude or longitude ... 
  short int Min;
  short int Sec;		// x SF 
} COORDINATE;	

// descibes each of 3 axes 
typedef struct axis
{		
  short Min;		// minimum value of X, Y or Z (km * SF ) 
  short Max;		// maximum value (km * SF ) 
  short N;		// # of points on axis 
  short Spacing;	// spacing of points (m) along axis 
  short Index;		// 0, 1,2,3, or 4 
} AXIS;

typedef struct field
{
  char name[8];	        // name of field 
  short	scale;		// scaling factor for field 
} FIELDS;

typedef struct landmarks
{
  char  name[6];	// name of landmark 
  short x;		// x coordinate (km) 
  short y;		// y coordinate (km) 
  short z;		// z coordinate (m) 
} LANDMARKS;
 
typedef struct mudhouse
{
  char  FileName[8];    //                                          8
  char  Program[4];	// software program name                    4
  char  ProgVer[2];	// program version                          2
  char  Project[4];     //                                          4
  char  Scientist[6];   //                                          6
  char  RadarSite[6];	// or data origin                           6
  char  ScanMode[4];	// type of scan                             4
  char  Catalogue[6];	// tape catalogue id                        6
  TIME	Start;		// starting time of tape                   12 bytes
  TIME	Stop;		// stopping time of tape                   12 bytes
  COORDINATE Lat;       //                                          6 bytes
  COORDINATE Lon;	// latitude & longitude of origin 
  short	Height;		// height in meters above sea level 
  short Rotate;		// degrees clockwise from north to x-axis x CF 
  short X0;		// x cooridinate of horizontal axis origin x SF 
  short Y0;		// y coordinate
  char 	TimeZone[4];
  char  JobID[6];	// job submission ID 
  char  Submitter[6];	// person running job 
  char  RunDate[8];	// date program run 		
  char  RunTime[8];	// time program ran 
  short U59;		// word 59 is unused 
  short Edition;	// tape edition # 
  short HeadLength;	// length of header record (= 510 16-bit words) 
  char  CPUSite[2];	// computer installation 
  short	Nbits;		// # of bits per value 
  short	BlockMode;	// = 2 
  short	BlockSize;	// = 3200 16-bit words 
  char 	DataSet[2];	// "SU" = subsection; "OR" = "original" 
  short	MissingData;	// =  -32768 
  short	SF;		// general scaling factor 
  short	CF;		// angle scaling factor 
  short	U70;		// unused 
  char 	TapeLabel[6][8]; // input tape labels 
  short U95; 		// word 95 unused 
  short NrecFP;		// # of records per field per plane 
  short NrecP;		// # of records per plane 
  short NrecV;		// # of records per volume without headers 
  short TNrecV;		// total # of records per volumne, including headers x
  short TNrecVNH;	// total # of records per volume, excluding headers 
  char 	VolumeScan[8];	// volumn scan designation 
  short	U105;		// unused 
  short	NPlanes;	// # of planes in volume 
  short	VolumeScanned;	// in cubic km ( /SF ) 
  short	NSamples;	// total # of sampling points (/SF ) 
  short	Density;	// average sampling density (Pts/Cubic m ) * SF 
  short	NPulses;	// # of pulses/pulse volume 
  short	VolumeNumber;	// physical volume number on tape or disk 
  short	U112, U113;
  short	U114, U115;
  TIME 	VolStart;	// volume start time 
  TIME 	VolStop;	// volume stop time 
  short	VolTime;	// volume time in seconds 
  short	NumTime;	// = 4 
  short	U130;
  short	U131;
  short	MinRange;	// of volume x SF 
  short	MaxRange;	// of volume x SF 
  short	Ngates;		// average # of gates per beam 
  short	GateSpacing;	// average gate spacing (m) 
  short	MinGates;	// minimum # of gates 
  short	MaxGates;	// maximum # of gates 
  short	U138;
  short	NumRange;	// index number-range ( = 1) 
  short	U140, U141;
  short	MinAz;		// minimum clockwise azimuth * CF 
  short	MaxAz;		// maximum clockwise azimuth * CF 
  short	NBeams;		// average # of beams per file 
  short AzInc;		// average incremented angle of beams x CF 
  short MinBeams;	// minimum # of beams per file 
  short MaxBeams;	// maximum # of beams per file 
  short NSteps;		// average # of steps/beam 
  short NumAz;		// index number-azimuth ( = 2) 
  short U150;
  char  ScanType[2];	// CO = coplane, PP = ppi 
  short MinElev;	// minimum elevation of volume scan x CF 
  short MaxElev;	// max elevation x CF 
  short NScans;		// number of elevation scans 
  short ElInc;		// increment between elevation scans (deg x CF )
  short AveElev;	// average elevation angle (deg x CF) 
  short Sense;		// direction indicator. +1 = bottom to top 
  short BaseLine;	// angle counter clockwise from N. (coplane only ) 
  short NumCoplane;	// index number - coplane/ppi ( = 3) 
  AXIS  X, Y, Z;	// describes the 3 cartesian axes 
  short	NFields;	// # of fields 
  FIELDS Field[25];	// maximum of 25 fields allowed 
  unsigned short NPoints; // # of points per field per plane 
  short	NLandMarks;	// # of landmarks 
  short	NRadars;	// # of radars 
  short	Nyquist;	// Nyquist velocity, if Nradars == 1 
  short	RadConst;	// Radar Constant, if Nradars == 1 
  LANDMARKS LandMark[15];	
  short int Unused[115]; // Words 396 - 510 are not used 
} MUDHOUSE;

// Describe the Level Header 
typedef struct level
{
  char label[6];		// should be "LEVEL " 
  unsigned short Level;		// level value of plane (km * SF) 
  unsigned short Plane;		// plane number with the volume 
  unsigned short NFields; 	// # of fields 
  unsigned short NPoints;	// # of points per field 
  unsigned short NRecF;		// # of records per field 
  unsigned short NRecP;		// # of records per plane
  unsigned short Unused;
} LEVEL;

class MudField
{
public:
  // Lifecycle functions.
  MudField(void);
  ~MudField(void);

public:
  short int *data;
};

class MudLevel
{
public:
  // Lifecycle functions.
  MudLevel(void);
  ~MudLevel(void);

public:
  LEVEL     header;
  MudField *fields;
};

// This class contains an entire Mudras file.
class MudFile
{
public:
  // Lifecycle functions.
  MudFile(void);
  ~MudFile(void);

  // Map x,y radar image indices to lat/lon (dgerees).
  int map(const float x, const float y, float &lat, float &lon);

  // Map lat/lon (degrees) to x,y radar image indices.
  int mapInverse(const float x, const float y, float &lat, float &lon);

  // Access functions.
  int getField(const int nfield, float *array,
	       const int nx, const int ny, const int nz,
	       const float missingv);

  // Modifier functions.
  //int deleteField(const int lev, const int nfield);
  int addField(const    int lev,
	       const    short *a,
	       const    int nx,
	       const    int ny,
	       const    short scale,
	       const    char *name);
  int clear(void);

  // Input functions.
  int readCSUMudrasVolume(FILE *fp);
  int readCedricTapeVolume(FILE *fp);
  int readCedricDiskVolume(FILE *fp);
  int readPureCedricVolume(FILE *fp, const int nvol = 0);

  // Output functions.
  int writeCSUMudrasVolume(FILE *fp);
  int writeCedricTapeVolume(FILE *fp);
  int writeCedricDiskVolume(FILE *fp);
  int writePureCedricVolume(FILE *fp);

public:
  // Mudras data.
  MUDHOUSE  header;
  MudLevel *levels;
};

#endif
