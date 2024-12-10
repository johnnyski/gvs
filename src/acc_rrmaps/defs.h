/* a central place to put the definitions */

// irint: Round  to integral value in integer format
#define irint(x)             ((int) (x + 0.5))  // TN added 08/30/97

/* functions */
float readInArray (FILE *fp, float* maparray, int arsize);
void writeOutArray (float* maparray, int arsize);
int addABArrays (float* a_array, float* b_array, float* res_array,
                      int arsize);
int subtractABArrays (float* a_array, float* b_array, float* diff_array,
                      int arsize);
void outputTRMMArray (FILE* fp, float* maparray, int arsize);
int reduceArrayRes (float* a_array, float* res_array, int arsize, 
                    int newpixeldim);
int computeStdDev (float* hires_array, float* loresmean_array, 
                   float *result_array, int arsize,  int newpixeldim);
int map_sphere(const float clat, const float clon,const float deltax, 
	       const float deltay, float &lat, float &lon);
int scalarMultArray(float* a_array, float* res_array,
		 int arsize, float factor);
int zeroPad(char dig2[2], int number);
int getCurrentGMTDate(char datestr[13]); 
int getCDFVar(const char *inputfn, const char *fieldname,
	   int &ndims, long *&dims, float *&data, long &len);
float rmsdABArrays (float* a_array, float* b_array, int arsize);
int basicStatsArray (float* a_array, int arsize, float &mean, float &condmean, 
		     int &numprecip);
int latlongtoXY(const float clat, const float clon,
        const float lat, const float lon,
        double &x, double &y);

void output3DArray (FILE* fp, float* maparray, int xsize, int ysize, int numlevels);
void output3DArray1level (FILE* fp, float* maparray, int xsize, int ysize, int level);
int getCDFDims(const char *inputfn, const char *fieldname,
	   int &ndims, long *&dims);

int xytoArrayIndex(const double xkm, const double ykm,
		   int arsize, float pixelkm, 
		   int flip, int &index_x, int &index_y);
int getWindows1and9 (float* a_array, int arsize, int index_x, int index_y, 
		     float &mean1, float &mean9);


int getWindows1and9cs (float* a_array, float* cs_array, 
		       int arsize, int index_x, int index_y, 
		     float &mean1, float &mean9, int &cs1, int &cs9);
/* constants */
#define MAX_LINE 256       // Increased from 86 to handle large size filename
#define DEBUG 0
#define MISSINGVAL -99
#define MAX_FILES 2*31*48  // two months of instant files

#define MASTER_DIAG_FILE "/home/disk/hail/yuter/trmm/aiw2/master.diagfile"
#define MAXGAUGES 100


#define CS_NOECHO 0
#define CS_SF 10
#define CS_CONV 20


