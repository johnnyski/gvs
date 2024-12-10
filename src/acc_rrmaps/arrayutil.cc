// Array Utility Functions
//
// SEY 18 August 1996

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "defs.h"

// have to handle array like it is 1d
float readInArray (FILE* fp, float* maparray, int arsize) {

int i,j;
double thing;

for (j=0;j<arsize;j++){
    for (i=0;i<arsize;i++){
	/* printf("i=%d j=%d\n",i,j); */
	fscanf(fp,"%lf",&thing);
	maparray[arsize*j+i]=thing;
    }}

return(0);
}


// write out TRMM array in the TRMM array format to file
void outputTRMMArray (FILE* fp, float* maparray, int arsize) {

int i,j;

for (j=0;j<arsize;j++){
    for (i=0;i<arsize;i++){
	fprintf(fp,"%0.3f ", maparray[arsize*j+i]);
    }
    fprintf(fp,"\n");
}

} // end outputTRMMArray

void output3DArray (FILE* fp, float* maparray, int xsize, int ysize, int numlevels) {

int i,j, k;

for (k=0;k<numlevels;k++){
    for (j=0;j<ysize;j++){
	for (i=0;i<xsize;i++){
	    fprintf(fp,"%0.3f ", maparray[xsize*ysize*k+xsize*j+i]);
	}
	fprintf(fp,"\n");
    }}

} // end output3DArray

void output3DArray1level (FILE* fp, float* maparray, int xsize, int ysize, int level) {

int i,j, k;

k = level-1;
    for (j=0;j<ysize;j++){
	for (i=0;i<xsize;i++){
	    fprintf(fp,"%0.3f ", maparray[xsize*ysize*k+xsize*j+i]);
	}
	fprintf(fp,"\n");
    }

} // end output3DArray


// write out array to stderr primarily for debugging
void writeOutArray (float* maparray, int arsize) {

int i,j;

for (j=0;j<arsize;j++){
    for (i=0;i<arsize;i++){
	fprintf(stderr,"%0.2f ", maparray[arsize*j+i]);
    }
fprintf(stderr,"\n");
}
	

}


// substract two arrays, they must be the same size with dim arsize
// if either of the points is missing set the result to missing
int subtractABArrays (float* a_array, float* b_array, float* diff_array,
		      int arsize) {

    int i,j;

    for (j=0;j<arsize;j++){
	for (i=0;i<arsize;i++){
	    if ((a_array[arsize*j+i] != MISSINGVAL) && 
		(b_array[arsize*j+i] != MISSINGVAL)) {
		diff_array[arsize*j+i]=a_array[arsize*j+i]-b_array[arsize*j+i];
	    }
	    else {
		diff_array[arsize*j+i]=MISSINGVAL;
	    }
	}}

    return(0);
} // end subtractABArrays

// add two arrays, they must be the same size with dim arsize
// if either of the points is missing set the result to missing
int addABArrays (float* a_array, float* b_array, float* res_array,
		 int arsize) {

    int i,j;

    for (j=0;j<arsize;j++){
	for (i=0;i<arsize;i++){
	    if ((a_array[arsize*j+i] != MISSINGVAL) && 
		(b_array[arsize*j+i] != MISSINGVAL)) {
		res_array[arsize*j+i]=a_array[arsize*j+i]+b_array[arsize*j+i];
	    }
	    else {
		res_array[arsize*j+i]=MISSINGVAL;
	    }
	}}

    return(0);
} //end addABArrays


// reduce resolution of array
// for case a when reduced resolution pixel contains missing values
// that pixel not counted in summation 
// if all the pixels are bad then set to missing
int reduceArrayRes (float* a_array, float* res_array, int arsize, 
		    int newpixeldim)
{
int i,j;
int newi, newj;
int newarsize, adjarsize;

newarsize = (int) floor(arsize/newpixeldim);
fprintf(stderr,"newarsize is %d\n",newarsize);

// keep track of the counts in the summation since 
// dont count missing data
int* newarraycount = new int[newarsize*newarsize]; // array of counts 

// initialize the new arrays
for (newj=0;newj<newarsize;newj++){
    for (newi=0;newi<newarsize;newi++){
	res_array[newarsize*newj+newi]=0;
	newarraycount[newarsize*newj+newi]=0;
    }}

// first sum up the values in each reduced resolution pixel
// adjust limits so only use portion of orig array that
// contains an integer number of newpixeldim

adjarsize = newarsize * newpixeldim;
fprintf(stderr,"adjarsize is %d\n",adjarsize);

for (j=0;j<adjarsize;j++){
    for (i=0;i<adjarsize;i++){
	newj= (int) floor(j/newpixeldim);
	newi= (int) floor(i/newpixeldim);
	// if value is missing treat as missing (wont count when divide) 
	if (a_array[arsize*j+i] != MISSINGVAL){
	    res_array[newarsize*newj+newi] += a_array[arsize*j+i];
	    newarraycount[newarsize*newj+newi] += 1;
    }
    }}

// now calc the mean
for (newj=0;newj<newarsize;newj++){
    for (newi=0;newi<newarsize;newi++){
	if (newarraycount[newarsize*newj+newi] > 0) {
	res_array[newarsize*newj+newi]=
	  res_array[newarsize*newj+newi]/newarraycount[newarsize*newj+newi];
    }
	else { // all the pixels were bad
	  res_array[newarsize*newj+newi] = MISSINGVAL;
      }
    }}

return(newarsize);
} // end reduceArrayRes

// compute the standard deviation of the hires pixels within each lo resolution
// pixel, has similar setup to reduceArrayRes for handling missing data
int computeStdDev (float* hires_array, float* loresmean_array, 
		   float *result_array, int arsize,  int newpixeldim)
{

int i,j;
int newi, newj;

int newarsize, adjarsize;
newarsize =  (int) floor(arsize/newpixeldim);
adjarsize = newarsize * newpixeldim;

// dont count missing data, so need to keep track of N for each pixel
int* newarraycount = new int[newarsize*newarsize]; // array of counts 

// initialize the result and count arrays
for (newj=0;newj<newarsize;newj++){
    for (newi=0;newi<newarsize;newi++){
	result_array[newarsize*newj+newi]=0;
	newarraycount[newarsize*newj+newi]=0;
    }}

// first compute the summation of (x-averx)^2

 for (j=0;j<adjarsize;j++){
     for (i=0;i<adjarsize;i++){
	 newj= (int) floor(j/newpixeldim);
	 newi= (int) floor(i/newpixeldim);
	 // if value is missing treat as missing (wont count when divide) 
	 if (hires_array[arsize*j+i] != MISSINGVAL){
	     result_array[newarsize*newj+newi] += 
	       pow((hires_array[arsize*j+i]-loresmean_array[newarsize*newj+newi]),2);
	     newarraycount[newarsize*newj+newi] += 1;
	 }
     }}

// now divide by N and calculate the sqrt
for (newj=0;newj<newarsize;newj++){
    for (newi=0;newi<newarsize;newi++){
	// condition was > 0, but want N to be the same for all
	if (newarraycount[newarsize*newj+newi] == (newpixeldim*newpixeldim)) { 
	result_array[newarsize*newj+newi]=
	  sqrt(result_array[newarsize*newj+newi]/newarraycount[newarsize*newj+newi]);
    }
	else { // all the pixels were bad
	  result_array[newarsize*newj+newi] = MISSINGVAL;
      }
    }}

return(newarsize);
} // end computeStdDev
// multiply array by a constant
int scalarMultArray (float* a_array, float* res_array,
		 int arsize, float scalar) {

    int i,j;

    for (j=0;j<arsize;j++){
	for (i=0;i<arsize;i++){
	    if (a_array[arsize*j+i] != MISSINGVAL) {
		res_array[arsize*j+i]= (float) a_array[arsize*j+i]* (float)scalar;
	    }
	    else {
		res_array[arsize*j+i]=MISSINGVAL;
	    }
	}}

    return(0);
} //end scalarMultArray

// calculate root mean square difference between two arrays, 
// they must be the same size with dim arsize
// if either of the points is missing dont use in the runningsqsum or N
// returns the rmsd value for the array
//
// formula:
//                   summation j=1 to N of (xaj-xbj)^2
//            sqrt(  --------------------------------  )
//                         N
float rmsdABArrays (float* a_array, float* b_array, int arsize) {

    int i,j;
    float rmsd;
    int Ncount =0;
    float runningsqdiff =0;

    for (j=0;j<arsize;j++){
	for (i=0;i<arsize;i++){
	    if ((a_array[arsize*j+i] != MISSINGVAL) && 
		(b_array[arsize*j+i] != MISSINGVAL)) {
		runningsqdiff +=pow(a_array[arsize*j+i]-b_array[arsize*j+i],2);
		Ncount++;
	    }
	    }}

    // now divide by N and take the sqrt
    if (Ncount>0){
    rmsd = pow((runningsqdiff/Ncount),0.5);
}
    else {
	rmsd=-99;
    }
    fprintf(stderr,"inside %0.2f\n",rmsd);
    return(rmsd);
} // end rmsdABArrays

// calculate some basic statistics
int basicStatsArray (float* a_array, int arsize, float &mean, float &condmean, int &numprecip) {

    int i,j;
    double runningtot =0.0;
    int count=0;
    numprecip=0;
    mean=0.0;
    condmean=0.0;
    

     for (j=0;j<arsize;j++){
	 for (i=0;i<arsize;i++){
	     if (a_array[arsize*j+i] != MISSINGVAL) {
		 runningtot+=a_array[arsize*j+i];
		 count++;
		 if (a_array[arsize*j+i]>0){
		     numprecip++;
		 }
	     }
	 }}

    // calc mean if can
    if ((count>0) && (runningtot>0)){
	mean=(float) runningtot/count;
	if (numprecip>0) {
	    condmean=(float) runningtot/numprecip;
	}
	else {
	    condmean=0.0;
	}
    }
    else {
	mean=0.0;
    }

/* fprintf(stderr,
	"inside stats mean is %0.4f condmean is %0.4f numprecip is %d\n",
	    mean, condmean, numprecip); */
return(0);

} //end basicStatsArray

// calculate the average rain in 1x1 pixel and 3x3 pixel windows 
// centered on index pixels
// in 3x3 ignores missing values, ie. calc mean without them
int getWindows1and9 (float* a_array, int arsize, int index_x, int index_y, 
		     float &mean1, float &mean9) {

    int i,j;
    double runningtot =0;
    int count=0;
    mean1=-99; // initialize values to missing
    mean9=-99;

    if ((index_x>=0) && (index_x<arsize) &&
	(index_y>=0) && (index_y<arsize)) // check limits
      {
	  mean1=a_array[arsize*index_y+index_x];
	  // check that 3x3 is not out of bounds
	  if (((index_x-1)>=0) && ((index_x+1)<arsize) &&
	      ((index_y-1)>=0) && ((index_y+1)<arsize)){
	      for (j=index_y-1;j<=index_y+1;j++){
		  for (i=index_x-1;i<=index_x+1;i++){
		      if (a_array[arsize*j+i] != MISSINGVAL) {
			  runningtot+=a_array[arsize*j+i];
			  count++;
		      }
		  }}

	      // calc mean9 if can
	      if (count>0) {
		  mean9=(float) runningtot/count;
	      }
	  }
	  /* fprintf(stderr,
	     "inside getWindows1and9  mean1 is %0.4f mean9 is %0.4f\n",
	     mean1, mean9); */
	  return(1);
      }
    else {
	return(0);
    }

} //end getWindows1and9 


// calculate the average rain in 1x1 pixel and 3x3 pixel windows 
// centered on index pixels
// in 3x3 ignores missing values, ie. calc mean without them
// the most numerous noecho=0, sf=1, conv=2 wins the pixel
int getWindows1and9cs (float* a_array, float* cs_array, 
		       int arsize, int index_x, int index_y, 
		     float &mean1, float &mean9, int &cs1, int &cs9) {

    int i,j;
    double runningtot =0;
    int count=0;
    int noechoct=0, sfct=0, convct=0;
    mean1=-99;
    mean9=-99;
    cs1=-99; // initialize values to missing
    cs9=-99;

    if ((index_x>=0) && (index_x<arsize) &&
	(index_y>=0) && (index_y<arsize)) // check limits
      {
	  mean1=a_array[arsize*index_y+index_x];
	  cs1=irint(cs_array[arsize*index_y+index_x]); // cdf data are float
	  // check that 3x3 is not out of bounds
	  if (((index_x-1)>=0) && ((index_x+1)<arsize) &&
	      ((index_y-1)>=0) && ((index_y+1)<arsize)){
	      for (j=index_y-1;j<=index_y+1;j++){
		  for (i=index_x-1;i<=index_x+1;i++){
		      if (a_array[arsize*j+i] != MISSINGVAL) {
			  runningtot+=a_array[arsize*j+i];
			  // it would be logical to use a
			  // raining condition here.. BUT
			  // thanks to Danny we cant
			  //if (a_array[arsize*j+i] >0 ) { // raining
			  if (cs_array[arsize*j+i] != MISSINGVAL) {
			      if (cs_array[arsize*j+i]==CS_NOECHO){
				  noechoct++;
			      }
			      else if (cs_array[arsize*j+i]==CS_SF){
				  sfct++; 
			      }
			      else if (cs_array[arsize*j+i]==CS_CONV){
				  convct++;
			      }
			      else {
				  fprintf(stderr,"unrecognized cs index =%0.1f\n",
					  cs_array[arsize*j+i]);
			      }}
			  count++;
		      }
		  }}

	      // calc mean9 if can
	      if (count>0) {
		  mean9=(float) runningtot/count;
		  //cs9= irint(runningcstot/count);
		  // take which cs count won
		  // ties take the lower
		  if ((noechoct>=sfct) && (noechoct>=convct)) cs9=CS_NOECHO;
		  else if ((sfct>=noechoct) && (sfct>=convct)) cs9=CS_SF;
		  else if ((convct>=noechoct) && (convct>=sfct)) cs9=CS_SF;
		  else cs9=-99; // can't assign
	      }
	  }
	  /* fprintf(stderr,
	     "inside getWindows1and9  mean1 is %0.4f mean9 is %0.4f\n",
	     mean1, mean9); */
	  return(1);
      }
    else {
	return(0);
    }

} //end getWindows1and9 
