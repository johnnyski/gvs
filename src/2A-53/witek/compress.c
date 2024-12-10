#define NEW

#if defined NEW
#include "compress.h"
#define MARKER 1
void prepUnCompress(int *prep, int nprep, int * value, int *nvals)
{
  int  v,j,k,nv;
  int  start,count;

  start = 0;
  nv = 0;
  k = 0;

  while (start <= nprep-1){
    
    if (prep[start] == MARKER && prep[start+1] == 0){
      count = 1;
      v = MARKER;
      start = start + 2;
    }else  if (prep[start] == MARKER){
      count = prep[start+1];
      v = prep[start+2];
      start = start + 3;
    }
    else{
      count = 1;
      v = prep[start];
      start = start+1;
    }

    for (j=0;j<=count-1;j++){
      value[k+j] = v;
      nv++;
    }
    k = k + count;
  }
  *nvals = nv;
}

void prepCompress(int *value, int nvals, int * prep, int *nprep)
{
  int count,i,j;
  
  count = 1;
  j = 0;
  for (i=0;i<=nvals-2;i++){
  
    if ((value[i] != value[i+1]) || (count == 255)){ 
      if (count > 2) {          /* runlength encode */
        prep[j++] = MARKER;      /* marks start of run length */
        prep[j++] = count;       /* the count */
        prep[j++] = value[i];    /* the value */
      }
      else {                    /* normal code */
        while (count > 0){
          if (value[i] == MARKER){  /* special case */
            prep[j++] = MARKER;
            prep[j++] = 0;
          }
          else 
            prep[j++] = value[i];
          count--;
        }
      }
      count = 0;
    }
    count++;
  }

  /* Do the last one */

  if (count > 2) {          /* runlength encode */
    prep[j++] = MARKER;      /* marks start of run length */
    prep[j++] = count;       /* the count */
    prep[j++] = value[i];    /* the value */
  }
  else {                    /* normal code */
    while (count > 0){
      if (value[i] == MARKER){  /* special case */
        prep[j++] = MARKER;
        prep[j++] = 0;
      }
      else 
        prep[j++] = value[i];
      count--;
    }
  }
  *nprep = j;
}


#else
prepUnCompress(int *value, int * nbins, int * prep, int ncodes)
{
  int v,i,j,k;
  int start,count;
  int data;

  start = 0;
  *nbins = 0;
  k = 0;

  while (start <= ncodes-1){
    
    if (prep[start] == 0 && prep[start+1] == 0){
      count = 1;
      start++;
    }else  if (prep[start] == 0){
      count = prep[++start];
      start++;
    }
    else
      count = 1;

    v = prep[start];
    for (j=0;j<=count-1;j++){
      value[k+j] = v;
      (*nbins)++;
    }
    k = k + count;
    start++;
  }
}

prepCompress(int *value, int nbins, int * prep, int *ncodes)
{
  int count,i,j;
  
  count = 1;
  j = 0;
  for (i=0;i<=nbins-2;i++){
  
    if ((value[i] != value[i+1]) || (count == 255)){ 
      if (count > 2) {         /* runlength encode */
        prep[j++] = 0;           /* marks start of run length */
        prep[j++] = count;       /* the count */
        prep[j++] = value[i];    /* the value */
      }
      else {                    /* normal code */
        while (count > 0){
          if (value[i] == 0){  /* special case */
            prep[j++] = 0;
            prep[j++] = 0;
          }
          else 
            prep[j++] = value[i];
          count--;
        }
      }
      count = 0;
    }
    count++;
  }

  /* Do the last one */

  if (count > 2) {         /* runlength encode */
    prep[j++] = 0;           /* marks start of run length */
    prep[j++] = count;       /* the count */
    prep[j++] = value[i];    /* the value */
  }
  else {                    /* normal code */
    while (count > 0){
      if (value[i] == 0){  /* special case */
        prep[j++] = 0;
        prep[j++] = 0;
      }
      else 
        prep[j++] = value[i];
      count--;
    }
  }
  *ncodes = j;
}

#endif

#ifdef OLD
prepCompress(int *value, int nbins, int * prep, int *ncodes)
{
  int count,i,j;
  
  count = 1;
  j = 0;
  for (i=0;i<=nbins-2;i++){
    if ((value[i] != value[i+1])){ 
      prep[j] = count;
      prep[j+1] = value[i];
      j = j + 2;
      count = 0;
    }
    count = count + 1;
  }
  prep[j+1] = value[i];
  prep[j] = count;
  *ncodes = j+2;
}


prepUnCompress(int *value, int * nbins, int * prep, int ncodes)
{
  int v,i,j,k;
  int start,count;

  start = 0;
  *nbins = 0;
  for (k=0;k<= ncodes-1;k=k+2){
    count = prep[k];
    v = prep[k+1];
    for (j=0;j<=count-1;j++){
      value[start+j] = v;
      (*nbins)++;
    }
    start = start + count;
  }
}
#endif



