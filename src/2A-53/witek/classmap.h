#ifndef _CLASSMAP_INCLUDED
#define _CLASSMAP_INCLUDED
                /* Size of the classification maps follow */

#define CLASS_NX    151
#define CLASS_NY    151

                  /* Function declarations follow */

#include <assert.h>
#include <stdio.h>
void writeClassificationMap(FILE *stream,int map[CLASS_NX][CLASS_NY]);
void readClassificationMap(FILE *stream,int map[CLASS_NX][CLASS_NY]);
#endif
