#ifndef _ENCODE_INCLUDED
#define _ENCODE_INCLUDED
#include <stdio.h>
void asciiEncode(FILE *stream,int *value,int ncodes);
int asciiDecode(FILE *stream,int * value, int ncodes);
#endif
