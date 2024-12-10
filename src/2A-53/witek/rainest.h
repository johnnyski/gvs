#include "aiw.h"

#include "classmap.h"


#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define MAXFILELENGTH 1000
#define CLASSES_PATH  "."
#define PRODUCT_PATH1 "."
#define PRODUCT_PATH2 "."

void settozero(void);
void instfield(FILE *fp);
void readray(FILE *fp);
void accumulation(void);
int  advective_accum(void);
void poltocart(void);
void carttopol(void);
void writehour(FILE *fp);
void writeminute(void);
void writeday(void);
void write5day(void);
void writemonth(void);
void products(void);

   





