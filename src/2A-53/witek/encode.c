#include <stdio.h>
#include <assert.h>
#include "encode.h"            /* function prototypes */

#define CEILING(a) ((a)==(int)(a) ? (a) : (a)>0 ? 1+(int)(a) : -(1+(int)(-a)))
#define ENC(c) (((c) & 077) + '!')
#define DEC(c)	(((c) - '!') & 077)
#define LINE_LENGTH  60          /* must be a multiple of 3 */

static void indec(char *p, int n, int * value);
static void outdec(char *p, FILE *f);

void asciiEncode(FILE *fp1,int *value,int ncodes)
{
  /*
  ** Takes an array "value" of integers that contains "ncodes" data points 
  ** that lie in the range 0-255 and ASCII-encodes it to the file associated
  ** with fp1;
  */

  int  i, n;
  int  l,k;
  char buff[LINE_LENGTH];
  int  *bp;
  
  bp = &value[0];
  n  = ncodes/LINE_LENGTH;

  for (i=0;i<=n-1;i++){
    for (k=0;k<=LINE_LENGTH-1;k++) 
      buff[k] = *bp++;
    putc(ENC(LINE_LENGTH), fp1);
    for (l=0;l<LINE_LENGTH;l +=3)
      outdec(&buff[l], fp1);
    putc('\n',fp1);
  }

  n = ncodes%LINE_LENGTH;
  if (n == 0)
    return; 

  for (k=0;k<=n-1;k++)
    buff[k] = *bp++;
  putc(ENC(n),fp1);
  for (l=0;l<n;l +=3)
    outdec(&buff[l], fp1);
  putc('\n',fp1);
  return;
}

int asciiDecode(FILE *fp2,int * value, int ncodes)
{
  int   total,ascii_count,n,k,nchars;
  char  *bp,buff[2000];
  int   *bp2;
  int   c = 0;

  total = ncodes;
  bp2 = &value[0];
  while (total > 0){
    ascii_count  = DEC(fgetc(fp2)); /* number of ascii codes */
    nchars = 4*CEILING(ascii_count/3.0);
    for (k=0;k<=nchars-1;k++)
      buff[k] = fgetc(fp2);
    c = fgetc(fp2);

    n = ascii_count;
    bp = &buff[0];
    while (n > 0) {
      indec(bp, n, bp2);
      if (n >= 1) bp2++;
      if (n >= 2) bp2++;
      if (n >= 3) bp2++;
      bp += 4;
      n -= 3;
    }
    total = total - ascii_count;
  }
  
  /*
  ** Have read a full record of runlength ASCII codes. The next
  ** character should be an EOL, EOF, or EOL followed by an EOF.
  */

  if (feof(fp2)) return(EOF);
  assert((c == '\n'));

  /* found EOL, see if we are at EOF */

  c = fgetc(fp2);
  if (feof(fp2))
    return(EOF);
  else
    ungetc(c,fp2);
  return(0);
}

static void indec(char *p, int n, int * value)
{
  int c1, c2, c3;
  
  c1 = DEC(*p) << 2 | DEC(p[1]) >> 4;
  c2 = DEC(p[1]) << 4 | DEC(p[2]) >> 2;
  c3 = DEC(p[2]) << 6 | DEC(p[3]);
  if (n >= 1) value[0] = c1 & 0377;
  if (n >= 2) value[1] = c2 & 0377;
  if (n >= 3) value[2] = c3 & 0377;
}

static void outdec(char *p, FILE *f)
{
  int c1, c2, c3, c4;

  c1 = *p >> 2;
  c2 = ((*p << 4) & 060) | ((p[1] >> 4) & 017);
  c3 = ((p[1] << 2) & 074) | ((p[2] >> 6) & 03);
  c4 = p[2] & 077;
  putc(ENC(c1), f);
  putc(ENC(c2), f);
  putc(ENC(c3), f);
  putc(ENC(c4), f);
}
