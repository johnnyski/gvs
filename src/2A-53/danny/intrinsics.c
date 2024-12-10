/*
 *
 * C interface to fortran intrinsic routines that f2c does not provide.
 *     ishft(word, n)
 *   
 */
int
#ifdef linux
 ishft_
#else
 ishft
#endif
(int *word, int *n)
{
  if (*n < 0) return *word >> *n;
  else return *word << *n;
}

int iand_ (int *w1, int *w2)
{
  return *w1 & *w2;
}

int ior_ (int *w1, int *w2)
{
  return *w1 | *w2;
}

int ixor_ (int *w1, int *w2)
{
  return *w1 ^ *w2;
}


