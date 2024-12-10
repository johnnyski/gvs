#include <malloc.h>
#include <math.h>
#include <findPoints.h>
#include <stdio.h>

// Mask dimensions.
static int nx = 0;
static int ny = 0;
static int npoints = 0;
static FrPoint *mask   = 0;
static FrPoint *points = 0;

int frInit(const int nxt, const int nyt, const float xres, const float yres,
	   const float radius)
{
  float r2 = radius * radius;
  float old_x;
  int ok;
  float old_y;
  int j;
  int ijrad;
  nx = nxt;
  ny = nyt;
  npoints = 0;

  delete[] mask;
  delete[] points;

  mask = new FrPoint[nx*ny];
  points = new FrPoint[nx*ny];

  // The problem is symmetric so just do one quarter of the circle.
  ijrad = int(radius / xres);
  old_x = -radius - xres;
  old_y = 0.0;

  // Travel up from y0 to y1 and compute a new starting x for each j value.
  for (j = 0; j <= ijrad; j++)
    {
      float new_y = j * yres;
      float new_y2 = new_y * new_y;
      float new_x;
      float old_y2 = old_y * old_y;
      int new_i;

      new_x = old_x + old_y / sqrt(r2 - old_y2) * xres;

      // Truncate the new x and include this point in the list of
      // points inside the circle of radius 'radius'.
      ok = 0;
      {
	int i;
	int start_i = int(new_x / xres - 1);
	for (i = start_i; i <= 0; i++)
	  {
	    float x0 = xres * i;
	    float x_dist2 = x0 * x0;
	    if ((x_dist2 + new_y2) <= r2)
	      {
		new_x = x0;
		ok = 1;
		break;
	      }
	  }
	if (!ok)
	  {
	    fprintf(stderr, "Error in circle algorithm.\n");
	    return(0);
	  }
      }
      
      new_i = int(new_x / xres);

      // Add all points inside this boundary point to the list of valid
      // points -- use symmetery.
      {
	int ii;
	for (ii = new_i; ii <= -new_i; ii++)
	  {
	    mask[npoints].x = ii;
	    mask[npoints++].y = j;
	  }
	if (j != 0)
	  {
	    for (ii = new_i; ii <= -new_i; ii++)
	      {
		mask[npoints].x = ii;
		mask[npoints++].y = -j;
	      }
	  }
      }
      old_x = new_x;
      old_y = new_y;
    }

  return(1);
}

int frGetPoints(const int i, const int j, FrPoint **pointst, int *npointst)
{
  int ii;
  *npointst = 0;

  for (ii=0;ii<npoints;ii++)
    {
      int i2 = i + mask[ii].x;
      int j2 = j + mask[ii].y;

      if ((i2 < 0) || (i2 >= nx)) continue;
      if ((j2 < 0) || (j2 >= ny)) continue;

      points[*npointst].x = i2;
      points[*npointst].y = j2;
      (*npointst)++;
    }
  *pointst = points;
  return(1);
}

int frGetMask(FrPoint **maskt, int *npointst)
{
  *maskt = mask;
  *npointst = npoints;
  return(1);
}

int frClose(void)
{
  delete[] mask;
  mask = 0;
  delete[] points;
  points = 0;
  return(1);
}
