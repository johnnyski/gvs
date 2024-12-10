typedef struct
{
  int x;
  int y;
} FrPoint;

int frGetMask(FrPoint **maskt, int *npointst);
int frGetPoints(const int i, const int j, FrPoint **points, int *npoints);
int frInit(const int nx, const int ny, const float xres, const float yres,
	   const float radius);
int frClose(void);
