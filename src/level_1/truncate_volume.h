/**********************************************************************/
/**********************************************************************/
/**********************************************************************/

#ifndef __TRUNCATE_H__
#define __TRUNCATE_H__ 1

#include "rsl.h"


Ray * truncate_ray(Ray * r, float range, float min_dbz);
Sweep * truncate_sweep(Sweep *s, float range, float min_dbz);
Volume * truncate_volume(Volume *v, float range, float min_dbz);


#endif
