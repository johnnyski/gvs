#include <mudras.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static const float pi = 3.14159;
static const float rearth = 6372000.0; // radius of earth in meters

static void error(const char *s)
{
  fprintf(stderr, "%s\n", s);
  return;
}

// MudField code.
MudField::MudField(void)
{
  data = 0;
  return;
}

MudField::~MudField(void)
{
  return;
}

// MudLevel code.
MudLevel::MudLevel(void)
{
  fields = 0;
  return;
}

MudLevel::~MudLevel(void)
{
  return;
}

// MudFile code.
MudFile::MudFile(void)
{
  levels = 0;
  return;
}

int MudFile::clear(void)
{
  int z = 0;
  int f = 0;

  // First delete all fields from every level.
  if (levels)
    {
      for (z = 0; z < header.Z.N; z++)
	{
	  if (levels[z].fields)
	    {
	      for (f = 0; f < levels[z].header.NFields; f++)
		{
		  delete[] levels[z].fields[f].data;
		}
	      delete[] levels[z].fields;
	    }
	}
      delete[] levels;
      levels = 0;
    }

  return(1);
}

MudFile::~MudFile(void)
{
  clear();
  return;
}

int MudFile::readCSUMudrasVolume(FILE *fp)
{
  int i = 0;
  int field = 0;
  int z = 0;

  clear();

  if (!fp) return(0);

  // Read in header.
  if (!fread(&header, sizeof(header), 1, fp))
    {
      error("Cannot read CSU Mudras header");
      return(0);
    }

  // Skip to byte 8192 in the file.
  fseek(fp, 8192, SEEK_SET);

  // Allocate space for the levels
  levels = new MudLevel[header.NPlanes];

  // The level information is not in the CSU file, so we must
  // create the level headers.  We assume that each level has the same number
  // of fields (which must be true for CSU files).
  {
    for (i = 0; i < header.NPlanes; i++)
      {
	float lev = (header.Z.Min + i * header.Z.Spacing) / 1000.0 * header.SF;
	levels[i].header.NFields = header.NFields;
	levels[i].header.NPoints = header.NPoints;
	levels[i].header.NRecF   = header.NrecFP;
	levels[i].header.NRecP   = header.NrecP;
	levels[i].header.Level   = int(lev);
	levels[i].fields = new MudField[header.NFields];
      }
  }
  
  for (field = 0; field < header.NFields; field++)
    {
      // Bring in all the levels.
      for (z = 0; z < header.NPlanes; z++)
	{
	  // CSU files are padded to an integral number of 8192 bytes.
	  int npnts = levels[z].header.NPoints;
	  int nbytes = npnts * 2;

	  // Number of 6400-byte records in this level.
	  int n6400 = header.NrecFP - 1;

	  // Number of good bytes in last 6400-byte record.
	  int remainder = nbytes - n6400 * 6400;

	  short int *buf = new short int[(n6400 + 1) * 3200];

	  for (i = 0; i < n6400; i++)
	    {
	      fread(&buf[i * 3200], 8192, 1, fp);
	    }

	  fread(&buf[n6400 * 3200], remainder, 1, fp);

	  // Skip past the remainder in this record.
	  fseek(fp, 8192 - remainder, SEEK_CUR);

	  levels[z].fields[field].data = buf;
	}
    }
  
  return(1);
}

int MudFile::readCedricTapeVolume(FILE *fp)
{
  int z = 0;
  int field = 0;

  clear();

  if (!fp) return(0);

  // Read in header.
  if (!fread(&header, sizeof(header), 1, fp))
    {
      error("Cannot read CEDRIC tape volume header.");
      return(0);
    }

  // Allocate space for the levels
  levels = new MudLevel[header.NPlanes];

  for (z = 0; z < header.NPlanes; z++)
    {
      // Read the header.
      if (!fread(&levels[z].header, sizeof(levels[z].header), 1, fp))
	{
	  char s[80];
	  sprintf(s, "Cannot read level %d from CEDRIC tape volume.", z);
	  error(s);
	  return(0);
	}

      int npts = levels[z].header.NPoints;

      levels[z].fields = new MudField[levels[z].header.NFields];
      
      // Pull out the field we want.
      for (field = 0; field < levels[z].header.NFields; field++)
	{	 
	  short int *buf = new short int[header.X.N * header.Y.N];
	  fread(buf, sizeof(short int) * npts, 1, fp);
	  levels[z].fields[field].data = buf;
	}
    }
  
  return(1);
}

int MudFile::readCedricDiskVolume(FILE *fp)
{
  int z = 0;
  int field = 0;

  clear();

  if (!fp) return(0);
  
  // Read in header and write it to tape.
  if (!fread(&header, sizeof(header), 1, fp))
    {
      error("Cannot read CEDRIC disk volume.");
      return(0);
    }

  // Skip past 4 extra padding bytes.
  fseek(fp, 4, SEEK_CUR);

  // Allocate space for the levels
  levels = new MudLevel[header.NPlanes];

  for (z = 0; z < header.NPlanes; z++)
    {
      // Read the header.
      if (!fread(&levels[z].header, sizeof(levels[z].header), 1, fp))
	{
	  return(0);
	}

      // Skip past 4 extra padding bytes.
      fseek(fp, 4, SEEK_CUR);

      int npts = levels[z].header.NPoints;

      levels[z].fields = new MudField[levels[z].header.NFields];

      // Compute the size of each field.
      int nbytes = sizeof(short int) * npts;
      int npadbytes = 0;
      if (nbytes%8) npadbytes = nbytes / 8 * 8 + 8 - nbytes;
	
      // Pull out the field we want.
      for (field = 0; field < levels[z].header.NFields; field++)
	{
	  short int *buf = new short int[header.X.N * header.Y.N];
	  fread(buf, nbytes, 1, fp);
	  levels[z].fields[field].data = buf;

	  // Skip past the extra padding bytes for this field.
	  fseek(fp, npadbytes, SEEK_CUR);
	}
    }
  
  return(1);
}

int MudFile::readPureCedricVolume(FILE *fp, const int nvol)
{
  int z = 0;
  int field = 0;

  clear();

  if (!fp) return(0);

  // First we need to read the file header.
  int fheader[385];  // 1540 bytes for the file header
  fread(fheader, sizeof(fheader), 1, fp);
    
  // Get the file offset for the volume requested.
  int foffset = fheader[4 + nvol];
  if (foffset <= 0)
    {
      error("Cannot find requested volume in Pure CEDRIC file.");
      return(0);
    }

  // Move the file pointer to the requested volume.
  fseek(fp, foffset, SEEK_SET);

  // Read in header and write it to tape.
  if (!fread(&header, sizeof(header), 1, fp))
    {
      error("Cannot read Pure CEDRIC header.");
      return(0);
    }

  // Remember to convert to our internal convention for Z min and max:
  // km * SF. (Pure CEDRIC is meters).
  header.Z.Min = header.Z.Min * header.SF / 1000;
  header.Z.Max = header.Z.Max * header.SF / 1000;

  // Allocate space for the levels
  levels = new MudLevel[header.NPlanes];

  for (z = 0; z < header.NPlanes; z++)
    {
      // Read the header.
      if (!fread(&levels[z].header, sizeof(levels[z].header), 1, fp))
	{
	  return(0);
	}

      int npts = levels[z].header.NPoints;

      levels[z].fields = new MudField[levels[z].header.NFields];

      // Compute the size of each field.
      int nbytes = sizeof(short int) * npts;

      // Read all fields.
      for (field = 0; field < levels[z].header.NFields; field++)
	{
	  short int *buf = new short int[header.X.N * header.Y.N];
	  fread(buf, nbytes, 1, fp);
	  levels[z].fields[field].data = buf;
	}
    }
  
  return(1);
}

int MudFile::getField(const int nfield, float *a,
		      const int nx, const int ny, const int nz,
		      const float missingv)
{
  int i = 0;
  int j = 0;
  int z = 0;
  if (header.NPlanes > nz)
    {
      fprintf(stderr,"\n*** Error: mudras.cc::getField:\n"
              "*** header.NPlanes = %d, exceeds nz = %d\n",
	      header.NPlanes, nz);
      fprintf(stderr,"*** How to correct:\n");
      fprintf(stderr,"*** Try decreasing the tolerance (P7) in PROCESS command"
              " in SPRINT input file.\n\n");
      return(0);
    }

  // Pre-fill the array with missingv.
  {
    for (i = 0; i < nx * ny * nz; i++) a[i] = missingv;
  }

  float scale = header.Field[nfield].scale;
  for (z = 0; z < header.NPlanes; z++)
    {
      for (j = 0; j < header.Y.N; j++)
	{
	  for (i = 0; i < header.X.N; i++)
	    {
	      short int v = levels[z].fields[nfield].data[i + j * header.X.N];
	      if (v != -32768)
		{
		  a[i + nx * j + nx * ny * z] = v / scale;
		}
	      else
		{
		  a[i + nx * j + nx * ny * z] = missingv;
		}
	    }
	}
    }
  
  return(1);
}

int MudFile::writeCSUMudrasVolume(FILE *fp)
{
  int i = 0;
  int field = 0;
  int z = 0;

  if (!fp) return(0);

  // This was hard to find. It turns out that certain words in
  // header.Unused are actually *used*!  In particular, word 400
  // in the header is the number of records per field, and word 400+I
  // is the starting record of field I.  We need to compute the values
  // of words 400+I for 1<=I<=number of fields we have.  Finding this
  // was *really* tricky.
  {
    for (i=0;i<header.NFields;i++)
      {
	header.Unused[5+i] = header.Unused[4] * i + 2;
      }
  }

  // Write out header.
  if (!fwrite(&header, sizeof(header), 1, fp))
    {
      error("Cannot write CSU Mudras header.");
      return(0);
    }
/*  
  fprintf(stderr, "%d - %d %d %d\n",
	  header.Unused[4],
	  header.Unused[5],
	  header.Unused[6],
	  header.Unused[7]);
*/
  // Write through byte 8192 in the file.
  {
    char buf[8192];
    fwrite(buf, 8192 - sizeof(header), 1, fp);
  }
  
  for (field = 0; field < header.NFields; field++)
    {
      // Write out all the levels.
      for (z = 0; z < header.NPlanes; z++)
	{
	  // CSU files are padded to an integral number of 8192 bytes.
	  int npnts = levels[z].header.NPoints;
	  int nbytes = npnts * 2;

	  // Number of 6400-byte records in this level.
	  int n6400 = header.NrecFP;

	  short int *buf = levels[z].fields[field].data;
	  short *buf2 = new short[n6400 * 3200 + 4096];
	  memcpy(buf2, (char *) buf, nbytes);

	  for (i = 0; i < n6400; i++)
	    {
	      fwrite(&buf2[i * 3200], 8192, 1, fp);
	    }
	  delete[] buf2;

	  levels[z].fields[field].data = buf;
	}
    }
  
  return(1);
}

int MudFile::writeCedricTapeVolume(FILE *fp)
{
  int z = 0;
  int field = 0;

  if (!fp) return(1);
  fwrite(&header, sizeof(header), 1, fp);

  // Write out each level.
  for (z = 0; z < header.NPlanes; z++)
    {
      // First write out the header.
      fwrite(&levels[z].header,
	     sizeof(levels[z].header), 1, fp);
      
      // Write all fields.
      for (field = 0; field < levels[z].header.NFields; field++)
	{	 
	  fwrite(levels[z].fields[field].data,
		 sizeof(short) * header.X.N * header.Y.N,
		 1, fp);
	}
    }
  
  fclose(fp);
  return(1);
}

int MudFile::writeCedricDiskVolume(FILE *fp)
{
  int z = 0;
  int field = 0;

  if (!fp) return(1);
  fwrite(&header, sizeof(header), 1, fp);

  // Create a buffer space large enough for any padding we need.
  char pad[8];

  // Write out a 4 byte pad.
  {
    fwrite(pad, 4, 1, fp);
  }

  // Write out each level.
  for (z = 0; z < header.NPlanes; z++)
    {
      // First write out the header.
      fwrite(&levels[z].header,
	     sizeof(levels[z].header), 1, fp);
      
      // Write out a 4 byte pad.
      {
	fwrite(pad, 4, 1, fp);
      }
      
      // Write all fields.
      for (field = 0; field < levels[z].header.NFields; field++)
	{	 
	  fwrite(levels[z].fields[field].data,
		 sizeof(short) * header.X.N * header.Y.N,
		 1, fp);

	  // Pad the field so that it consumes an even number of 2-byte words.
	  int nbytes = sizeof(short) * header.X.N * header.Y.N;
	  int npadbytes = 0;	  
	  if (nbytes%8) npadbytes = nbytes / 8 * 8 + 8 - nbytes;
	  if (npadbytes > 0) fwrite(pad, npadbytes, 1, fp);
	}
    }
  
  fclose(fp);
  return(1);
}

int MudFile::writePureCedricVolume(FILE *fp)
{
  int l = 0;
  int z = 0;
  int field = 0;

  if (!fp) return(1);

  // First we need to write out the file header.
  int fheader[385];  // 1540 bytes for the file header
  strncpy((char *) fheader, "CED1", 4);
  fheader[1] = 0;

  // Compute the file size.
  int nlevels = header.NPlanes;

  // Each level may have a different number of fields, so me must loop
  // over all levels to compute the size of the file.
  int sz = 1540 + 1020;
  {
    for (l = 0; l < nlevels; l++)
      {
	sz += levels[l].header.NFields * levels[l].header.NPoints * 2;
      }
  }

  fheader[2] = sz;
  fheader[4] = 1540;
  fwrite(fheader, sizeof(fheader), 1, fp);

  // Now write the volume header. Remember to change the units on
  // the Z extents from the internal representation (km * SF) to the
  // Pure CEDRIC convention of meters.
  // Apparently, the CEDRIC doc is inconsistent with the latest version
  // of SPRINT:  zmin and zmax are given in meters in Pure CEDRIC files.
  // (not scaled by SF).
  header.Z.Min = header.Z.Min * 1000 / header.SF;
  header.Z.Max = header.Z.Max * 1000 / header.SF;
  fwrite(&header, sizeof(header), 1, fp);
  header.Z.Min = header.Z.Min * header.SF / 1000;
  header.Z.Max = header.Z.Max * header.SF / 1000;

  // Write out each level.
  for (z = 0; z < header.NPlanes; z++)
    {
      // First write out the header.
      fwrite(&levels[z].header,
	     sizeof(levels[z].header), 1, fp);
      
      // Write all fields.
      for (field = 0; field < levels[z].header.NFields; field++)
	{	 
	  fwrite(levels[z].fields[field].data,
		 sizeof(short) * header.X.N * header.Y.N,
		 1, fp);
	}
    }
  
  fclose(fp);
  return(1);
}

int MudFile::addField(const    int lev,
		      const    short *a,
		      const    int nx,
		      const    int,
		      const    short scale,
		      const    char *name)
{
  int i = 0;
  int j = 0;
  int k = 0;

  // Add a 2-d field to the specified level.
  int il = lev;
  
  MudLevel &level = levels[il];
  int nf = level.header.NFields;
  MudField *newfields = new MudField[nf+1];
  
  // Copy the old fields to the new fields.
  {
    for (i = 0; i < nf; i++)
      {
	newfields[i] = level.fields[i];
      }
  }
  newfields[nf].data = new short[header.X.N * header.Y.N];
  
  int mnx = header.X.N;
  int mny = header.Y.N;
  
  int sz = mnx * mny;
  
  {
    // Initialize the new field.
    for (i = 0; i < sz; i++)
      {
	newfields[nf].data[i] = header.MissingData;
      }
    
    if (il == lev)
      {
	// Copy the data into the new field.
	for (j = 0; j < mny; j++)
	  {
	    for (k = 0; k < mnx; k++)
	      {
		newfields[nf].data[k + j * mnx] = a[k + j * nx];
	      }
	  }
      }
  }
  
  // Add the new field.
  strcpy(header.Field[nf].name, name);
  header.Field[nf].scale = scale;
  level.header.NFields++;
  delete[] level.fields;
  level.fields = newfields;

  return(1);
}

int MudFile::map(const float x, const float y, float &lat, float &lon)
{
  // First compute the center lat/lon.
  float clat = header.Lat.Deg + header.Lat.Min / 60.0 +
    header.Lat.Sec / header.SF / 3600.0;
  float clon = header.Lon.Deg + header.Lon.Min / 60.0 +
    header.Lon.Sec / header.SF / 3600.0;

  // We need to convert x/y indices to true x/y in space.
  float sx = (x - header.X0) * header.X.Spacing;
  float sy = (y - header.Y0) * header.Y.Spacing;
  float rot = (float(header.Rotate) / header.CF - 90.0) * (pi / 180.0);

  // Now compute the actual zonal and meridional displacements.
  float sx2 = sin(rot) * sy + cos(rot) * sx;
  float sy2 = -sin(rot) * sx + cos(rot) * sy;

  lat = clat + asin(sy2 / rearth) * 180.0 / pi;
  lon = clon + asin(sx2 / (rearth * cos(lat * pi / 180.0))) * 180.0 / pi;

  return(1);
}

int MudFile::mapInverse(const float tlat, const float tlon, float &x, float &y)
{
  float lat = tlat;
  float lon = tlon;

  // First compute the center lat/lon.
  float clat = header.Lat.Deg + header.Lat.Min / 60.0 +
    header.Lat.Sec / header.SF / 3600.0;
  float clon = header.Lon.Deg + header.Lon.Min / 60.0 +
    header.Lon.Sec / header.SF / 3600.0;
  float rot = (float(header.Rotate) / header.CF - 90.0) * (pi / 180.0);

  float dlat = (lat - clat) * pi / 180.0;
  float dlon = (lon - clon) * pi / 180.0;

  lat *= pi / 180.0;
  lon *= pi / 180.0;

  // Compute zonal (dx) and meridional (dy) displacements of specified
  // point from the origin of the grid.
  float dx = rearth * cos(lat) * sin(dlon);
  float dy = rearth * sin(dlat);

  // Compute the grid x/y displacements.
  float gdx = -dy * sin(rot) + dx * cos(rot);
  float gdy = dy * cos(rot) + dx * sin(rot);

  x = header.X0 + gdx / header.X.Spacing;
  y = header.Y0 + gdy / header.Y.Spacing;

  return(1);
}
