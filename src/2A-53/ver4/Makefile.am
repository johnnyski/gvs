# Automake #

AUTOMAKE_OPTIONS = foreign
SUBDIRS = tsdis_toolkit
INCLUDES = -I../../gvslib -Itsdis_toolkit -I$(prefix)/include -I$(prefix)/toolkit/include -DPROG_VERSION=\"$(PACKAGE)-$(VERSION)\"

LOCAL_LIB = $(top_srcdir)/src/gvslib/.libs/libgvs.a
LDADD = @LIBS@ $(LOCAL_LIB)

bin_PROGRAMS = singlevos_2A-53-v4

singlevos_2A_53_v4_SOURCES = singlevos_2A-53-v4.c 2A53.h zr_table.h \
  interpolate.cc zr.c  mudras.cc write_rrmap2hdf_file.c mudras.h


filesdir = $(prefix)/data
files_DATA = dual.zr single.zr runtime_2A-53_????.txt 2A53_metadata.txt \
       darw_Dec1997.zr hstn_Feb1998.zr kwaj_Dec1997.zr kwaj_Jan1998.zr \
       melb_Feb1998.zr kwaj_Dec1997Jan1998.zr \
       KAMA_05-1994_FACE_uniform.zr  KLSX_07-1993_FACE_uniform.zr

EXTRA_DIST = $(files_DATA) 2A53_metadata.txt toolkit_error_msg.txt
