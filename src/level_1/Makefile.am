# Automake #

AUTOMAKE_OPTIONS = foreign
SUBDIRS = data
INCLUDES = -I../gvslib -Itsdis_toolkit -I$(prefix)/include -I$(prefix)/toolkit/include -DCURR_PROG_VERSION=\"$(PACKAGE)-$(VERSION)\"

LOCAL_LIB = $(top_srcdir)/src/gvslib/.libs/libgvs.a
LDADD = @LIBS@ $(LOCAL_LIB)

bin_PROGRAMS = level_1 level_1_null_granule
level_1_SOURCES = level_1.c level_1_qc.c vosqc.f setvos.f \
                  buildvos.f qc.f intrinsics.c truncate_volume.c \
				  truncate_volume.h vosqc.par 


EXTRA_DIST = CHANGES
