# Automake #

AUTOMAKE_OPTIONS = foreign
SUBDIRS = tsdis_toolkit
INCLUDES = -I../../gvslib -Itsdis_toolkit -I$(prefix)/include -I$(prefix)/toolkit/include -DPROG_VERSION=\"$(PACKAGE)-$(VERSION)\"

LOCAL_LIB = $(top_srcdir)/src/gvslib/.libs/libgvs.a
LDADD = @LIBS@ $(LOCAL_LIB)

bin_PROGRAMS = singlevos_2A-53-dc

singlevos_2A_53_dc_SOURCES = singlevos_2A-53-dc.c 2A53.h zr_table.h \
  zr.c write_rrmap2hdf_file.c


filesdir = $(prefix)/data
EXTRA_DIST = toolkit_error_msg.txt
