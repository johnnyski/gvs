# Automake #

AUTOMAKE_OPTIONS = foreign

INCLUDES = -I../gvslib -Itsdis_toolkit -I$(prefix)/include -I$(prefix)/toolkit/include -DPROG_VERSION=\"$(PACKAGE)-$(VERSION)\"

LOCAL_LIB = $(top_srcdir)/src/gvslib/.libs/libgvs.a
LDADD = @LIBS@ $(LOCAL_LIB)

bin_PROGRAMS = \
 get_radar_name_from_file \
 date2pentad_number \
 pentad_number2begin_date \
 level_2_create_empty_granule_hdf \
 get_info_from_file

EXTRA_DIST = CHANGES
