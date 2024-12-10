# Automake #

AUTOMAKE_OPTIONS = foreign
SUBDIRS = tsdis_toolkit
INCLUDES = -I$(prefix)/include -I$(prefix)/toolkit/include -DPROG_VERSION=\"$(PACKAGE)-$(VERSION)\"

LOCAL_LIB = $(top_srcdir)/src/gvslib/.libs/libgvs.a
LDADD = @LIBS@ $(LOCAL_LIB)

bin_PROGRAMS = \
 2A-54_hdf2ascii \
 level_1_ghdf_to_singlevos_uf \
 level_2_ghdf_to_singlevos_hdf \
 level_2_singlevos_hdf_to_ghdf \
 level_3_rrmap_hdf2ascii \
 2A-53_hdf2ascii \
 2A-55_hdf2ascii

bin_SCRIPTS = $(scripts)

perl_scripts = uf2mudras 2A-53or2A-55 2A-53 level_2_ghdf_to_singlevos_ascii
scripts = $(perl_scripts)

format_conversiondir = $(prefix)/data
format_conversion_DATA = sprint_entire_volume.inp sprint_base_scan.inp

uf2mudras: Makefile
	@for p in $(perl_scripts); do \
	   echo -n "Checking file $$p ... ";\
	   cp $$p $$p.in;\
	   sed -e 's&PROG_VERSION[ ]*=.*&PROG_VERSION = \"$(PACKAGE)-$(VERSION)\";&' \
		-e 's&#\!.*perl&#\! $(perlbin)&' \
		-e 's&(\@INC,.*&(\@INC, \".\", \"$(bindir)\"\)\;&' \
	   < $$p.in > $$p.new;\
	   if cmp -s $$p $$p.new; then \
          rm $$p.new; \
          echo "$$p remains untouched."; \
       else \
          mv $$p.new $$p; \
          echo "substitutions made in $$p."; \
       fi;\
	   rm -f $$p.in;\
    done

EXTRA_DIST = $(format_conversion_DATA) CHANGES $(scripts)


