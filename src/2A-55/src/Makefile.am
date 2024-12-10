# Automake #

AUTOMAKE_OPTIONS = foreign
SUBDIRS = tsdis_toolkit geomap
INCLUDES = -I../../gvslib -Itsdis_toolkit -I$(prefix)/include -I$(prefix)/toolkit/include -DPROG_VERSION=\"$(PACKAGE)-$(VERSION)\"

LOCAL_LIB = $(top_srcdir)/src/gvslib/.libs/libgvs.a
LDADD = @LIBS@ $(LOCAL_LIB)

bin_PROGRAMS = a2A55

bin_SCRIPTS = $(scripts)

# Hmmm, can't start a variable name w/ a number!  I create install-exec-hook.
# This renames the installed binary to 2A55

a2A55_SOURCES = \
	ANALhist_cfad.cc \
	ANALmean_prof.cc \
	READgauge.cc \
	READgeogr.cc \
	READparam.cc \
	PREPvalid_range.cc \
	VOLio.cc \
	VOLlifecycle.cc \
	mudras.cc \
	2A55.cc \
	output_hdf.cc \
	write_grid_to_hdf.c \
	2A55.h exitCodes.h mudras.h volume.h

perl_scripts = 2A-55 singlevos_2A-55
scripts = $(perl_scripts)

datadir = $(prefix)/data
data_DATA = 2A55_metadata.txt

install-exec-hook:
	mv $(bindir)/a2A55 $(bindir)/2A55

2A-55: Makefile
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

EXTRA_DIST = tsdistk_2A-55_error_msg.txt $(scripts) $(data_DATA)
