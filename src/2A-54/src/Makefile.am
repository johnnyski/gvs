# Automake #

AUTOMAKE_OPTIONS = foreign
SUBDIRS = tsdis_toolkit
INCLUDES = -I../../gvslib -Itsdis_toolkit -I$(prefix)/include -I$(prefix)/toolkit/include -DPROG_VERSION=\"$(PACKAGE)-$(VERSION)\"

LOCAL_LIB = $(top_srcdir)/src/gvslib/.libs/libgvs.a
LDADD = @LIBS@ $(LOCAL_LIB)

bin_PROGRAMS = a2A54 singlevos_2A-54-dc
bin_SCRIPTS = $(scripts)

# Hmmm, can't start a variable name w/ a number!  I create install-exec-hook.
# This renames the installed binary to 2A54.

a2A54_SOURCES = \
    ANALconv_stra.cc \
	PREPvalid_range.cc \
	READgauge.cc \
	READgeogr.cc \
	READparam.cc \
	VOLfindPoints.cc \
	VOLlifecycle.cc \
	VOLio.cc \
	2A54.cc \
	mudras.cc \
    write_csmap2hdf.c \
    2A54.h exitCodes.h findPoints.h mudras.h volume.h

singlevos_2A_54_dc_SOURCES = \
    ANALconv_stra.cc \
	PREPvalid_range.cc \
	READgauge.cc \
	READgeogr.cc \
	READparam.cc \
	VOLfindPoints.cc \
	VOLlifecycle.cc \
	VOLio.cc \
	mudras.cc \
	singlevos_2A-54-dc.cc \
        write_csmap2hdf.c \
    2A54.h exitCodes.h findPoints.h mudras.h volume.h



perl_scripts = 2A-54 singlevos_2A-54 2A-54-dc
scripts = $(perl_scripts)

filesdir = $(prefix)/data
files_DATA = 2A54_metadata.txt

install-exec-hook:
	mv $(bindir)/a2A54 $(bindir)/2A54 

2A-54: Makefile
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

EXTRA_DIST = $(files_DATA) 2A54_metadata.txt \
             tsdistk_2A-54_error_msg.txt $(scripts)
