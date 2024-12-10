AUTOMAKE_OPTIONS = foreign

# Install scripts as programs.  Must have a letter as the first character.
# That is an automake restriction.  But, see install-exec-hook, it's renamed
# upon install.
SUBDIRS = tsdis_toolkit
INCLUDES = -I../gvslib -I$(prefix)/include -I$(prefix)/toolkit/include -DPROG_VERSION=\"$(PACKAGE)-$(VERSION)\"

LOCAL_LIB = $(top_srcdir)/src/gvslib/.libs/libgvs.a
LDADD = @LIBS@ $(LOCAL_LIB)

# Hmm, can't start a target w/ a number!  I create install-exec-hook to
# rename some programs.
bin_PROGRAMS = \
        a3A-55_ascii2hdf \
        level_3_accu_vprof \
        level_3_mean_vprof \
        level_3_accu_cfad \
        level_2_dump_metadata \
        level_3_finalize_metadata

bin_SCRIPTS = $(scripts)

a3A_55_ascii2hdf_SOURCES = 3A-55_ascii2hdf.c 3A55.h
 
scripts = 3A-55 level_3_accu_2A-55

filedir = $(prefix)/data
file_DATA = 3A55_metadata.txt

3A-55: Makefile
	@for p in $(scripts); do \
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

install-exec-hook:
	mv $(bindir)/a3A-55_ascii2hdf $(bindir)/3A-55_ascii2hdf

EXTRA_DIST = $(file_DATA) 3A55_metadata.txt CHANGES $(scripts)
