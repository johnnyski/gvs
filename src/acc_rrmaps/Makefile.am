AUTOMAKE_OPTIONS = foreign

# Install scripts as programs.  Must have a letter as the first character.
# That is an automake restriction.  But, see install-exec-hook, it's renamed
# upon install.
SUBDIRS = tsdis_toolkit
INCLUDES = -I../gvslib -I$(prefix)/include -I$(prefix)/toolkit/include -DPROG_VERSION=\"$(PACKAGE)-$(VERSION)\"

LOCAL_LIB = $(top_srcdir)/src/gvslib/.libs/libgvs.a
LDADD = @LIBS@ $(LOCAL_LIB)

bin_PROGRAMS = acc_rrmaps_single_vos_ascii \
		       create_level_3_rrmap_hdf
 
bin_SCRIPTS = $(scripts)

acc_rrmaps_single_vos_ascii_SOURCES = acc_maps.cc arrayutil.cc assortutil.cc \
									  defs.h

scripts = acc_rrmaps_granule_hdf

acc_rrmaps_granule_hdf: Makefile
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

EXTRA_DIST = CHANGES tsdistk_acc_rrmaps_error_msg.txt $(scripts)
