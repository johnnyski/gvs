# Process w/ automake or 'autoreconf; make' 

AUTOMAKE_OPTIONS = foreign

SUBDIRS = tsdis_toolkit

INCLUDES = -I$(prefix)/include -I$(prefix)/toolkit/include \
           -DPROG_VERSION=\"$(PACKAGE)-$(VERSION)\"

LOCAL_LIB = $(top_srcdir)/src/gvslib/.libs/libgvs.a
LDADD = @LIBS@ $(LOCAL_LIB) 

# Hmmm, can't start a variable name w/ a number!  I create a redirect target.
bin_PROGRAMS = a2A-52i
bin_SCRIPTS = $(scripts)

a2A_52i_SOURCES = 2A-52i.c

perl_scripts = process_new_closest_vos_in_the_previous_granule \
               sort_2A52_and_remove_dups
scripts = $(perl_scripts)

sort_2A52_and_remove_dups: Makefile
	@for p in $(perl_scripts); do \
	   echo -n "Checking file $$p ... ";\
	   cp $$p $$p.in;\
	   sed -e 's&PROG_VERSION =.*&PROG_VERSION = \"$(PACKAGE)-$(VERSION)\";&' \
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
	mv $(bindir)/a2A-52i $(bindir)/2A-52i 

EXTRA_DIST = CHANGES CHECK_LIST $(scripts)
