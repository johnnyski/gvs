# Automake #

AUTOMAKE_OPTIONS = foreign

INCLUDES = -I../danny -I../../gvslib -Itsdis_toolkit -I$(prefix)/include -I$(prefix)/toolkit/include -DPROG_VERSION=\"$(PACKAGE)-$(VERSION)\"

# Can't have a target that begins w/ a number -- I redirect.
bin_PROGRAMS = a2A53c
bin_SCRIPTS = $(scripts)

a2A53c_SOURCES = 2A53c.c

scripts = $(perl_scripts)
perl_scripts = singlevos_2A-53-c

install-exec-hook:
	mv $(bindir)/a2A53c $(bindir)/2A53c

singlevos_2A-53-c: Makefile
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

EXTRA_DIST = CHANGES $(scripts)
