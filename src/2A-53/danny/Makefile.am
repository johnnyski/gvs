AUTOMAKE_OPTIONS = foreign

INCLUDES = -I../../gvslib -Itsdis_toolkit -I$(prefix)/include -I$(prefix)/toolkit/include -DPROG_VERSION=\"$(PACKAGE)-$(VERSION)\"

bin_PROGRAMS = a2A53d
bin_SCRIPTS = $(scripts)

# Automake can't handle targets begining w/ a digit.  See install-exec-hook.
a2A53d_SOURCES = 2A53.c voswin.f rdvosp.f setvos.f buildvos.f \
                grad.f mds.f readzr.f swin.f \
                cshouze.f bbf.f gwin.f pol2cart.f intrinsics.c julian.c \
                getzh.f sort.f write_rrmap2hdf.c \
				voswin.par 

cshouze.o: cshouze.f


filesdir = $(prefix)/data
files_DATA = zr_mlb.out zr_drw.out mlb_sitelist sitelist_cscale

scripts = $(perl_scripts)
perl_scripts =  singlevos_2A-53-d

install-exec-hook:
	mv $(bindir)/a2A53d $(bindir)/2A53d 

singlevos_2A-53-d: Makefile
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

EXTRA_DIST = $(files_DATA) CHANGES p_drw.pl p_mlb.pl $(scripts)