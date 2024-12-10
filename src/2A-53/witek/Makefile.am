# Automake #

AUTOMAKE_OPTIONS = foreign
SUBDIRS = . data
INCLUDES = -Itsdis_toolkit -I$(prefix)/include -I$(prefix)/toolkit/include -DPROG_VERSION=\"$(PACKAGE)-$(VERSION)\"

bin_PROGRAMS = out.x rainest prep_proc
bin_SCRIPTS = $(scripts)

out_x_SOURCES = \
        nrainest.c\
        settozero.c\
        products.c\
        accumulation.c\
        advective_accum.c\
        instfield.c\
        readray.c\
        poltocart.c\
        carttopol.c\
        writeminute.c\
        writeday.c\
        write5day.c\
        writemonth.c \
        compress.c error.c encode.c io.c \
        write_rrmap2hdf.c trap_signals.c \
		rainest.h classmap.h encode.h

prep_proc_SOURCES = prep_proc.c julday.c \
		compress.c error.c encode.c io.c trap_signals.c \
		aiw.h encode.h error.h compress.h

rainest_SOURCES = rainest.c trap_signals.c \
				  rainest.h


scripts = $(perl_scripts)
perl_scripts = singlevos_2A-53-w

singlevos_2A-53-w : Makefile
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
