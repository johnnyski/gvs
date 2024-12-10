# Process w/ automake.  Or, autoreconf; make #
AUTOMAKE_OPTIONS = foreign

INCLUDES = -I. -I$(prefix)/include -I$(prefix)/toolkit/include 

lib_LTLIBRARIES = libgvs.la

libgvs_la_LDFLAGS = -version-info 4:20
libgvs_la_SOURCES = \
            gvs_sat_coin.c gvs_metadata.c \
            $(headers_DATA)

libdir = $(prefix)/lib
includedir = $(prefix)/include

headersdir = $(includedir)
headers_DATA = gvs_metadata.h gvs_sat_coin.h

gvs_metadata.h: Makefile
	@for p in $(headers_DATA); do \
	   echo -n "Checking file $$p ... ";\
	   cp $$p $$p.in;\
	   sed -e 's&SOFTWARE_VERSION.*&SOFTWARE_VERSION  \"$(PACKAGE)-$(VERSION)\"&' \
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

EXTRA_DIST = CHANGES

