AUTOMAKE_OPTIONS = foreign

# Install scripts as programs.  Must have a letter as the first character.
# That is an automake restriction.  But, see install-exec-hook, it's renamed
# upon install.

bin_SCRIPTS = $(scripts)

scripts = 3A-54

filedir = $(prefix)/data
file_DATA = 3A54_metadata.txt

3A-54: Makefile
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

EXTRA_DIST = CHANGES $(file_DATA) $(scripts)