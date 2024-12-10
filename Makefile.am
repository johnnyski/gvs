# Process w/ automake or 'autoreconf; make' 

AUTOMAKE_OPTIONS = foreign

SUBDIRS = src script format_conversion doc

# Install README and CHANGES as documentation.
docdir = $(prefix)/doc/$(PACKAGE)
doc_DATA = $(html_doc)

html_doc = README CHANGES

EXTRA_DIST = CHANGES Copyright GPL 