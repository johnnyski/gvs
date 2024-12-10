AUTOMAKE_OPTIONS = foreign

datadir = $(prefix)/data
data_DATA = $(gzdata:.gz=)

gzdata = \
darw_rainest.par.gz  poltocart.FL.gz \
carttopol.DR.gz      melb_rainest.par.gz  \
carttopol.FL.gz      poltocart.DR.gz      

$(gzdata:.gz=): $(gzdata)
	gunzip -c $@.gz > $@

CLEANFILES = $(gzdata:.gz=)

EXTRA_DIST = $(gzdata)

