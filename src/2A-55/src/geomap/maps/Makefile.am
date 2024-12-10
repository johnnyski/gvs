AUTOMAKE_OPTIONS = foreign

binmapsdir = $(prefix)/data

binmaps_DATA = $(gzmaps:.gz=.bin)

# List the original names (*.gz), then all derivations for targets.
gzmaps = \
darw_landsea_new.map.gz \
hstn_landsea_new.map.gz \
fla_landsea_new.map.gz \
melb_landsea_new.map.gz

asciimaps = $(gzmaps:.gz=.ascii)
maps = $(gzmaps:.gz=)

$(maps): $(gzmaps)
	gunzip -c $@.gz > $@

$(binmaps_DATA): %.bin : %
	../mkgeomap $<

CLEANFILES = $(asciimaps) $(binmaps_DATA) $(maps)

EXTRA_DIST = $(gzmaps)
