SUBDIRS = src m4 etc
etcdir = $(prefix)/etc

EXTRA_DIST = VERSION

install-data-local:
	$(INSTALL) Makefile.appl Makefile.lib $(etcdir)
	$(INSTALL) swarmconfig.h $(includedir)

