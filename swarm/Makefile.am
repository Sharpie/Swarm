SUBDIRS = src m4 etc

EXTRA_DIST = VERSION

SWARM_INSTALL = @SWARM_INSTALL@

install-data-local:
	$(INSTALL_DATA) swarmconfig.h $(includedir)
	$(mkinstalldirs) $(bindir) $(datadir)/swarm
	$(INSTALL_PROGRAM) $(top_builddir)/libtool $(bindir)/libtool-swarm
	if test $(SWARM_INSTALL) = install-sh; then $(INSTALL_PROGRAM) $(srcdir)/install-sh $(bindir); fi

