SUBDIRS = libobjc @AVCALLDIR@ src java m4 etc tests

EXTRA_DIST = VERSION avcall

SWARM_INSTALL = @SWARM_INSTALL@

install-data-local:
	$(INSTALL_DATA) swarmconfig.h $(includedir)
	$(INSTALL_DATA) externvar.h $(includedir)
	$(mkinstalldirs) $(bindir) $(datadir)/swarm
	$(INSTALL_PROGRAM) $(top_builddir)/libtool $(bindir)/libtool-swarm
	if test $(SWARM_INSTALL) = install-sh; then $(INSTALL_PROGRAM) $(srcdir)/install-sh $(bindir); fi

