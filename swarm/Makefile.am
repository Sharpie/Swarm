if USEBUILTINAVCALL
SUBDIRS = libobjc avcall src tools java COM m4 etc tests
else
SUBDIRS = libobjc src tools java COM m4 etc tests
endif

EXTRA_DIST = VERSION

SWARM_INSTALL = @SWARM_INSTALL@

install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(includedir)
	$(INSTALL_DATA) swarmconfig.h $(DESTDIR)$(includedir)
	$(INSTALL_DATA) externvar.h $(DESTDIR)$(includedir)
	$(mkinstalldirs) $(DESTDIR)$(bindir) $(DESTDIR)$(datadir)/swarm
	$(INSTALL_PROGRAM) $(top_builddir)/libtool $(DESTDIR)$(bindir)/libtool-swarm
	if test $(SWARM_INSTALL) = install-sh; then $(INSTALL_PROGRAM) $(srcdir)/install-sh $(DESTDIR)$(bindir); fi

install-recursive: install-data-local
