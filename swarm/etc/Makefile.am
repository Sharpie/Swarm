EXTRA_DIST = config.swarm.in Makefile.common.in Makefile.appl.in Makefile.lib.in Makefile.rules Makefile.tests.in common.el protocol.el interface.el Makefile.protocol modules swarmrc.in swarmtrc.in swarm.make

install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(sysconfdir)/swarm
	$(INSTALL_DATA) config.swarm $(DESTDIR)$(sysconfdir)/swarm
	$(INSTALL_DATA) Makefile.appl $(DESTDIR)$(sysconfdir)/swarm
	$(INSTALL_DATA) Makefile.lib $(DESTDIR)$(sysconfdir)/swarm
	$(INSTALL_DATA) Makefile.common $(DESTDIR)$(sysconfdir)/swarm
	$(INSTALL_DATA) $(srcdir)/Makefile.rules $(DESTDIR)$(sysconfdir)/swarm
	$(INSTALL_DATA) $(srcdir)/swarm.make $(DESTDIR)$(sysconfdir)/swarm
	$(INSTALL_DATA) swarmrc $(DESTDIR)$(sysconfdir)/swarm
	$(INSTALL_DATA) swarmtrc $(DESTDIR)$(sysconfdir)/swarm
