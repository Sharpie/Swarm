EXTRA_DIST = config.swarm.in Makefile.common.in Makefile.appl.in Makefile.lib.in Makefile.rules Makefile.tests.in common.el protocol.el java-stub.el

install-data-local:
	$(mkinstalldirs) $(sysconfdir)/swarm
	$(INSTALL_DATA) config.swarm $(sysconfdir)/swarm
	$(INSTALL_DATA) Makefile.appl $(sysconfdir)/swarm
	$(INSTALL_DATA) Makefile.lib $(sysconfdir)/swarm
	$(INSTALL_DATA) Makefile.common $(sysconfdir)/swarm
	$(INSTALL_DATA) $(srcdir)/Makefile.rules $(sysconfdir)/swarm
