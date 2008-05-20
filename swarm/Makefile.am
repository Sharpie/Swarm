ACLOCAL_AMFLAGS= -I m4

if USEBUILTINAVCALL
SUBDIRS = $(LIBOBJCDIR) avcall etc src tools java COM m4 tests
else
SUBDIRS = $(LIBOBJCDIR) etc src tools java COM m4 tests
endif

EXTRA_DIST = VERSION macosx/buildlibs.sh macosx/ChangeLog macosx/configure.sh macosx/INSTALL.MacOSX macosx/README.MacOSX macosx/swarm.xcode/project.pbxproj

SWARM_INSTALL = @SWARM_INSTALL@

install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(includedir)
	$(INSTALL_DATA) swarmconfig.h $(DESTDIR)$(includedir)
	$(INSTALL_DATA) externvar.h $(DESTDIR)$(includedir)
	$(mkinstalldirs) $(DESTDIR)$(bindir) $(DESTDIR)$(datadir)/swarm
	$(INSTALL_PROGRAM) $(top_builddir)/libtool $(DESTDIR)$(bindir)/libtool-swarm
	if test $(SWARM_INSTALL) = install-sh; then $(INSTALL_PROGRAM) $(srcdir)/install-sh $(DESTDIR)$(bindir); fi

install-recursive: install-data-local

super-clean: clean maintainer-clean
	-rm -f etc/Makefile.in
	-rm -f m4/Makefile.in
	-rm -f COM/Makefile.in
	-rm -f avcall/configure avcall/aclocal.m4
	-rm -rf avcall/autom4te.cache
	-rm -f libobjc/aclocal.m4 libobjc/compiler-info.h libobjc/config.h.in
	-rm -f libobjc/configure libobjc/Makefile.in
	-rm -f libobjc/doc/Makefile.in
	-rm -f libobjc/m4/Makefile.in
	-rm -f libobjc/objc/Makefile.in libobjc/objc/mframe.h
	-rm -rf libobjc/autom4te.cache
	-rm -rf java/c/.libs java/c/_libs
	-rm -f java/c/*.lo
	-rm -f java/c/*.o
	-rm -f java/c/libjavaswarm.la java/c/libjavaswarmstubs.la
	-rm -f java/Makefile.in java/Makefile.common
	-for d in java/c/*.m ; do if test $$d != "java/c/SwarmEnvironmentExtra.m"; then rm -f $$d; fi; done
	-for d in java/c/*.c ; do if test $$d != "java/c/javaswarm.dll.c"; then rm -f $$d; fi; done
	-rm -f java/swarm/*.class java/swarm/package.html java/swarm/swarm.jar
	-rm -f java/swarm/SwarmEnvironment.java
	-rm -f java/swarm/SwarmEnvironmentC.java
	-rm -f java/swarm/SwarmEnvironmentCImpl.java
	-rm -f java/swarm/SwarmEnvironmentImpl.java
	-rm -f java/swarm/SwarmEnvironmentS.java
	-rm -rf java/swarm/activity java/swarm/analysis java/swarm/collections
	-rm -rf java/swarm/defobj java/swarm/gui java/swarm/objectbase
	-rm -rf java/swarm/random java/swarm/simtools java/swarm/simtoolsgui
	-rm -rf java/swarm/space
	-rm -f src/activity/Makefile.in
	-rm -f src/analysis/Makefile.in
	-rm -f src/collections/Makefile.in
	-rm -f src/defobj/Makefile.in src/defobj/modulemap.elc
	-rm -f src/defobj/modulemap src/defobj/modulemap.c
	-rm -f src/gui/Makefile.in
	-rm -f src/misc/Makefile.in
	-rm -f src/objectbase/Makefile.in
	-rm -f src/random/Makefile.in
	-rm -f src/simtools/Makefile.in
	-rm -f src/simtoolsgui/Makefile.in
	-rm -f src/space/Makefile.in
	-rm -f src/swarmgstep/Makefile.in
	-rm -f src/swarm_xmlrpc/Makefile.in
	-rm -f src/tclobjc/Makefile.in
	-rm -f src/tkobjc/Makefile.in src/tkobjc/analysis_tcl.x
	-rm -f src/tkobjc/simtools_tcl.x
	-rm -f src/tkobjc/tk/Makefile.in
	-rm -f src/Makefile.in src/getters.elc
	-rm -f src/SwarmEnvironment_getters.h src/SwarmEnvironment_getters.m
	-rm -f tests/activity/Makefile.in
	-rm -rf tests/activity/tactivity
	-rm -f tests/collections/Makefile.in
	-rm -f tests/collections/deepList.scm tests/collections/deepMap.scm
	-rm -f tests/collections/shallowList.scm tests/collections/shallowMap.scm
	-rm -f tests/defobj/Makefile.in
	-rm -f tests/defobj/deep.scm tests/defobj/shallow.scm
	-rm -f tests/objectbase/Makefile.in
	-rm -f tests/simtools/Makefile.in tests/simtools/File.test
	-rm -f tests/space/Makefile.in
	-rm -f tests/space/objects.scm tests/space/values.scm
	-rm -f tests/Makefile.in
	-rm -f tools/Makefile.in
	-rm -f common.elc protocol.elc interface.elc
	-rm -f Makefile.in aclocal.m4 configure
	-rm -f swarmconfig.h.in stamp-h*
	-rm -f swarm_xmlrpc/Makefile.in
