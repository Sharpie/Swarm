C_EXTRA_DIST = c/Makefile.in c/SwarmEnvironment.m c/ChangeLog c/javaswarm.dll.c
KAWAJAR=@KAWAJAR@
SWARM_EXTRA_DIST = swarm/Makefile.rules.in swarm/Makefile.in swarm/ChangeLog swarm/Selector.java swarm/NonUniqueMethodSignatureException.java swarm/SignatureNotFoundException.java swarm/PhaseCImpl.java swarm/Primitives.java swarm/SwarmEnvironment.java swarm/Globals.java swarm/CustomizedType.java JavaDeps.jar $(KAWAJAR)
JAVASWARMSCRIPTS=@JAVASWARMSCRIPTS@
EXTRA_SCRIPTS = javacswarm.in javaswarm.in jdkcswarm jdkswarm
EXTRA_DIST = java-stub.el $(C_EXTRA_DIST) $(SWARM_EXTRA_DIST) $(EXTRA_SCRIPTS)
EMACS = @EMACS@

 include $(top_srcdir)/etc/Makefile.protocol

SUBMAKEFILES = $(foreach module,$(MODULES),swarm/$(module)/Makefile)

.PHONY: all stubs

bin_SCRIPTS = $(JAVASWARMSCRIPTS)

java-stub.elc: $(abs_top_builddir)/protocol.elc $(abs_top_builddir)/interface.elc

JAVASTUBS=@JAVASTUBS@
JAVASTUBS_FUNCTION=@JAVASTUBS_FUNCTION@
stubs_ALL=all-stubs
stubs_INSTALL=install-stubs


all: $($(JAVASTUBS)_ALL)
install: $($(JAVASTUBS)_INSTALL)
	test -n "$(KAWAJAR)" && $(INSTALL) $(srcdir)/$(KAWAJAR) $(datadir)/swarm/kawa.jar

all-stubs: stubs
	$(MAKE) -C c all
	$(MAKE) -C swarm all

install-stubs: all
	$(MAKE) -C c install
	$(MAKE) -C swarm install

stubs: Makefile.common

# although this excludes all the Java and C stubs, it is useful
# because it makes makefile regeneration automatic.
Makefile.common: java-stub.elc $(foreach module,$(MODULES),$(top_srcdir)/src/$(module)/$(module).h)
	$(ENVEMACS) -batch -l '$(dos_abs_builddir)@PATHDELIM@java-stub.elc' -f $(JAVASTUBS_FUNCTION)

