C_EXTRA_DIST = c/Makefile.in c/SwarmEnvironmentExtra.m c/ChangeLog c/javaswarm.dll.c
KAWAJAR=@KAWAJAR@
SWARM_EXTRA_DIST = swarm/Makefile.rules.in swarm/Makefile.in swarm/ChangeLog $(SWARM_JAVA_BASIC) $(SWARM_JAVA_EXTRA)
SWARM_JAVA_BASIC = swarm/Selector.java swarm/NonUniqueMethodSignatureException.java swarm/SignatureNotFoundException.java swarm/PhaseCImpl.java swarm/Primitives.java swarm/Globals.java swarm/CustomizedType.java swarm/ObjCProxy.java 
SWARM_JAVA_EXTRA = swarm/ProxyClassLoader.java $(KAWAJAR) JavaDeps.jar
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
	test -z "$(KAWAJAR)" || $(INSTALL) $(srcdir)/$(KAWAJAR) $(datadir)/swarm/kawa.jar

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

JAVA_SRC = $(addprefix $(srcdir)/,$(SWARM_JAVA_BASIC)) $(wildcard swarm/*.java) $(foreach module,$(MODULES),$(wildcard swarm/$(module)/*.java))

gcjswarm.so:
	gcj -fPIC -shared -o gcjswarm.so --encoding=UTF-8 -fjni -g -O2 $(JAVA_SRC)

JAVA_OBJS = $(patsubst %.java,%.lo,$(notdir $(JAVA_SRC)))

$(JAVA_OBJS): $(JAVA_SRC)
	for i in $(JAVA_SRC); do libtool --mode=compile gcj -O2 -g -fjni -c $$i; done

libgcjswarm.la: $(JAVA_OBJS)
	libtool --mode=link gcc $(JAVA_OBJS) -o libgcjswarm.la -rpath $(libdir) -Lc -ljavaswarm
