include $(top_srcdir)/Makefile.common

MODULES = defobj collections activity objectbase random simtools gui simtoolsgui space analysis

.PHONY: all stubs

java-stub.elc: $(swarm_dossrcdir)/java/java-stub.el protocol.elc
	$(elcrule)

stubs: java-stub.elc $(foreach module,$(MODULES),$(swarm_dossrcdir)/src/$(module)/$(module).h)
	SWARMSRCDIR=$(swarm_dossrcdir) $(EMACS) -batch -l $(abs_builddir)/java-stub.elc -f java-run-all-javadoc

all: stubs
	@JAVADOC@ -public -sourcepath .:$(swarm_dossrcdir)/java/swarm $(foreach module, $(MODULES),swarm.$(module)) $(swarm_dossrcdir)/java/swarm/*.java
