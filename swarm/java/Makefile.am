EXTRA_DIST = java-stub.el

abs_top_builddir := $(shell cd $(top_builddir); pwd)
abs_builddir := $(shell pwd)
EMACS = TOP_BUILDDIR=$(abs_top_builddir)/ BUILDDIR=$(abs_builddir) @EMACS@

define elcrule
cp -f $< $(dir $@)_$(notdir $<)
$(EMACS) -batch -f batch-byte-compile $(dir $@)_$(notdir $<)
mv $(dir $@)_$(notdir $@) $@
$(RM) $(dir $@)_$(notdir $<)
endef

MODULES = defobj collections activity objectbase random simtools gui simtoolsgui space analysis

SUBMAKEFILES = $(foreach module,$(MODULES),swarm/$(module)/Makefile)

.PHONY: all stubs

%.elc: $(top_srcdir)/etc/%.el
	$(elcrule)

$(abs_top_builddir)/%.elc: $(top_srcdir)/etc/%.el
	$(elcrule)

%.elc: %.el
	$(elcrule)

protocol.elc: $(abs_top_builddir)/common.elc
protocol.elc: $(top_srcdir)/etc/protocol.el

java-stub.elc: protocol.elc

all: stubs
	$(MAKE) -C c all
	$(MAKE) -C swarm all

# although this excludes all the Java and C stubs, it is useful
# because it makes makefile regeneration automatic.
stubs: Makefile.common $(SUBMAKEFILES)

Makefile.common $(SUBMAKEFILES): java-stub.elc $(foreach module,$(MODULES),$(top_srcdir)/src/$(module)/$(module).h)
	$(EMACS) -batch -l ./java-stub.elc -f java-run-all
