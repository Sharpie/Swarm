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

%.elc: $(top_srcdir)/etc/%.el
	$(elcrule)

$(abs_top_builddir)/%.elc: $(top_srcdir)/etc/%.el
	$(elcrule)

%.elc: %.el
	$(elcrule)

protocol.elc: $(abs_top_builddir)/common.elc
protocol.elc: $(top_srcdir)/etc/protocol.el

java-stub.elc: protocol.elc

stubs: java-stub.elc
	$(EMACS) -batch -l ./java-stub.elc -f java-run-all