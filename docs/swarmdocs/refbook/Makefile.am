NAME = refbook
MODULES = activity analysis collections defobj gui objectbase random simtools simtoolsgui space
SUBDIRS = $(MODULES)

swarm_srcdir = @swarm_srcdir@
gridturtle_srcdir = @gridturtle_srcdir@

include $(top_srcdir)/Makefile.rules
include $(top_srcdir)/refbook/Makefile.rules

PAGES = activity/activitypages.xml \
	analysis/analysispages.xml \
	collections/collectionspages.xml \
	defobj/defobjpages.xml \
	gui/guipages.xml \
	objectbase/objectbasepages.xml \
	random/randompages.xml \
	simtools/simtoolspages.xml \
	simtoolsgui/simtoolsguipages.xml \
	space/spacepages.xml \
	swarmpages.xml

GENERATED_XML = $(PAGES) refindex.xml refbookrevhistory.xml
nodist_noinst_DATA += $(GENERATED_XML)

ENT = refbook.ent $(foreach module,$(SUBDIRS),$(module)/$(module).ent)

XML = conventions-app.xml license-app.xml probes-app.xml ref00.xml ref00a.xml refbook.xml refcont.xml swarmcont.xml refmeta.xml grid-app.xml

EXTRA_DIST += Makefile.rules refbook.ent sgml.el graph.el $(XML)

XML_FILES = $(XML) $(ENT) $(GENERATED_XML)

sgml.elc: $(abs_top_builddir)/common.elc $(abs_top_builddir)/protocol.elc

refindex.xml $(PAGES): $(swarm_srcdir)/src/activity/activity.h \
	$(swarm_srcdir)/src/analysis/analysis.h \
	$(swarm_srcdir)/src/collections/collections.h \
	$(swarm_srcdir)/src/defobj/defobj.h \
	$(swarm_srcdir)/src/gui/gui.h \
	$(swarm_srcdir)/src/objectbase/objectbase.h \
	$(swarm_srcdir)/src/random/random.h \
	$(swarm_srcdir)/src/simtools/simtools.h \
	$(swarm_srcdir)/src/simtoolsgui/simtoolsgui.h \
	$(swarm_srcdir)/src/space/space.h \
	$(abs_top_builddir)/common.elc $(abs_top_builddir)/protocol.elc sgml.elc
	SWARMSRCDIR='$(swarm_dossrcdir)' SWARMDOCS='$(top_dossrcdir)/' $(EMACS) -batch -l sgml.elc -f run-all $(swarm_dossrcdir)

graph: $(abs_top_builddir)/common.elc $(abs_top_builddir)/protocol.elc graph.elc
	SWARMSRCDIR='$(swarm_dossrcdir)' SWARMDOCS='$(top_dossrcdir)/' $(EMACS) -batch -l graph.elc -f create-graphs
	for m in $(MODULES); do dot -Tps $$m.dot > $$m.ps ; dot -Tps $${m}-singlepage.dot > $${m}-singlepage.ps ; done
	dot -Tps protocols.dot > protocols.ps

# Local variables:
# mode: makefile
# end:
