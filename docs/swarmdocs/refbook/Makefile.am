NAME = refbook
MODULES = activity analysis collections defobj gui objectbase random simtools simtoolsgui space
SUBDIRS = $(MODULES) tech

swarm_srcdir = @swarm_srcdir@
gridturtle_srcdir = @gridturtle_srcdir@
swarm_ChangeLog =

include $(top_srcdir)/Makefile.rules
include $(top_srcdir)/refbook/Makefile.rules

PAGES = activity/activitypages.sgml \
	analysis/analysispages.sgml \
	collections/collectionspages.sgml \
	defobj/defobjpages.sgml \
	gui/guipages.sgml \
	objectbase/objectbasepages.sgml \
	random/randompages.sgml \
	simtools/simtoolspages.sgml \
	simtoolsgui/simtoolsguipages.sgml \
	space/spacepages.sgml

GENERATED_SGML = $(PAGES) refindex.sgml refbookrevhistory.sgml
nodist_noinst_DATA = $(GENERATED_SGML)

ENT = refbook.ent $(foreach module,$(SUBDIRS),$(module)/$(module).ent)

SGML = conventions-app.sgml license-app.sgml probes-app.sgml ref00.sgml ref00a.sgml refbook.sgml refcont.sgml refmeta.sgml grid-app.sgml

EXTRA_DIST = Makefile.rules refbook.ent sgml.el graph.el $(SGML)

SGML_FILES = $(SGML) $(ENT) $(GENERATED_SGML)

sgml.elc: $(abs_top_builddir)/common.elc $(abs_top_builddir)/protocol.elc

refindex.sgml $(PAGES): $(swarm_srcdir)/src/activity/activity.h \
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
