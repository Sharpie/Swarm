NAME = refbook
SUBDIRS = activity analysis collections defobj gui objectbase random simtools simtoolsgui space

include $(top_srcdir)/Makefile.rules

CLEANFILES = *.htm $(NAME).tex _$(NAME).tex refbook.rtf

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

noinst_DATA = $(PAGES)

SGML = conventions-app.sgml gpl-app.sgml grid-app.sgml lgpl-app.sgml probes-app.sgml ref00.sgml ref00a.sgml refbook.sgml refcont.sgml refindex.sgml refmeta.sgml 

SGML_FILES = $(SGML) $(PAGES)

$(PAGES): $(SWARMHOME)/src/activity/activity.h \
	$(SWARMHOME)/src/analysis/analysis.h \
	$(SWARMHOME)/src/collections/collections.h \
	$(SWARMHOME)/src/defobj/defobj.h \
	$(SWARMHOME)/src/gui/gui.h \
	$(SWARMHOME)/src/objectbase/objectbase.h \
	$(SWARMHOME)/src/random/random.h \
	$(SWARMHOME)/src/simtools/simtools.h \
	$(SWARMHOME)/src/simtoolsgui/simtoolsgui.h \
	$(SWARMHOME)/src/space/space.h
	SWARMDOCS_BUILD_AREA=$(top_builddir) $(EMACS) -batch -l $(top_srcdir)/protocol.el -f run-all $(SWARMHOME)
