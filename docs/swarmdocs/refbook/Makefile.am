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

GENERATED_SGML = $(PAGES) gridexamples.sgml refindex.sgml

noinst_DATA = $(GENERATED_SGML)

SGML = conventions-app.sgml gpl-app.sgml grid-app.sgml lgpl-app.sgml probes-app.sgml ref00.sgml ref00a.sgml refbook.sgml refcont.sgml refmeta.sgml 

SGML_FILES = $(SGML) $(GENERATED_SGML)

EXAMPLES=GridTurtle.h GridTurtle.m Makefile Mousetrap.h Mousetrap.m \
        Mousetrap2.m grid.h grid.m grid0.m grid1a.m grid1b.m grid1c.m\
        grid2.m grid2b.m grid3.m grid3b.m grid4.m  grid4b.m grid5.m \
        grid6.m grid7.m grid8.m grid9.m mousetraps.m mousetraps2.m \
        strtest.m 

gridexamples.sgml: $(addprefix $(srcdir)/grid/,$(EXAMPLES))
	- (cd $(srcdir)/grid ; (for i in $(EXAMPLES); do echo "<EXAMPLE ID=\"SWARM.SRC.GRID.$$i\">"; echo "<TITLE>$$i</TITLE>"; echo "<PROGRAMLISTING>"; echo '<![ CDATA [' ; cat $$i ; echo ']]>'; echo "</PROGRAMLISTING>"; echo "</EXAMPLE>";  done) ; cd ..) > $@

refindex.sgml $(PAGES): $(SWARMHOME)/src/activity/activity.h \
	$(SWARMHOME)/src/analysis/analysis.h \
	$(SWARMHOME)/src/collections/collections.h \
	$(SWARMHOME)/src/defobj/defobj.h \
	$(SWARMHOME)/src/gui/gui.h \
	$(SWARMHOME)/src/objectbase/objectbase.h \
	$(SWARMHOME)/src/random/random.h \
	$(SWARMHOME)/src/simtools/simtools.h \
	$(SWARMHOME)/src/simtoolsgui/simtoolsgui.h \
	$(SWARMHOME)/src/space/space.h $(top_srcdir)/protocol.el $(top_srcdir)/common.el
	SWARMDOCS=$(top_srcdir) SWARMDOCS_BUILD_AREA=$(top_builddir) $(EMACS) -batch -l $(top_srcdir)/protocol.el -f run-all $(SWARMHOME)
