NAME = refbook
SUBDIRS = activity analysis collections defobj gui objectbase random simtools simtoolsgui space tech

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

GENERATED_SGML = $(PAGES) gridexamples.sgml refindex.sgml refbookrevhistory.sgml

noinst_DATA = $(GENERATED_SGML)

ENT = refbook.ent $(foreach module,$(SUBDIRS),$(module)/$(module).ent)

SGML = conventions-app.sgml gpl-app.sgml grid-app.sgml lgpl-app.sgml probes-app.sgml ref00.sgml ref00a.sgml refbook.sgml refcont.sgml refmeta.sgml

EXTRA_DIST = Makefile.rules refbook.ent $(SGML)

SGML_FILES = $(SGML) $(ENT) $(GENERATED_SGML)

EXAMPLES = GridTurtle.h GridTurtle.m Makefile Mousetrap.h Mousetrap.m \
        Mousetrap2.m grid.h grid.m grid0.m grid1a.m grid1b.m grid1c.m\
        grid2.m grid2b.m grid3.m grid3b.m grid4.m  grid4b.m grid5.m \
        grid6.m grid7.m grid8.m grid9.m mousetraps.m mousetraps2.m \
        strtest.m 

gridexamples.sgml: $(addprefix $(gridturtle_srcdir)/,$(EXAMPLES))
	- (for i in $(EXAMPLES); do echo "<EXAMPLE ID=\"SWARM.GRID.SGML.EXAMPLE.$$i\">"; echo "<TITLE>$$i</TITLE>"; echo "<PROGRAMLISTING>"; echo '<![ CDATA [' ; cat $(gridturtle_srcdir)/$$i ; echo ']]>'; echo "</PROGRAMLISTING>"; echo "</EXAMPLE>";  done) > $@

refindex.sgml $(PAGES): $(swarm_srcdir)/src/activity/activity.h \
	$(swarm_srcdir)/src/analysis/analysis.h \
	$(swarm_srcdir)/src/collections/collections.h \
	$(swarm_srcdir)/src/defobj/defobj.h \
	$(swarm_srcdir)/src/gui/gui.h \
	$(swarm_srcdir)/src/objectbase/objectbase.h \
	$(swarm_srcdir)/src/random/random.h \
	$(swarm_srcdir)/src/simtools/simtools.h \
	$(swarm_srcdir)/src/simtoolsgui/simtoolsgui.h \
	$(swarm_srcdir)/src/space/space.h $(top_srcdir)/protocol.el $(top_srcdir)/common.el
	SWARMSRCDIR=$(swarm_dossrcdir) SWARMDOCS=$(top_dossrcdir)/ SWARMDOCS_BUILD_AREA=$(top_builddir)/ $(EMACS) -batch -l $(top_dossrcdir)/protocol.el -f run-all $(swarm_dossrcdir)

# Local variables:
# mode: makefile
# end:
