noinst_DATA = techrevhistory.sgml

swarm_Changelog =

include $(top_srcdir)/Makefile.rules

techrevhistory.sgml: ChangeLog $(top_srcdir)/history.el $(top_srcdir)/common.el
	SWARMDOCS=`cd $(top_srcdir); pwd`/ SWARMDOCS_BUILD_AREA=$(top_builddir)/ $(EMACS) -batch -l $(top_srcdir)/history.el -f process-changelog tech
