SUBDIRS = figs src install over set
EXTRA_DIST = Makefile.common Makefile.rules common.el history.el protocol.el common.dsl html.dsl print.dsl global.ent docbook.dcl

all-recursive-am: versions.ent

noinst_DATA = versions.ent

.PHONY: FORCE

html dvi rtf fot ps: all
	- make -C install $@
	- make -C over $@
	- make -C src $@
	- make -C set $@

versions.ent: FORCE
	((echo '<!ENTITY swarm-release-number "' ; tail -1 $(swarm_srcdir)/VERSION ; echo '">') | tr -d '\n' ; echo) > versions.ent
	echo '<!ENTITY swarm-doc-release-number "@VERSION@">' >> versions.ent
	((echo '<!ENTITY pubdate "' ; date +'%d %B %Y' ; echo '">') | tr -d '\n' ; echo) >> versions.ent
