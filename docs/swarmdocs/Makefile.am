SUBDIRS = figs refbook installbook overbook set m4 tex
EXTRA_DIST = Makefile.common Makefile.rules common.el history.el protocol.el common.dsl html.dsl print.dsl global.ent docbook.dcl

all-recursive-am: versions.ent

noinst_DATA = versions.ent

.PHONY: FORCE html dvi rtf fot ps htmldist dvidist rtfdist psdist alldist

alldist: htmldist dvidist rtfdist psdist

html dvi rtf fot ps htmldist dvidist rtfdist psdist:
	- $(MAKE) -C installbook $@
	- $(MAKE) -C overbook $@
	- $(MAKE) -C refbook $@
	- $(MAKE) -C set $@

versions.ent: FORCE
	((echo '<!ENTITY swarm-release-number "' ; head -1 $(swarm_srcdir)/VERSION ; echo '">') | tr -d '\n' ; echo) > versions.ent.new
	echo '<!ENTITY swarm-doc-release-number "@VERSION@">' >> versions.ent.new
	((echo '<!ENTITY pubdate "' ; date +'%d %B %Y' ; echo '">') | tr -d '\n' ; echo) >> versions.ent.new
	if test -f versions.ent && cmp versions.ent versions.ent.new; then \
	  $(RM) versions.ent.new; \
	else \
	  mv versions.ent.new versions.ent; \
	fi
