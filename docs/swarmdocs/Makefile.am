SUBDIRS = src installing overview set

all-recursive-am: versions.ent

noinst_DATA = versions.ent

versions.ent:
	((echo '<!ENTITY swarm-release-number "' ; tail -1 $(swarm_srcdir)/VERSION ; echo '">') | tr -d '\n' ; echo) > versions.ent
	echo '<!ENTITY swarm-doc-release-number "@VERSION@">' >> versions.ent
	((echo '<!ENTITY pubdate "' ; date +'%d %B %Y' ; echo '">') | tr -d '\n' ; echo) >> versions.ent
