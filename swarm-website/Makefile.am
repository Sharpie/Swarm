SUBDIRS = graphics biblio archive

AUTOMAKE_OPTIONS = foreign
NAME = website

## SDG organizational pages 
SDG_XML_ENTITIES=sdg sdg-charter sdg-strength sdg-membership sdg-membership-ind sdg-membership-group sdg-membership-other sdg-board sdg-members sdg-sigs sdg-report-2000

## Introductory material pages
INTRO_XML_ENTITIES=intro intro-people intro-tutorial intro-papers 

## Examples pages
EXAMPLE_XML_ENTITIES=examples examples-heatbugs examples-mousetrap 

## Release pages 
RELEASE_XML_ENTITIES=release release-swarm release-swarm-snapshot release-apps release-docs release-platforms release-license release-mirrors 

## Community pages 
COMMUNITY_XML_ENTITIES=community community-yp community-programmers-yp community-projects-yp community-swarmfest community-swarmfest-program community-swarmfest-registration community-swarmfest-hotels community-swarmfest-driving community-past-swarmfests community-mailing-lists community-support community-contrib community-links community-publications _community-swarmfest-program community-swarmfest-2001

## Resource pages
RESOURCE_XML_ENTITIES=resources resources-objc resources-other

## Other pages
OTHER_XML_ENTITIES=about contact

## Webcam pages
WEBCAMS_XML_ENTITIES=webcams

XML_ENTITIES=$(SDG_XML_ENTITIES) $(INTRO_XML_ENTITIES) $(EXAMPLE_XML_ENTITIES) $(RELEASE_XML_ENTITIES) $(COMMUNITY_XML_ENTITIES) $(RESOURCE_XML_ENTITIES) $(OTHER_XML_ENTITIES) $(WEBCAMS_XML_ENTITIES)

XML_FILES=website.xml $(addsuffix .xml, $(XML_ENTITIES)) 

website.ent: $(XML_FILES) 
	echo "<!--system ids for XML files -->" > website.ent
	for i in $(XML_ENTITIES); do \
	echo "<!ENTITY $$i SYSTEM \"$(top_srcdir)/$$i.xml\">" >> website.ent; \
	done

HTML = org.dsl
PRINT = print.dsl
FIGS = $(top_builddir)/graphics/sdg.jpeg $(top_builddir)/graphics/sdg-small.jpeg

$(FIGS):
	$(MAKE) -C $(top_builddir)/graphics 

EXTRA_DIST = $(XML_FILES) $(HTML) xml.dcl org.xsl

GENERAL = $(XML_FILES) $(top_srcdir)/$(HTML) $(top_srcdir)/$(PRINT) $(FIGS) website.ent

.PHONY: xsl dsssl

index.html: xsl

xsl: $(GENERAL) org.xsl
#	$(XMLLINT) $(CATOPTS) -s $(top_srcdir)/xml.dcl $(top_srcdir)/website.xml
	SGML_CATALOG_FILES=catalog $(XSLTPROC) --catalogs $(top_srcdir)/org.xsl $(top_srcdir)/website.xml
	for i in $(SUBDIRS); do \
	$(MAKE) -C $$i; \
	done

dsssl: $(GENERAL)
	$(RUNJADE) $(CATOPTS) -t sgml -d $(top_srcdir)/$(HTML) $(top_srcdir)/xml.dcl $<
	for i in $(SUBDIRS); do \
	$(MAKE) -C $$i; \
	done

include $(top_srcdir)/Makefile.rules

swarmfest2001.tex: swarmfest2001.xml swarmfest2001.dsl _community-swarmfest-program.xml
	$(RUNJADE) $(CATOPTS) -V tex-backend -t tex -d $(srcdir)/swarmfest2001.dsl -o $@ $(top_srcdir)/xml.dcl $<

ALLHTML = $(wildcard *.html)

CLEANFILES = $(ALLHTML)

htmldir = $(prefix)
html_DATA = $(ALLHTML) swarm.css overlib.js
