XML_FILES = pubs.xml pubs-biblio.xml media.xml media-biblio.xml

EXTRA_DIST = $(XML_FILES)

GENERAL = $(XML_FILES) $(top_srcdir)/$(HTML) $(top_srcdir)/$(PRINT)

.PHONY: all

all: pubs-print.ps media-print.ps

include $(top_srcdir)/Makefile.rules

%-print.xml: %-biblio.xml
	@echo "<!DOCTYPE article PUBLIC \"-//Norman Walsh//DTD DocBook XML V3.1.7//EN\"" > $@
	@echo  "\"$(top_srcdir)/dtd/docbookx.dtd\" [" >> $@
	@echo "<!ENTITY $*-biblio SYSTEM \"$(top_srcdir)/biblio/$*-biblio.xml\">]>" >> $@

	@echo "<article><title></title><para></para>&$*-biblio;</article>" >> $@

%-print.tex: %-print.xml
	$(RUNJADE) $(CATOPTS) -V tex-backend -t tex -d $(srcdir)/print-biblio.dsl -o $@ $(top_srcdir)/xml.dcl $<

CLEANFILES = $(addprefix pubs-print, .xml .tex .aux .dvi .log .ps) $(addprefix media-print, .xml .tex .aux .dvi .log .ps)

generateddir = $(prefix)/biblio
generated_DATA = pubs-print.ps pubs-biblio.xml media-print.ps media-biblio.xml