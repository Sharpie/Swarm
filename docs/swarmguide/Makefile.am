SUBDIRS = figs
NAME = userbook
GENERATED_SGML = genindex.sgml
SGML = userbook.sgml usercont.sgml usermeta.sgml userbiblio.sgml user1.01.sgml user1.02.sgml user1.03.sgml user1.04.sgml user1.05.sgml user2.01.sgml user2.02.sgml user2.03.sgml user2.04.sgml user2.05.sgml user2.06.sgml user2.07.sgml user2.08.sgml user2.09.sgml user2.10.sgml user2.11.sgml user3.01.sgml user3.02.sgml user3.03.sgml user3.04.sgml user3.05.sgml user3.06.sgml user3.07.sgml

SGML_FILES = $(SGML) $(GENERATED_SGML)

include $(top_srcdir)/Makefile.rules
include $(top_srcdir)/Makefile.common

HTML = $(docbookstyle_srcdir)/html/docbook.dsl
PRINT = $(docbookstyle_srcdir)/print/docbook.dsl

## ensure citations are links
JADEFLAGS = -V biblio-citation-check 

%.html: %.sgml
	$(RUNJADE) $(CATOPTS) -t sgml -d $(HTML) `echo $< | sed 's,//\([A-Za-z]\),\1:,'`

## automatically generate index, if any non-generated SGML files change
genindex.sgml: $(SGML)
	perl $(docbookstyle_srcdir)/bin/collateindex.pl -N -o genindex.sgml
	$(RUNJADE) -V html-index $(CATOPTS) -t sgml -d $(HTML) `echo $< | sed 's,//\([A-Za-z]\),\1:,'`	
	perl $(docbookstyle_srcdir)/bin/collateindex.pl -g -o genindex.sgml HTML.index

# Local variables:
# mode: makefile
# end:
